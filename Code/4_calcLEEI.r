
library(tidyverse)
library(lubridate)
library(MuMIn)
library(gtsummary)
library(gt)
library(patchwork)
library(GGally)

dat<-readRDS("Outputs/RHQdf_clean.RDS")

prescreenDat<-readRDS("Data/summarydf.RDS")

joined <- left_join(dat, prescreenDat, by="email" )

#write.csv(joined, "Outputs/joinPreandPostscreenDat.csv")


# Load and parse dataset

df <- joined %>%
  mutate(
    #prescreendate_parsed = parse_date_time(prescreendate, orders = c("mdy", "dmy", "ymd")),
    prescreendate = ymd(prescreendate),
    dob_parsed = ymd(dob_parsed.x)
  )

# Step 1: Menopause age estimation logic
# Detect surgical menopause
df <- joined %>%
  mutate(
    hyst_ooph = tolower(HystCats) == "oopherectomy",
    both_ovaries_removed = ovary_removal == "Yes, both were taken out",
    surgical_meno = (hyst_ooph | both_ovaries_removed) & !is.na(ovary_removal_age)
  )

# Assign menopause age and flag
df <- df %>%
  mutate(
    menopause_age = case_when(
      surgical_meno ~ ovary_removal_age,
      menopause_symptoms_ever == "yes" & !is.na(menopause_symptoms_start_age) ~ menopause_symptoms_start_age,
      menopause_symptoms_ever == "no" & period_last_year == "no" ~ 50,
      TRUE ~ NA_real_
    ),
    flag_estimatedMenoage = case_when(
      surgical_meno ~ "surgical",
      menopause_symptoms_ever == "yes" & !is.na(menopause_symptoms_start_age) ~ "reported",
      menopause_symptoms_ever == "no" & period_last_year == "no" ~ "estimated_no_symptoms",
      TRUE ~ "missing"
    )
  )


df <- df %>%
  mutate(
    menopause_years = menopause_age,
    meno_date = dob_parsed + years(round(menopause_years)),
    menarche_date = dob_parsed + years(round(first_period)),
    last_reproductive_date = case_when(
      !is.na(meno_date) ~ meno_date,
      TRUE ~ prescreendate_parsed
    ),
    reproductive_days = as.numeric(last_reproductive_date - menarche_date, units = "days"),
    total_life_days = as.numeric(prescreendate_parsed - dob_parsed, units = "days"),
    ReproductiveRatio = reproductive_days / total_life_days
  )

#pregnancy and breastfeeding
df <- df %>%
  mutate(
    preg_years = ifelse(!is.na(pregnancy_count), pregnancy_count, 0) * 9/12,
    bf_years = ifelse(!is.na(breastfeeding_total_months), breastfeeding_total_months, 0) / 12,
    amen_years = ifelse(!is.na(amenorrhea_duration), amenorrhea_duration, 0)
  )

  df %>% select(pregnancy_count, breastfeeding_total_months, amenorrhea_duration) %>%summary()

# Step 4: Calculate EES
df <- df %>%
  mutate(
    EES_raw = (ReproductiveRatio*10) - (preg_years + bf_years + amen_years),
    EES = ifelse(EES_raw < 0, 0, EES_raw)
  )

# Step 4 alternative calculation of EES
df <- df %>%
  mutate(
    EES_raw = (reproductive_days - (preg_years + bf_years + amen_years))/total_life_days*100,
    EES = ifelse(EES_raw < 0, 0, EES_raw)
  )

# Step 4: Partial loss adjustment (e.g., one ovary removed)
df <- df %>%
  mutate(
    partial_loss_flag = (
      ovary_removal == "Yes, one was taken out" |
      (HystCats == "FullHysterectomy" & ovary_removal != "Yes, both were taken out")
    ),
    EES_adjusted = ifelse(partial_loss_flag & !is.na(EES), EES * 0.95, EES)
  )

# Step 5: Calculate Exogenous Estrogen Score (XES)
# Helper to detect hormonal IUD
df <- df %>%
  mutate(
    used_hormonal_iud = ifelse(
      bc_iud_ever == 1 & grepl("hormonal|mirena", tolower(iud_type)),
      TRUE, FALSE
    ))

# Core hormonal ever-use detection
df <- df %>%
  mutate(
    XES = 0,
    XES = XES + ifelse(bc_pill_ever == 1, 5, 0),
    XES = XES + ifelse(bc_patch_ever == 1, 2, 0),
    XES = XES + ifelse(bc_ring_ever == 1, 2, 0),
    XES = XES + ifelse(bc_implant_ever == 1, 3, 0),
    XES = XES + ifelse(bc_injection_ever == 1, 3, 0),
    XES = XES + ifelse(used_hormonal_iud, 2, 0),
    #XES = XES + ifelse(HT == 1, 3, 0),
    #XES = XES + ifelse(is.na(bc_pill_ever) & bc_type == "pill", 5, 0)
  )


df <- df %>%
  mutate(
    LEEI_adjusted = EES_adjusted + XES,
    LEEI = EES_raw + XES
  ) 


################################################################################
# filtering
################################################################################


track_filters <- function(data, ...) {
  conditions <- enquos(...)
  names(conditions) <- sapply(conditions, rlang::as_label)

  out <- tibble(Step = "Original", RowsRemaining = nrow(data), RowsRemoved = 0)
  current_data <- data

  for (i in seq_along(conditions)) {
    step_label <- names(conditions)[i]
    before <- nrow(current_data)
    current_data <- filter(current_data, !!conditions[[i]])
    after <- nrow(current_data)
    out <- bind_rows(out, tibble(
      Step = step_label,
      RowsRemaining = after,
      RowsRemoved = before - after
    ))
  }

  list(filtered_data = current_data, tracking_table = out)
}

result <- track_filters(
  df,
  birth_sex == "female",
  country_US == "USA",
  !is.na(birth_control),
  !is.na(pregnant),
  pregnant != "Yes",
  !is.na(totalPqb),
  EatingDisorder == 0,
  SubstanceUseDisorder == 0,
  seizure == 0,
  Endocrine_or_thyroid_Disease == 0,
  psychosisDXyesNo == 0,
  MEN_IorII == 0
)

# Extract both elements
d_filtered <- result$filtered_data #923
trackingExclusions <- result$tracking_table

#write.csv(trackingExclusions, "Outputs/trackingExclusions.csv")

df_leei <- df %>% 
    filter(!is.na(LEEI)) %>%
    filter(reproductive_days>365*15) %>%
    filter(!psychosisDXyesNo == 1)
     # remove people who didn't start periods by 20 (15 years after youngest person in study) as errors


################################################################################
# sample overview
################################################################################

sampleCharTab <- df_leei %>%
  select(reproductive_days,
         preg_years, 
         bf_years,
         amen_years,
         total_life_days ) %>%
  tbl_summary(missing = "ifany") %>%
  bold_labels()

gtsave(as_gt(sampleCharTab), "Outputs/sampleCharTab_paper.pdf")


################################################################################
#corrs
################################################################################

corrs<- df_leei %>%
select(LEEI, EES,
        reproductive_days,
         preg_years, 
         bf_years,
         amen_years,
         total_life_days)

ggpairs(corrs)

################################################################################
# plots
################################################################################
plot(density(df_leei$LEEI))

LEEI <- df_leei %>%
ggplot(aes(x=LEEI, y=totalPqb)) +
geom_point(aes(color=age), alpha=0.75, size=3) +
geom_smooth(method="lm")+
viridis::scale_color_viridis() +
#facet_wrap(~moodDXyesNo) +
theme_minimal()

EES <- df_leei %>%
ggplot(aes(x=EES, y=totalPqb)) +
geom_point(aes(color=age), alpha=0.75, size=3) +
geom_smooth(method="lm")+
viridis::scale_color_viridis() +
#facet_wrap(~moodDXyesNo) +
theme_minimal()

XES <- df_leei %>%
ggplot(aes(x=XES, y=totalPqb)) +
geom_point(aes(color=age), alpha=0.75, size=3) +
geom_smooth(method="lm")+
viridis::scale_color_viridis() +
#facet_wrap(~moodDXyesNo) +
theme_minimal()

LEEI | (XES / EES)

df_leei %>%
ggplot(aes(x=LEEI, y=totalPqb)) +
geom_hex() +
geom_smooth(method="lm")+
viridis::scale_fill_viridis() +
theme_minimal()

df_leei %>%
ggplot(aes(x=LEEI, y=age)) +
geom_point(alpha=0.75) +
geom_smooth(method="lm")+
theme_minimal()

################################################################################
#mods
################################################################################


m0<-lm(totalPqb~age, df_leei) 

m05<-lm(totalPqb~moodDXyesNo, df_leei) 
m1<-lm(totalPqb~LEEI, df_leei) 
m2<-lm(totalPqb~LEEI + age, df_leei) 
m3<-lm(totalPqb~LEEI * age, df_leei) 
m4<-lm(totalPqb~EES + XES, df_leei) 
m5<-lm(totalPqb~EES, df_leei) 
m6<-lm(totalPqb~ XES, df_leei) 
m7<-lm(totalPqb~ XES*EES, df_leei) 
m8<-lm(totalPqb~ reproductive_days, df_leei) 
m9<-lm(totalPqb~LEEI + moodDXyesNo, df_leei) 
m10<-lm(totalPqb~EES + moodDXyesNo, df_leei) 



model.sel(m0, m05, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10)
summary(m9)











# Step 5: Calculate XES using assumptions:
# +5 years if ever used pill, +2 years for patch, +2 years for ring, +5 years for hormone therapy
df <- df %>%
  mutate(
    XES = 5 * as.numeric(bc_pill_ever == 1) +
          2 * as.numeric(bc_patch_ever == 1) +
          2 * as.numeric(bc_ring_ever == 1) +
          5 * as.numeric(hormone_med_ever == "yes")
  )

#variales from RHQ: current
  #birthcontrol_current_type
df %>% mutate(hormone_med_ever=as.factor(hormone_med_ever)) %>% group_by(df$hormone_med_ever) %>% summarise(n())

birthcontrol_current_type - not useable
hormone_med_current - too many NAs, not useabel (because not required)
hormone_med_current_type - free text, and not required - the prescreen data probs better

#variales from RHQ: ever
hormone_med_ever - cant use (not mandatory, too many NAs)
hormone_med_ever_details - cant use (not mandatory, too many NAs)
bc_pill_ever - can use
bc_patch_ever - can use
bc_ring_ever - can use
bc_implant_ever - can use
bc_implant_ever - can use
bc_iud_ever - can use, but could be non hormonal - could pair with iud_type to discern if hormonal or not

  #variables from prescreen: all descrbiing current
HT
hormone_therapy_category
bc_type
iud_type

df %>% mutate(hormone_therapy_category=as.factor(hormone_therapy_category)) %>% group_by(df$hormone_therapy_category) %>% summarise(n())

df %>% mutate(HT=as.factor(HT)) %>% group_by(df$HT) %>% summarise(n())

df %>% mutate(HT=as.factor(bc_type)) %>% group_by(df$bc_type) %>% summarise(n())

df %>% mutate(HT=as.factor(bc_type)) %>% group_by(df$iud_type) %>% summarise(n())



# Step 6: Total LEEI
df <- df %>%
  mutate(
    LEEI = EES + XES
  )



#### suggestsed additional partial ovary removal :

  mutate(
    partial_loss_flag = (
      ovary_removal == "Yes, one was taken out" |
      (HystCats == "FullHysterectomy" & ovary_removal != "Yes, both were taken out")
    ),
    EES_adjusted = ifelse(partial_loss_flag & !is.na(EES), EES * 0.95, EES)
  )





x <- df %>% filter(!is.na(ReproductiveRatio)) %>% filter(!ReproductiveRatio < 0)


table(x$ReproductiveRatio) 







df <- joined %>%
  mutate(
    prescreendate = ymd(prescreendate),
    dob_parsed = ymd(dob_parsed)
  )

# 1. Estimate menopause date in date form
df <- df %>%
  mutate(
    menopause_years = case_when(
      flag_estimatedMenoage == "surgical" ~ ovary_removal_age,
      flag_estimatedMenoage == "reported" ~ menopause_symptoms_start_age,
      flag_estimatedMenoage == "estimated_no_symptoms" ~ 50,
      TRUE ~ NA_real_
    ),
    meno_date = dob_parsed + years(round(menopause_years))
  )

# 2. Estimate menarche date
df <- df %>%
  mutate(
    menarche_date = dob_parsed + years(round(first_period))
  )

# 3. Define last reproductive date
df <- df %>%
  mutate(
    last_reproductive_date = case_when(
      !is.na(meno_date) ~ meno_date,
      TRUE ~ prescreendate
    )
  )

# 4. Calculate ReproductiveRatio
df <- df %>%
  mutate(
    reproductive_days = as.numeric(last_reproductive_date - menarche_date, units = "days"),
    total_life_days = as.numeric(prescreendate - dob_parsed, units = "days"),
    ReproductiveRatio = reproductive_days / total_life_days
  )

# 5. Optional sanity checks
summary(df$ReproductiveRatio)
table(is.na(df$ReproductiveRatio))












# Step 1: Estimate menopause age

LEEIcalcdf <- df %>%
  mutate(
    hyst_ooph = tolower(HystCats) == "oopherectomy",
    both_ovaries_removed = ovary_removal == "Yes, both were taken out",
    surgical_meno = (hyst_ooph | both_ovaries_removed) & !is.na(ovary_removal_age)
  )

# Compute menopause_age with full fallback logic
LEEIcalcdf <- df %>%
  mutate(
    menopause_age = case_when(
      surgical_meno ~ ovary_removal_age,  # use surgical age first
      menopause_symptoms_ever == "yes" & !is.na(menopause_symptoms_start_age) ~ menopause_symptoms_start_age,
      menopause_symptoms_ever == "no" & period_last_year == "no" ~ 50,
      TRUE ~ NA_real_
    ),
    flag_estimatedMenoage = case_when(
      surgical_meno ~ "surgical",
      menopause_symptoms_ever == "yes" & !is.na(menopause_symptoms_start_age) ~ "reported",
      menopause_symptoms_ever == "no" & period_last_year == "no" ~ "estimated_no_symptoms",
      TRUE ~ "missing"
    )
  )

# Review results
table(df$flag_estimatedMenoage)


#### suggestsed additional partial ovary removal :

  mutate(
    partial_loss_flag = (
      ovary_removal == "Yes, one was taken out" |
      (HystCats == "FullHysterectomy" & ovary_removal != "Yes, both were taken out")
    ),
    EES_adjusted = ifelse(partial_loss_flag & !is.na(EES), EES * 0.95, EES)
  )


# mutate last reproductive date - today or date of menopause
# equation- last rpri date - menarche / todaydate - birthdate 








df <- df %>%
  mutate(
    menopause_age = ifelse(!is.na(menopause_symptoms_end_age), 
                           menopause_symptoms_end_age,
                           ifelse(period_last_year == "no", 50, NA))
  )

# Step 2: Calculate reproductive span
df <- df %>%
  mutate(
    repro_years = menopause_age - first_period
  )

# Step 3: Adjustments
df <- df %>%
  mutate(
    preg_years = ifelse(!is.na(pregnancy_count), pregnancy_count, 0) * 1,
    bf_years = ifelse(!is.na(breastfeeding_total_months), breastfeeding_total_months, 0) / 12,
    amen_years = ifelse(!is.na(amenorrhea_duration), amenorrhea_duration, 0)
  )

# Step 4: Calculate EES
df <- df %>%
  mutate(
    EES_raw = repro_years - (preg_years + bf_years + amen_years),
    EES = ifelse(EES_raw < 0, 0, EES_raw)
  )

# Step 5: Calculate XES using assumptions:
# +5 years if ever used pill, +2 years for patch, +2 years for ring, +5 years for hormone therapy
df <- df %>%
  mutate(
    XES = 5 * as.numeric(bc_pill_ever == 1) +
          2 * as.numeric(bc_patch_ever == 1) +
          2 * as.numeric(bc_ring_ever == 1) +
          5 * as.numeric(hormone_med_ever == "yes")
  )

# Step 6: Total LEEI
df <- df %>%
  mutate(
    LEEI = EES + XES
  )

# View results (first few rows)
df %>% 
  select(first_period, menopause_age, EES, XES, LEEI) %>%
  head()

LEEI <- df %>% filter(!is.na(LEEI))

saveRDS(LEEI, "Outputs/LEEIdat.RDS")
