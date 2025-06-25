# === Reproductive Health Data Cleaning Script === #
library(tidyverse)

df <- read_csv("Outputs/joinedCorrected_RHQ_dfELIGIBLEONLY.csv")

# Convert and clean variables

# Batch 1
df_clean <- df %>%
  mutate(
    first_period = as.numeric(str_extract(first_period, "\\d+(\\.\\d+)?")),
    cycle_regular_lifetime = recode_factor(as.character(cycle_regular_lifetime), `0` = "irregular", `1` = "regular", `2` = "both"),
    currently_menstruating = recode_factor(as.character(currently_menstruating), `0` = "no", `1` = "yes", `2` = NA_character_),
    period_last_month = recode_factor(as.character(period_last_month), `0` = "no", `1` = "yes", .default = NA_character_),
    period_last_year = recode_factor(as.character(period_last_year), `0` = "no", `1` = "yes", .default = NA_character_)
  ) %>%

  # Batch 2
  mutate(
    cycle_regular_now = recode_factor(as.character(cycle_regular_now), `0` = "irregular", `1` = "regular", `2` = "both"),
    irregular_duration = as.numeric(irregular_duration),
    irregular_bleed_length_change = recode_factor(as.character(irregular_bleed_length_change), `0` = "no", `1` = "yes", `2` = NA_character_),
    amenorrhea_any = recode_factor(as.character(amenorrhea_any), `0` = "no", `1` = "yes", `2` = NA_character_),
    amenorrhea_duration = as.numeric(amenorrhea_duration)
  ) %>%

  # Batch 3
  mutate(
    menopause_symptoms_ever = recode_factor(as.character(menopause_symptoms_ever), `0` = "no", `1` = "yes", `2` = NA_character_),
    menopause_symptoms_start_age = as.numeric(menopause_symptoms_start_age),
    menopause_symptoms_end_age = as.numeric(menopause_symptoms_end_age),
    ever_pregnant = recode_factor(as.character(ever_pregnant), `0` = "no", `1` = "yes", `2` = NA_character_),
    pregnancy_count = as.numeric(pregnancy_count),
    pregnancy_6months_ever = recode_factor(as.character(pregnancy_6months_ever), `0` = "no", `1` = "yes", `2` = NA_character_),
    live_birth_count = as.numeric(live_birth_count)
  ) %>%

  # Batch 4
  mutate(
    prenatal_depression = recode_factor(as.character(prenatal_depression), `0` = "no", `1` = "yes", `2` = NA_character_),
    postpartum_depression = recode_factor(as.character(postpartum_depression), `0` = "no", `1` = "yes", `2` = NA_character_),
    postpartum_psychosis = recode_factor(as.character(postpartum_psychosis), `0` = "no", `1` = "yes", `2` = NA_character_),
    breastfed_any = recode_factor(as.character(breastfed_any), `0` = "no", `1` = "yes", `2` = NA_character_),
    breastfed_child_count = as.numeric(breastfed_child_count),
    breastfeeding_total_months = as.numeric(breastfeeding_total_months)
  ) %>%

  # Batch 5
  mutate(
    ovary_removal = recode_factor(as.character(ovary_removal), `0` = "no", `1` = "yes", `2` = NA_character_),
    ovary_removal_age = as.numeric(ovary_removal_age),
    tubal_ligation = recode_factor(as.character(tubal_ligation), `0` = "no", `1` = "yes", `2` = NA_character_),
    tubal_ligation_age = as.numeric(tubal_ligation_age),
    breast_cancer_dx = recode_factor(as.character(breast_cancer_dx), `0` = "no", `1` = "yes", `2` = NA_character_)
  ) %>%
  
  # Batch 6 - Birth Control Methods and Pregnancy Detail
mutate(
  pregnancy_6months_count = as.numeric(pregnancy_6months_count),
  bc_pill_ever = as.numeric(bc_pill_ever),
  bc_patch_ever = as.numeric(bc_patch_ever),
  bc_ring_ever = as.numeric(bc_ring_ever),
  bc_implant_ever = as.numeric(bc_implant_ever)
) %>%

mutate(
    # Final batch
    ovarian_uterine_cancer_dx = recode_factor(as.character(ovarian_uterine_cancer_dx), `0` = "no", `1` = "yes", `2` = NA_character_),
    birthcontrol_current = recode_factor(as.character(birthcontrol_current), `0` = "no", `1` = "yes", `2` = NA_character_),
    birthcontrol_ever = recode_factor(as.character(birthcontrol_ever), `0` = "no", `1` = "yes", `2` = NA_character_),
    birthcontrol_current_type = recode_factor(as.character(birthcontrol_current_type), `0` = "no", `1` = "yes", `2` = NA_character_),
    hormone_med_current = recode_factor(as.character(hormone_med_current), `0` = "no", `1` = "yes", `2` = NA_character_)
  ) %>%

mutate(
    bc_injection_ever = as.numeric(bc_injection_ever),
    bc_iud_ever = as.numeric(bc_iud_ever),
    bc_other_ever = as.numeric(bc_other_ever),
    hormone_med_current_type = as.character(hormone_med_current_type),
    hormone_med_ever = recode_factor(as.character(hormone_med_ever), `0` = "no", `1` = "yes", `2` = NA_character_),
    hormone_med_ever_details = as.character(hormone_med_ever_details),
    DataCollection = as.character(DataCollection)
  )




write_csv(df_clean, "Outputs/RHQdf_clean.csv")
saveRDS(df_clean, "Outputs/RHQdf_clean.RDS")


# === Define valid subsets for contingent missing logic ===
  period_last_month_valid <- df_clean %>% filter(period_last_month != "not_applicable")
  period_last_year_valid <- df_clean %>% filter(period_last_year != "not_applicable")
  amenorrhea_valid <- df_clean %>% filter(amenorrhea_any == "yes")
  menopause_valid <- df_clean %>% filter(menopause_symptoms_ever == "yes")
  pregnancy_valid <- df_clean %>% filter(ever_pregnant == "yes")
  breastfed_valid <- df_clean %>% filter(breastfed_any == "yes")
  
  # === Summary Table === #
 # === Define valid subsets for contingent missing logic ===
period_last_month_valid <- df_clean %>% filter(period_last_month != "not_applicable")
period_last_year_valid <- df_clean %>% filter(period_last_year != "not_applicable")
amenorrhea_valid <- df_clean %>% filter(amenorrhea_any == "yes")
menopause_valid <- df_clean %>% filter(menopause_symptoms_ever == "yes")
pregnancy_valid <- df_clean %>% filter(ever_pregnant == "yes")
breastfed_valid <- df_clean %>% filter(breastfed_any == "yes")
# === Define valid subsets for contingent missing logic ===
period_last_month_valid <- df_clean %>% filter(period_last_month != "not_applicable")
period_last_year_valid <- df_clean %>% filter(period_last_year != "not_applicable")
amenorrhea_valid <- df_clean %>% filter(amenorrhea_any == "yes")
menopause_valid <- df_clean %>% filter(menopause_symptoms_ever == "yes")
pregnancy_valid <- df_clean %>% filter(ever_pregnant == "yes")
breastfed_valid <- df_clean %>% filter(breastfed_any == "yes")

# === Summary Table === #
summary_df <- tibble(
  Variable = c(
    "first_period", "cycle_regular_lifetime", "currently_menstruating",
    "period_last_month", "period_last_year", "cycle_regular_now", "irregular_duration",
    "irregular_bleed_length_change", "amenorrhea_any", "amenorrhea_duration",
    "menopause_symptoms_ever", "menopause_symptoms_start_age", "menopause_symptoms_end_age",
    "ever_pregnant", "pregnancy_count", "pregnancy_6months_ever", "live_birth_count",
    "prenatal_depression", "postpartum_depression", "postpartum_psychosis",
    "breastfed_any", "breastfed_child_count", "breastfeeding_total_months",
    "ovary_removal", "ovary_removal_age", "tubal_ligation", "tubal_ligation_age",
    "breast_cancer_dx", "ovarian_uterine_cancer_dx", "birthcontrol_current",
    "birthcontrol_ever", "birthcontrol_current_type", "hormone_med_current",
    "pregnancy_6months_count", "bc_pill_ever", "bc_patch_ever", "bc_ring_ever", "bc_implant_ever",
    "bc_injection_ever", "bc_iud_ever", "bc_other_ever",
    "hormone_med_current_type", "hormone_med_ever", "hormone_med_ever_details",
    "DataCollection", "email", "prescreendate"
  ),

  `Data Type` = sapply(df_clean %>% select(all_of(Variable)), \(x) class(x)[1]),

  `Cleaning Actions` = c(
    "Converted to numeric",
    "Mapped 0/1/2 to irregular/regular/both",
    "Mapped 0/1/2 to no/yes/NA (treated 2 as missing)",
    "Mapped 0/1 to no/yes; not_applicable if not menstruating",
    "Mapped 0/1 to no/yes; not_applicable if not menstruating",
    "Mapped 0/1/2 to irregular/regular/both",
    "Converted to numeric",
    "Mapped 0/1/2 to no/yes/don’t know",
    "Mapped 0/1/2 to no/yes/don’t know",
    "Converted to numeric (if amenorrhea_any == yes)",
    "Mapped 0/1/2 to no/yes/don’t know",
    "Converted to numeric (if menopause_symptoms_ever == yes)",
    "Converted to numeric (if menopause_symptoms_ever == yes)",
    "Mapped 0/1/2 to no/yes/don’t know",
    "Converted to numeric (if ever_pregnant == yes)",
    "Mapped 0/1/2 to no/yes/don’t know",
    "Converted to numeric (if ever_pregnant == yes)",
    "Mapped 0/1/2 to no/yes/don’t know",
    "Mapped 0/1/2 to no/yes/don’t know",
    "Mapped 0/1/2 to no/yes/don’t know",
    "Mapped 0/1/2 to no/yes/don’t know",
    "Converted to numeric (if breastfed_any == yes)",
    "Converted to numeric (if breastfed_any == yes)",
    "Mapped 0/1/2 to no/yes/don’t know",
    "Converted to numeric (if ovary_removal == yes)",
    "Mapped 0/1/2 to no/yes/don’t know",
    "Converted to numeric (if tubal_ligation == yes)",
    "Mapped 0/1/2 to no/yes/don’t know",
    "Mapped 0/1/2 to no/yes/don’t know",
    "Mapped 0/1/2 to no/yes/don’t know",
    "Mapped 0/1/2 to no/yes/don’t know",
    "Mapped 0/1/2 to no/yes/don’t know",
    "Mapped 0/1/2 to no/yes/don’t know",
    "Converted to numeric (if ever pregnant)",
    "Converted to numeric (binary 0/1)",
    "Converted to numeric (binary 0/1)",
    "Converted to numeric (binary 0/1)",
    "Converted to numeric (binary 0/1)",
        "Converted to numeric (binary 0/1)",
    "Converted to numeric (binary 0/1)",
    "Converted to numeric (binary 0/1)",
    "Retained as character text",
    "Mapped 0/1/2 to no/yes/don’t know",
    "Retained as character text",
    "Retained as character text",
    "Not cleaned – ID variable",
    "Not cleaned – date variable"

  ),

  `Missing (n NA)` = c(
    sum(is.na(df_clean$first_period)),
    sum(is.na(df_clean$cycle_regular_lifetime)),
    sum(is.na(df_clean$currently_menstruating)),
    sum(is.na(period_last_month_valid$period_last_month)),
    sum(is.na(period_last_year_valid$period_last_year)),
    sum(is.na(df_clean$cycle_regular_now)),
    sum(is.na(df_clean$irregular_duration)),
    sum(is.na(df_clean$irregular_bleed_length_change)),
    sum(is.na(df_clean$amenorrhea_any)),
    sum(is.na(amenorrhea_valid$amenorrhea_duration)),
    sum(is.na(df_clean$menopause_symptoms_ever)),
    sum(is.na(menopause_valid$menopause_symptoms_start_age)),
    sum(is.na(menopause_valid$menopause_symptoms_end_age)),
    sum(is.na(df_clean$ever_pregnant)),
    sum(is.na(pregnancy_valid$pregnancy_count)),
    sum(is.na(df_clean$pregnancy_6months_ever)),
    sum(is.na(pregnancy_valid$live_birth_count)),
    sum(is.na(df_clean$prenatal_depression)),
    sum(is.na(df_clean$postpartum_depression)),
    sum(is.na(df_clean$postpartum_psychosis)),
    sum(is.na(df_clean$breastfed_any)),
    sum(is.na(breastfed_valid$breastfed_child_count)),
    sum(is.na(breastfed_valid$breastfeeding_total_months)),
    sum(is.na(df_clean$ovary_removal)),
    sum(is.na(df_clean$ovary_removal_age)),
    sum(is.na(df_clean$tubal_ligation)),
    sum(is.na(df_clean$tubal_ligation_age)),
    sum(is.na(df_clean$breast_cancer_dx)),
    sum(is.na(df_clean$ovarian_uterine_cancer_dx)),
    sum(is.na(df_clean$birthcontrol_current)),
    sum(is.na(df_clean$birthcontrol_ever)),
    sum(is.na(df_clean$birthcontrol_current_type)),
    sum(is.na(df_clean$hormone_med_current)),
    sum(is.na(pregnancy_valid$pregnancy_6months_count)),
    sum(is.na(df_clean$bc_pill_ever)),
    sum(is.na(df_clean$bc_patch_ever)),
    sum(is.na(df_clean$bc_ring_ever)),
    sum(is.na(df_clean$bc_implant_ever)),
    sum(is.na(df_clean$bc_injection_ever)),
    sum(is.na(df_clean$bc_iud_ever)),
    sum(is.na(df_clean$bc_other_ever)),
    sum(is.na(df_clean$hormone_med_current_type)),
    sum(is.na(df_clean$hormone_med_ever)),
    sum(is.na(df_clean$hormone_med_ever_details)),
    sum(is.na(df_clean$DataCollection)),
    sum(is.na(df_clean$email)),
    sum(is.na(df_clean$prescreendate))
  ),

  `Missing %` = round(c(
    mean(is.na(df_clean$first_period)),
    mean(is.na(df_clean$cycle_regular_lifetime)),
    mean(is.na(df_clean$currently_menstruating)),
    mean(is.na(period_last_month_valid$period_last_month)),
    mean(is.na(period_last_year_valid$period_last_year)),
    mean(is.na(df_clean$cycle_regular_now)),
    mean(is.na(df_clean$irregular_duration)),
    mean(is.na(df_clean$irregular_bleed_length_change)),
    mean(is.na(df_clean$amenorrhea_any)),
    mean(is.na(amenorrhea_valid$amenorrhea_duration)),
    mean(is.na(df_clean$menopause_symptoms_ever)),
    mean(is.na(menopause_valid$menopause_symptoms_start_age)),
    mean(is.na(menopause_valid$menopause_symptoms_end_age)),
    mean(is.na(df_clean$ever_pregnant)),
    mean(is.na(pregnancy_valid$pregnancy_count)),
    mean(is.na(df_clean$pregnancy_6months_ever)),
    mean(is.na(pregnancy_valid$live_birth_count)),
    mean(is.na(df_clean$prenatal_depression)),
    mean(is.na(df_clean$postpartum_depression)),
    mean(is.na(df_clean$postpartum_psychosis)),
    mean(is.na(df_clean$breastfed_any)),
    mean(is.na(breastfed_valid$breastfed_child_count)),
    mean(is.na(breastfed_valid$breastfeeding_total_months)),
    mean(is.na(df_clean$ovary_removal)),
    mean(is.na(df_clean$ovary_removal_age)),
    mean(is.na(df_clean$tubal_ligation)),
    mean(is.na(df_clean$tubal_ligation_age)),
    mean(is.na(df_clean$breast_cancer_dx)),
    mean(is.na(df_clean$ovarian_uterine_cancer_dx)),
    mean(is.na(df_clean$birthcontrol_current)),
    mean(is.na(df_clean$birthcontrol_ever)),
    mean(is.na(df_clean$birthcontrol_current_type)),
    mean(is.na(df_clean$hormone_med_current)),
    mean(is.na(pregnancy_valid$pregnancy_6months_count)),
    mean(is.na(df_clean$bc_pill_ever)),
    mean(is.na(df_clean$bc_patch_ever)),
    mean(is.na(df_clean$bc_ring_ever)),
    mean(is.na(df_clean$bc_implant_ever)),
    mean(is.na(df_clean$bc_injection_ever)),
    mean(is.na(df_clean$bc_iud_ever)),
    mean(is.na(df_clean$bc_other_ever)),
    mean(is.na(df_clean$hormone_med_current_type)),
    mean(is.na(df_clean$hormone_med_ever)),
    mean(is.na(df_clean$hormone_med_ever_details)),
    mean(is.na(df_clean$DataCollection)),
    mean(is.na(df_clean$email)),
    mean(is.na(df_clean$prescreendate))
  ) * 100, 1),

  Notes = c(
    "", "", "",
    "NAs only counted if currently menstruating",
    "NAs only counted if currently menstruating",
    "'2' = both retained",
    "Assumed months",
    "'2' = don’t know, treated as NA",
    "'2' = don’t know, treated as NA",
    "Contingent: amenorrhea_any == 'yes'",
    "'2' = don’t know, treated as NA",
    "Contingent: menopause_symptoms_ever == 'yes'",
    "Contingent: menopause_symptoms_ever == 'yes'",
    "'2' = don’t know, treated as NA",
    "Contingent: ever_pregnant == 'yes'",
    "'2' = don’t know, treated as NA",
    "Contingent: ever_pregnant == 'yes'",
    "'2' = don’t know, treated as NA",
    "'2' = don’t know, treated as NA",
    "'2' = don’t know, treated as NA",
    "'2' = don’t know, treated as NA",
    "Contingent: breastfed_any == 'yes'",
    "Contingent: breastfed_any == 'yes'",
    "'2' = don’t know, treated as NA",
    "Contingent: ovary_removal == 'yes'",
    "'2' = don’t know, treated as NA",
    "Contingent: tubal_ligation == 'yes'",
    "'2' = don’t know, treated as NA",
    "'2' = don’t know, treated as NA",
    "'2' = don’t know, treated as NA",
    "'2' = don’t know, treated as NA",
    "'2' = don’t know, treated as NA",
    "Contingent: ever_pregnant == 'yes'",
    "0 = no, 1 = yes (assumed binary)",
    "0 = no, 1 = yes (assumed binary)",
    "0 = no, 1 = yes (assumed binary)",
    "0 = no, 1 = yes (assumed binary)",
    "0 = no, 1 = yes (assumed binary)",
    "0 = no, 1 = yes (assumed binary)",
    "0 = no, 1 = yes (assumed binary)",
    "0 = no, 1 = yes (assumed binary)",
    "Free text response, not cleaned",
    "'2' = don’t know, treated as NA",
    "Free text response, not cleaned",
    "Collection site or study wave",
    "Participant ID/email",
    "Date of prescreen entry"
  )
)

# Save
write_csv(summary_df, "Outputs/tableDataCleanDetails_RHQ.csv")
saveRDS(summary_df, "Outputs/tableDataCleanDetails_RHQ.RDS")
