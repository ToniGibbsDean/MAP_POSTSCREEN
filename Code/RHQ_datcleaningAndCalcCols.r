

library(tidyverse)

d<-readRDS("Outputs/joinedCorrected_RHQ_dfELIGIBLEONLY.rds")

repro_span = first_period - list(names(d))

cldat <- d %>%
  mutate(
    first_period = as.numeric(stringr::str_extract(first_period, "\\d+(\\.\\d+)?")),
    
    cycle_regular_lifetime = factor(case_when(
      cycle_regular_lifetime == 1 ~ "regular",
      cycle_regular_lifetime == 0 ~ "irregular",
      cycle_regular_lifetime == 2 ~ "both",
      TRUE ~ NA_character_
    )),
    
    currently_menstruating = factor(case_when(
      currently_menstruating == 1 ~ "yes",
      currently_menstruating == 0 ~ "no",
      currently_menstruating == 2 ~ NA_character_,  # Use NA_character_ not "NA"
      TRUE ~ NA_character_
    ))
  )
  
cldat %>% 
select(currently_menstruating) %>% 
mutate(currently_menstruating = as.factor(currently_menstruating)) %>%
#filter(is.na(cycle_regular_lifetime)) %>%
summary()


x<-cldat %>% 
#select(cycle_regular_lifetime) %>% 
filter(is.na(cycle_regular_lifetime)) %>%
summary()

write.csv(x, "Outputs/x.csv")

first_period
cycle_regular_lifetime
currently_menstruating
period_last_month
period_last_year
cycle_regular_now
"irregular_duration",
irregular_bleed_length_change
amenorrhea_any
amenorrhea_duration
menopause_symptoms_ever
menopause_symptoms_start_age
menopause_symptoms_end_age
ever_pregnant
pregnancy_count
pregnancy_6months_ever
live_birth_count
prenatal_depression
postpartum_depression
postpartum_psychosis
breastfed_any
breastfed_child_count
breastfeeding_total_months
ovary_removal
ovary_removal_age
tubal_ligation
tubal_ligation_age
breast_cancer_dx
ovarian_uterine_cancer_dx
birthcontrol_current
birthcontrol_ever
birthcontrol_current_type
"  repro83___1 = ""bc_pill_ever"",
  repro83___2 = ""bc_patch_ever"",
  repro83___3 = ""bc_ring_ever"",
  repro83___4 = ""bc_implant_ever"",
  repro83___5 = ""bc_injection_ever"",
  repro83___6 = ""bc_iud_ever"",
  repro83___7 = ""bc_other_ever"","
hormone_med_current
hormone_med_current_type
hormone_med_ever
hormone_med_ever_details
























