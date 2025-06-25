#SampleCharacteristicTable

library(tidyverse)
library(gtsummary)
library(gt)

path="Figures"

d<-readRDS("Outputs/RHQdf_clean.RDS")

getnames <- data.frame(Variable = names(d))

prescreenCleanDat_sampleCharTab <- d %>%
  select(cycle_regular_lifetime,
currently_menstruating,
period_last_month,
period_last_year,
cycle_regular_now,
irregular_duration,
irregular_bleed_length_change,
amenorrhea_any,
amenorrhea_duration,
menopause_symptoms_ever,
menopause_symptoms_start_age,
menopause_symptoms_end_age,
ever_pregnant,
pregnancy_count,
pregnancy_6months_ever,
pregnancy_6months_count,
live_birth_count,
prenatal_depression,
postpartum_depression,
postpartum_psychosis,
breastfed_any,
breastfed_child_count,
breastfeeding_total_months,
ovary_removal,
ovary_removal_age,
tubal_ligation,
tubal_ligation_age,
breast_cancer_dx,
ovarian_uterine_cancer_dx,
birthcontrol_current,
birthcontrol_ever,
birthcontrol_current_type,
bc_pill_ever,
bc_patch_ever,
bc_ring_ever,
bc_implant_ever,
bc_injection_ever,
bc_iud_ever,
bc_other_ever,
hormone_med_current,
hormone_med_current_type,
hormone_med_ever,
hormone_med_ever_details,
DataCollection) %>%
  tbl_summary(missing = "ifany") %>%
  bold_labels()



gtsave(as_gt(prescreenCleanDat_sampleCharTab), "Outputs/RHQCleanDat_sampleCharTab.pdf")
