devtools::load_all("orientationdiscrimination")
data(orientation)

library(dplyr)
library(lme4)

# Create contrast variables
# -------------------------
orientation <- orientation %>%
  recode_mask_type %>%
  recode_cue_type

# Remove outlier subjs
# --------------------
orientation <- filter(orientation,
                      subj_id %nin% dualmask_outliers,
                      subj_id %nin% temporal_outliers)

# Predict rts from mask_type and cue_type
# ---------------------------------------
rt_mod <- lmerTest::lmer(rt ~ mask_c * (cue_l + cue_q) + (1|subj_id), data = orientation)
summary(rt_mod)
report_lmerTest_effect(rt_mod, "mask_c:cue_l")
