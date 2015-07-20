source("scripts/all_data.R")

library(lme4)

source("scripts/contrasts.R")
source("scripts/outliers.R")

# Create contrast variables
# -------------------------
orientation <- recode_mask_type(orientation)
orientation <- recode_cue_type(orientation)

# Remove outlier subjs
# --------------------
orientation <- filter(orientation,
                      subj_id %nin% dualmask_outliers,
                      subj_id %nin% temporal_outliers)

# Predict rts from mask_type and cue_type
# ---------------------------------------
rt_mod <- lmerTest::lmer(rt ~ mask_c * (cue_l + cue_q) + (1|subj_id), data = orientation)
summary(rt_mod)