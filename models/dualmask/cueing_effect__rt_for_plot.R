CLEAR_GLOBAL_ENVIRONMENT <- FALSE
source("scripts/dualmask_data.R")

library(lme4)

source("scripts/contrasts.R")
source("scripts/outliers.R")
source("scripts/report_stats.R")

# Create contrast variables
# -------------------------
dualmask <- recode_mask_type(dualmask)
dualmask <- recode_cue_type(dualmask)

# Remove outlier subjs
# --------------------
dualmask <- filter(dualmask, subj_id %nin% dualmask_outliers)

# Models predicting reaction time
# -------------------------------
# Predict rts from mask_type and cue_type
rt_mod <- lmer(rt ~ mask_c * (cue_l + cue_q) + (1|subj_id), data = dualmask)
