CLEAR_GLOBAL_ENVIRONMENT <- FALSE
source("scripts/temporal_data.R")

library(lme4)

source("scripts/contrasts.R")
source("scripts/outliers.R")
source("scripts/report_stats.R")

# Set contrasts
# -------------
temporal <- recode_mask_type(temporal)
temporal <- recode_cue_type(temporal)

# Drop outliers
# -------------
temporal <- filter(temporal, subj_id %nin% temporal_outliers)

# Models prediction RTs
# ---------------------
rt_mod_full <- lmer(rt ~ (mask_m + mask_r) * (cue_l + cue_q) + (1|subj_id), data = temporal)
