source("scripts/temporal_data.R")

library(lme4)

source("scripts/contrasts.R")
source("scripts/outliers.R")

# Set contrasts
# -------------
temporal <- recode_mask_type(temporal)
temporal <- recode_cue_type(temporal)

# Drop outliers
# -------------
temporal <- filter(temporal, subj_id %nin% temporal_outliers)

# Predict RT from cue_type and mask_type
# --------------------------------------
rt_mod <- lmerTest::lmer(rt ~ mask_c * (cue_l + cue_q) + (1|subj_id),
                         data = temporal)
summary(rt_mod)

# Replicate the dualmask effect
temporal_noafter <- filter(temporal, mask_type != "after")
rt_mod_during <- lmerTest::lmer(rt ~ mask_c * (cue_l + cue_q) + (1|subj_id),
                                data = temporal_noafter)
summary(rt_mod_during)

# Extend to case where mask is after cue offset
temporal_noduring <- filter(temporal, mask_type != "during")
rt_mod_after <- lmerTest::lmer(rt ~ mask_c * (cue_l + cue_q) + (1|subj_id),
                                data = temporal_noduring)
summary(rt_mod_after)

# Full model
# ----------
rt_mod_full <- lmerTest::lmer(rt ~ (mask_m + mask_r) * (cue_l + cue_q) + (1|subj_id),
                              data = temporal)
summary(rt_mod_full)