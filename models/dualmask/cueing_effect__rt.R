source("scripts/dualmask_data.R")

library(lme4)
source("scripts/contrasts.R")

# Create contrast variables
# -------------------------
dualmask <- recode_mask_type(dualmask)
dualmask <- recode_cue_type(dualmask)

# Remove outlier subj
# -------------------
dualmask <- filter(dualmask, subj_id != "MWP205a")

# Predict rts from mask_type and cue_type
# ---------------------------------------
rt_mod <- lmer(rt ~ mask_c * (cue_l + cue_q) + (1|subj_id), data = dualmask)
summary(rt_mod)

summary(lmerTest::lmer(rt ~ mask_c * (cue_l + cue_q) + (1|subj_id), data = dualmask))
