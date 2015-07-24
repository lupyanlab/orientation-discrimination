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
rt_mod <- lmerTest::lmer(rt ~ mask_c * (cue_l + cue_q) + (1|subj_id), data = dualmask)
summary(rt_mod)
report_lmerTest_effect(rt_mod, "mask_c:cue_l")

# Predict rts from mask_type (invalid cues)
rt_mod_invalid <- lmerTest::lmer(rt ~ mask_c + (1|subj_id),
                                 data = filter(dualmask, cue_type == "invalid"))
summary(rt_mod_invalid)
report_lmerTest_effect(rt_mod_invalid, "mask_c")


# Predict rts from mask_type (noise cues)
rt_mod_noise <- lmerTest::lmer(rt ~ mask_c + (1|subj_id),
                               data = filter(dualmask, cue_type == "noise"))
summary(rt_mod_noise)
report_lmerTest_effect(rt_mod_noise, "mask_c")
# 3.22 ms., 95% CI [-4.39, 10.83], p = 0.4068

# Predict rts from mask_type (valid cues)
rt_mod_valid <- lmerTest::lmer(rt ~ mask_c + (1|subj_id),
                               data = filter(dualmask, cue_type == "valid"))
summary(rt_mod_valid)
report_lmerTest_effect(rt_mod_valid, "mask_c")
# 1.72 ms., 95% CI [-6.57, 10.01], p = 0.6843
