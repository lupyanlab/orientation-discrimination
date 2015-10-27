devtools::load_all("orientationdiscrimination")
data(dualmask)

library(lme4)
library(dplyr)

# Create contrast variables
# -------------------------
dualmask <- dualmask %>%
  recode_mask_type %>%
  recode_cue_type

# Remove outlier subjs
# --------------------
dualmask <- filter(dualmask, subj_id %nin% dualmask_outliers)

# Models predicting reaction time
# -------------------------------
# Effect of valid cue relative to baseline
rt_mod_nomask_valid <- lmerTest::lmer(rt ~ cue_c + (1|subj_id),
                                      data = filter(dualmask, mask_type == "nomask", cue_type != "invalid"))
summary(rt_mod_nomask_valid)
report_lmerTest_effect(rt_mod_nomask_valid, "cue_c")
# -19.13 ms., 95% CI [-27.18, -11.07], p = 0.0000


# Effect of invalid cue relative to baseline
rt_mod_nomask_invalid <- lmerTest::lmer(rt ~ cue_c + (1|subj_id),
                                        data = filter(dualmask, mask_type == "nomask", cue_type != "valid"))
summary(rt_mod_nomask_invalid)
report_lmerTest_effect(rt_mod_nomask_invalid, "cue_c")
# 36.97 ms., 95% CI [24.43, 49.52], p = 0.0000


# Predict rts from mask_type and cue_type
rt_mod <- lmerTest::lmer(rt ~ mask_c * (cue_l + cue_q) + (1|subj_id), data = dualmask)
summary(rt_mod)
report_lmerTest_effect(rt_mod, "mask_c:cue_l")
# -25.53 ms., 95% CI [-42.89, -8.18], p = 0.0040

# Visual interference affected verbal cue trials only
unique(dualmask[,c("cue_type", "cue_l", "cue_q")])
report_lmerTest_effect(rt_mod, "mask_c:cue_q")
# 14.29 ms., 95% CI [2.82, 25.77], p = 0.0147


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


# Visual interference affected performance on verbal cue trials only
