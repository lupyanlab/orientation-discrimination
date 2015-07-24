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
# Cueing effect on nomask trials
rt_mod_nomask <- lmerTest::lmer(rt ~ cue_l + cue_q + (1|subj_id),
                                data = filter(temporal, mask_type == "nomask"))
summary(rt_mod_nomask)
report_lmerTest_effect(rt_mod_nomask, "cue_l")
# 54.39 ms., 95% CI [44.75, 64.03], p = 0.0000


# Predict RT from cue_type and overall mask_type
rt_mod <- lmerTest::lmer(rt ~ mask_c * (cue_l + cue_q) + (1|subj_id),
                         data = temporal)
summary(rt_mod)
report_lmerTest_effect(rt_mod, "mask_c:cue_l")
# -17.13 ms., 95% CI [-30.69, -3.57], p = 0.0133


# Replicate the dualmask effect
temporal_noafter <- filter(temporal, mask_type != "after")
rt_mod_during <- lmerTest::lmer(rt ~ mask_c * (cue_l + cue_q) + (1|subj_id),
                                data = temporal_noafter)
summary(rt_mod_during)
report_lmerTest_effect(rt_mod_during, "mask_c:cue_l")
# -15.62 ms., 95% CI [-32.24, 1.01], p = 0.0657


# Extend to case where mask is after cue offset
temporal_noduring <- filter(temporal, mask_type != "during")
rt_mod_after <- lmerTest::lmer(rt ~ mask_c * (cue_l + cue_q) + (1|subj_id),
                                data = temporal_noduring)
summary(rt_mod_after)
report_lmerTest_effect(rt_mod_after, "mask_c:cue_l")
# -18.74 ms., 95% CI [-35.40, -2.08], p = 0.0275


# Effect of mask on invalid cue trials
rt_mod_invalid <- lmerTest::lmer(rt ~ mask_m + mask_r + (1|subj_id),
                                 data = filter(temporal, cue_type == "invalid"))
summary(rt_mod_invalid)
report_lmerTest_effect(rt_mod_invalid, "mask_m")
# -13.11 ms., 95% CI [-26.45, 0.22], p = 0.0542
report_lmerTest_effect(rt_mod_invalid, "mask_r")
# 7.17 ms., 95% CI [-11.74, 26.08], p = 0.4574


# Effect of mask on valid cue trials
rt_mod_valid <- lmerTest::lmer(rt ~ mask_m + mask_r + (1|subj_id),
                               data = filter(temporal, cue_type == "valid"))
summary(rt_mod_valid)
report_lmerTest_effect(rt_mod_valid, "mask_m")
# 4.05 ms., 95% CI [-2.55, 10.64], p = 0.2291
report_lmerTest_effect(rt_mod_valid, "mask_r")
# 9.78 ms., 95% CI [0.47, 19.08], p = 0.0395


# Effect of mask on noise cue trials
rt_mod_noise <- lmerTest::lmer(rt ~ mask_m + mask_r + (1|subj_id),
                               data = filter(temporal, cue_type == "noise"))
summary(rt_mod_noise)
report_lmerTest_effect(rt_mod_noise, "mask_m")
# 2.19 ms., 95% CI [-3.57, 7.95], p = 0.4569
report_lmerTest_effect(rt_mod_noise, "mask_r")
# 16.33 ms., 95% CI [8.19, 24.47], p = 0.0001

# Full model
# ----------
rt_mod_full <- lmerTest::lmer(rt ~ (mask_m + mask_r) * (cue_l + cue_q) + (1|subj_id),
                              data = temporal)
summary(rt_mod_full)
report_lmerTest_effect(rt_mod_full, "mask_r:cue_l")
# -3.08 ms., 95% CI [-22.26, 16.10], p = 0.7529