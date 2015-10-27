devtools::load_all("orientationdiscrimination")
data(temporal)

library(lme4)
library(dplyr)

# Create contrast variables
# -------------------------
temporal <- recode_mask_type(temporal)
temporal <- recode_cue_type(temporal)

# Remove outlier subjs
# --------------------
temporal <- filter(temporal, subj_id %nin% temporal_outliers)

# Models prediction error rate
# ----------------------------
# Predict error rate on trials without visual interference
error_mod_nomask <- glmer(is_error ~ cue_l + cue_q + (1|subj_id),
                          family = binomial,
                          data = filter(temporal, mask_type == "nomask"))
summary(error_mod_nomask)
report_glmer_effect(error_mod_nomask, "cue_l")
# -0.14 log-odds, 95% CI [-0.66, 0.34], p = 0.5703


# Predict error rate from mask_type and cue_type
error_mod <- glmer(is_error ~ mask_c * (cue_l + cue_q) + (1|subj_id),
                   family = binomial, data = temporal)
summary(error_mod)

# 
full_mod <- glmer(is_error ~ (mask_m + mask_r) * (cue_l + cue_q) + (1|subj_id),
                  family = binomial, data = temporal)
summary(full_mod)

# Effect of mask on noise cue trials
error_mod_noise <- glmer(is_error ~ mask_m + mask_r + (1|subj_id),
                         family = binomial,
                         data = filter(temporal, cue_type == "noise"))
summary(error_mod_noise)
report_glmer_effect(error_mod_noise, "mask_m")
# -0.12 log-odds, 95% CI [-0.41, 0.16], p = 0.3879
report_glmer_effect(error_mod_noise, "mask_r")
# 0.41 log-odds, 95% CI [0.00, 0.84], p = 0.0470


# Effect of mask on invalid cue trials
error_mod_invalid <- glmer(is_error ~ mask_m + mask_r + (1|subj_id),
                           family = binomial,
                           data = filter(temporal, cue_type == "invalid"))
summary(error_mod_invalid)
report_glmer_effect(error_mod_invalid, "mask_m")
# 0.24 log-odds, 95% CI [-0.34, 0.84], p = 0.4127
report_glmer_effect(error_mod_invalid, "mask_r")
# 0.40 log-odds, 95% CI [-1.20, 0.38], p = 0.3218


# Effect of mask on valid cue trials
error_mod_valid <- glmer(is_error ~ mask_m + mask_r + (1|subj_id),
                         family = binomial,
                         data = filter(temporal, cue_type == "valid"))
summary(error_mod_valid)
#report_glmer_effect(error_mod_valid, "mask_m")
# -0.30 log-odds, 95% CI [-0.66, 0.06], p = 0.0956
report_glmer_effect(error_mod_valid, "mask_r")
# -0.00 log-odds, p = 0.998