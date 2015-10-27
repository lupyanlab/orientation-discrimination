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

# Mean error rates
# ----------------
dualmask %>% group_by(cue_type, mask_type) %>%
  summarize(error_rate = round(mean(is_error, na.rm = TRUE) * 100, 2))

# Models prediction accuracy
# --------------------------
# Predict error rate from mask_type and cue_type
error_mod <- glmer(is_error ~ mask_c * (cue_l + cue_q) + (1|subj_id),
                   family = binomial, data = dualmask)
summary(error_mod)
report_glmer_effect(error_mod, "mask_c:cue_l")
# -0.59 log-odds, 95% CI [-1.39, 0.20], p = 0.1389


# Effect of mask on noise cue trials
dualmask_noise <- filter(dualmask, cue_type == "noise")
error_mod_noise <- glmer(is_error ~ mask_c + (1|subj_id),
                         family = binomial, data = dualmask_noise)
summary(error_mod_noise)
report_glmer_effect(error_mod_noise, "mask_c")
# -0.07 log-odds, 95% CI [-0.42, 0.29], p = 0.7136

# Effect of mask on invalid cue trials
dualmask_invalid <- filter(dualmask, cue_type == "invalid")
error_mod_invalid <- glmer(is_error ~ mask_c + (1|subj_id),
                         family = binomial, data = dualmask_invalid)
summary(error_mod_invalid)
report_glmer_effect(error_mod_invalid, "mask_c")
# -0.11 log-odds, 95% CI [-0.80, 0.56], p = 0.7405


# Effect of mask on valid cue trials
dualmask_valid <- filter(dualmask, cue_type == "valid")
error_mod_valid <- glmer(is_error ~ mask_c + (1|subj_id),
                         family = binomial, data = dualmask_valid)
summary(error_mod_valid)
report_glmer_effect(error_mod_valid, "mask_c")
# 0.48 log-odds, 95% CI [0.07, 0.90], p = 0.0228
