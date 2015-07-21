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

# Models predicting error rate (is_error)
# ---------------------------------------
# Cueing effect in accuracies (nomask trials)
cueing_effect_error_mod <- glmer(is_error ~ cue_l + cue_q + (1|subj_id),
                                 family = binomial,
                                 data = filter(orientation, mask_type == "nomask"))
summary(cueing_effect_error_mod)
# Hearing a verbal cue did not influence accuracy performance

orientation %>%
  filter(mask_type == "nomask") %>%
  group_by(cue_type) %>%
  summarize(error_rate = mean(is_error, na.rm = TRUE))

# Predict rts from mask_type and cue_type
# ---------------------------------------
# Collapses across both experiments
error_mod <- glmer(is_error ~ mask_c * (cue_l + cue_q) + (1|subj_id),
                   family = binomial, data = orientation)
summary(error_mod)

# Is the reduction in the cueing effect (mask_c:cue_l)


# Effect of mask on noise cue trials
orientation_noise <- filter(orientation, cue_type == "noise")
error_mod_noise <- glmer(is_error ~ mask_c + (1|subj_id),
                         family = binomial, data = orientation_noise)
summary(error_mod_noise)


# Effect of mask on invalid cue trials
orientation_invalid <- filter(orientation, cue_type == "invalid")
error_mod_invalid <- glmer(is_error ~ mask_c + (1|subj_id),
                           family = binomial, data = orientation_invalid)
summary(error_mod_invalid)


# Effect of mask on valid cue trials
orientation_valid <- filter(orientation, cue_type == "valid")
error_mod_valid <- glmer(is_error ~ mask_c + (1|subj_id),
                         family = binomial, data = orientation_valid)
summary(error_mod_valid)
