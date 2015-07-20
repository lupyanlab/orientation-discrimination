source("scripts/temporal_data.R")
library(lme4)
source("scripts/contrasts.R")
source("scripts/outliers.R")

# Create contrast variables
# -------------------------
temporal <- recode_mask_type(temporal)
temporal <- recode_cue_type(temporal)

# Remove outlier subjs
# --------------------
temporal <- filter(temporal, subj_id %nin% temporal_outliers)

# Predict rts from mask_type and cue_type
# ---------------------------------------
error_mod <- glmer(is_error ~ mask_c * (cue_l + cue_q) + (1|subj_id),
                   family = binomial, data = temporal)
summary(error_mod)


# Effect of mask on noise cue trials
temporal_noise <- filter(temporal, cue_type == "noise")
error_mod_noise <- glmer(is_error ~ mask_c + (1|subj_id),
                         family = binomial, data = temporal_noise)
summary(error_mod_noise)


# Effect of mask on invalid cue trials
temporal_invalid <- filter(temporal, cue_type == "invalid")
error_mod_invalid <- glmer(is_error ~ mask_c + (1|subj_id),
                           family = binomial, data = temporal_invalid)
summary(error_mod_invalid)


# Effect of mask on valid cue trials
temporal_valid <- filter(temporal, cue_type == "valid")
error_mod_valid <- glmer(is_error ~ mask_c + (1|subj_id),
                         family = binomial, data = temporal_valid)
summary(error_mod_valid)
