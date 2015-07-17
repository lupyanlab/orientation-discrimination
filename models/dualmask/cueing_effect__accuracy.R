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
error_mod <- glmer(is_error ~ mask_c * (cue_l + cue_q) + (1|subj_id),
                   family = binomial, data = dualmask)
summary(error_mod)

# Effect of mask on valid cue trials
# ----------------------------------
dualmask_valid <- filter(dualmask, cue_type == "valid")
error_mod_valid <- glmer(is_error ~ mask_c + (1|subj_id),
                         family = binomial, data = dualmask_valid)
summary(error_mod_valid)

# Effect of mask on invalid cue trials
# ------------------------------------
dualmask_invalid <- filter(dualmask, cue_type == "invalid")
error_mod_invalid <- glmer(is_error ~ mask_c + (1|subj_id),
                         family = binomial, data = dualmask_invalid)
summary(error_mod_invalid)
