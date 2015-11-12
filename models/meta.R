devtools::load_all("orientationdiscrimination")
data(orientation)

library(ggplot2)
library(lme4)

# Drop outliers 
# -------------
orientation <- orientation %>%
  filter(rt < 2000)

# Recode variables
# ----------------
mask_type_map <- data_frame(
  mask_type = c("nomask", "mask", "during", "after", "auditory", "visual"),
  mask_c = c(-0.5, 0.5, 0.5, 0.5, NA, 0.5)
)

cue_type_map <- data_frame(
  cue_type = c("valid", "invalid", "noise", "nocue"),
  cue_l = car::recode(cue_type, "'valid'=-1/2; 'invalid'=1/2; else=0", as.factor.result = FALSE, as.numeric.result = TRUE),
  cue_q = car::recode(cue_type, "'valid'=-1/3; 'invalid'=-1/3; else=2/3", as.factor.result = FALSE, as.numeric.result = TRUE)
)

orientation <- orientation %>%
  left_join(mask_type_map) %>%
  left_join(cue_type_map)

# Fit RT model
# ------------
rt_mod <- lmer(rt ~ (cue_l + cue_q) * mask_c + (1|subj_id) + (1|exp), data = orientation)
summary(rt_mod)

# Fit error model
# ---------------
error_mod <- glmer(is_correct ~ (cue_l + cue_q) * mask_c + (1|subj_id) + (1|exp),
                   data = orientation, family = binomial)
summary(error_mod)
