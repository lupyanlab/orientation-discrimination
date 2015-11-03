devtools::load_all("orientationdiscrimination")
data(modality)

library(lme4)
library(dplyr)

# Set contrasts
# -------------
modality <- modality %>%
  recode_modality_mask_type %>%
  recode_modality_cue_type

# Models prediction RTs
# ---------------------
rt_mod_full <- lmer(rt ~ (helmert_main + helmert_residual) * (cue_l + cue_q) + (1|subj_id), data = modality)
