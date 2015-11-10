devtools::load_all("orientationdiscrimination")
data(modality_blocked)

library(lme4)
library(dplyr)

# Set contrasts
# -------------
modality_blocked <- modality_blocked %>%
  recode_modality_mask_type %>%
  recode_cue_type

# Drop outliers
# -------------
modality_blocked <- modality_blocked %>%
  filter(rt < 2000)

# Models prediction RTs
# ---------------------
rt_mod_full <- lmer(rt ~ (helmert_main + helmert_residual) * (cue_l + cue_q) + (1|subj_id),
                    data = modality_blocked)
