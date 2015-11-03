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
# Cueing effect on nomask trials
rt_mod_nomask <- lmerTest::lmer(rt ~ cue_l + cue_q + (1|subj_id),
                                data = filter(modality, mask_type == "nomask"))
summary(rt_mod_nomask)
report_lmerTest_effect(rt_mod_nomask, "cue_l")
# 24.39 ms., 95% CI [17.42, 31.35], p < 0.001

# Reduction in cueing effect on visual mask trials
rt_mod_visualmask <- lmerTest::lmer(rt ~ (cue_l + cue_q) * mask_c + (1|subj_id),
                                    data = filter(modality, mask_type != "auditory"))
summary(rt_mod_visualmask)

# No reduction in cueing effect on auditory trials
rt_mod_auditorymask <- lmerTest::lmer(rt ~ (cue_l + cue_q) * mask_c + (1|subj_id),
                                      data = filter(modality, mask_type != "visual"))
summary(rt_mod_auditorymask)

# Treatment contrast model
rt_mod_treat <- lmerTest::lmer(rt ~ (cue_l + cue_q) * (visual_v_nomask + auditory_v_nomask) + (1|subj_id),
                               data = modality)
summary(rt_mod_treat)
