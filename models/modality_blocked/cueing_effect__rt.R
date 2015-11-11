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

# Quick plot of means
# -------------------
ggplot(modality_blocked, aes(x = mask_type, y = rt, color = cue_type)) +
  geom_point(stat = "summary", fun.y = "mean", size = 3)

# Models prediction RTs
# ---------------------
# Cueing effect on nomask trials
rt_mod_nomask <- lmerTest::lmer(rt ~ cue_l + cue_q + (1|subj_id),
                                data = filter(modality_blocked, mask_type == "nomask"))
summary(rt_mod_nomask)
report_lmerTest_effect(rt_mod_nomask, "cue_l")

# Reduction in cueing effect on visual mask trials
rt_mod_visualmask <- lmerTest::lmer(rt ~ (cue_l + cue_q) * mask_c + (1|subj_id),
                                    data = filter(modality_blocked, mask_type != "auditory"))
summary(rt_mod_visualmask)

# No reduction in cueing effect on auditory trials
rt_mod_auditorymask <- lmerTest::lmer(rt ~ (cue_l + cue_q) * mask_c + (1|subj_id),
                                      data = filter(modality_blocked, mask_type != "visual"))
summary(rt_mod_auditorymask)

# Treatment contrast model
rt_mod_treat <- lmerTest::lmer(rt ~ (cue_l + cue_q) * (visual_v_nomask + auditory_v_nomask) + (1|subj_id),
                               data = modality_blocked)
summary(rt_mod_treat)

modality_blocked %>%
  group_by(mask_type, cue_type) %>%
  summarize(
    rt = mean(rt, na.rm = TRUE),
    accuracy = mean(is_correct, na.rm = TRUE)
  )

modality_blocked$subj_id %>% unique %>% length
