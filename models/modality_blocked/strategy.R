devtools::load_all("orientationdiscrimination")
data(modality_blocked)

library(ggplot2)
library(tidyr)

# Recode variables
# ----------------
modality_blocked <- modality_blocked %>%
  recode_cue_type %>%
  recode_modality_mask_type

# Drop outliers
# -------------
modality_blocked <- modality_blocked %>%
  filter(rt < 2000)

# Add in strategy
# ---------------
modality_blocked <- modality_blocked %>%
  left_join(post_survey_strategies) %>%
  mutate(strategy = ifelse(is.na(strategy), "unknown", strategy))

# Plot means
# ----------
ggplot(modality_blocked, aes(x = mask_type, y = rt, color = cue_type)) +
  geom_point(stat = "summary", fun.y = "mean", shape = 1, size = 3) +
  facet_wrap("strategy")

ggsave("models/modality_blocked/strategy-means.png")

# Calculate cueing effect by hand
# -------------------------------
cueing_effects <- modality_blocked %>%
  group_by(strategy, subj_id, mask_type, cue_type) %>%
  summarize(rt = mean(rt, na.rm = TRUE)) %>%
  spread(cue_type, rt) %>%
  mutate(cueing_effect = invalid - valid) %>%
  select(-(invalid:valid))
  
ggplot(cueing_effects, aes(x = mask_type, y = cueing_effect, color = strategy)) +
  geom_point(shape = 1) +
  geom_point(stat = "summary", fun.y = "mean", size = 3) +
  scale_y_continuous("cueing_effect = invalid_rt - valid_rt") +
  facet_wrap("strategy")

ggsave("models/modality_blocked/strategy-cueing-effect.png")
  