library(lme4)
library(broom)
library(ggplot2)
library(dplyr)
library(devtools)
load_all("orientationwords")
data(bilateral)

# ---- cueing-effect-mod
cueing_effect_mod <- lmer(rt ~ cue_c * mask_c + (1|subj_id),
                          # cue_c is NA on response_type == "word" trials
                          # so using all the data here is fine because
                          # lmer drops those rows implicitly
                          data = bilateral)
tidy(cueing_effect_mod, effects = "fixed")

# ---- cueing-effect-errors-mod
cueing_errors_mod <- glmer(is_error ~ cue_c * mask_c + (1|subj_id),
                           family = binomial,
                           # cue_c is NA on response_type == "word" trials
                           # so using all the data here is fine because
                           # glmer drops those rows implicitly
                           data = bilateral)
tidy(cueing_errors_mod, effects = "fixed") %>%
  add_sig_stars

# ---- word-mod
word_mod <- lmer(rt ~ mask_c + (1|subj_id),
                 data = filter(bilateral, response_type == "word"))
tidy(word_mod, effects = "fixed")

# ---- word-errors-mod
word_error_mod <- glmer(is_error ~ mask_c + (1|subj_id),
                        family = binomial,
                        data = filter(bilateral, response_type == "word"))
tidy(word_error_mod, effects = "fixed") %>%
  add_sig_stars

# ---- rt-plot
ggplot(bilateral, aes(x = mask_c, y = rt, color = cue_task)) +
  geom_point(stat = "summary", fun.y = "mean") +
  geom_line(aes(group = cue_task), stat = "summary", fun.y = "mean") +
  facet_wrap("response_label") +
  scale_x_mask +
  scale_y_rt +
  scale_color_cue_task +
  base_theme

# ---- error-plot
ggplot(bilateral, aes(x = mask_c, y = is_error, color = cue_task)) +
  geom_point(stat = "summary", fun.y = "mean") +
  geom_line(aes(group = cue_task), stat = "summary", fun.y = "mean") +
  facet_wrap("response_label") +
  scale_x_mask +
  scale_y_error +
  scale_color_cue_task +
  base_theme