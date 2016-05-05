# ---- setup
library(dplyr)
library(tidyr)

library(lme4)
library(broom)

library(ggplot2)

library(orientationdiscrimination)
data(unilateral)

scale_x_mask <- ggplot2::scale_x_continuous("", breaks = c(-0.5, 0.5),
                                            labels = c("No mask", "Mask"))

scale_y_rt <- ggplot2::scale_y_continuous("Reaction Time")
scale_y_error <- ggplot2::scale_y_continuous("Error Rate", labels = scales::percent)

cue_colors <- list(invalid = "#d7191c", valid = "#a6d96a")
scale_color_cue_type <- ggplot2::scale_color_manual("",
                                                    labels = c("Invalid", "Valid"),
                                                    values = unlist(cue_colors))

cue_task_colors <- c(cue_colors, list(word = "gray"))
scale_color_cue_task <- ggplot2::scale_color_manual(
  "",
  labels = c("Invalid", "Valid", "Word Repetition"),
  values = unlist(cue_task_colors)
)

base_theme <- ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.ticks = ggplot2::element_blank(),
    panel.margin = grid::unit(2, "lines")
  )

# Version 1 --------------------------------------------------------------------

# ---- v1-pic-mod
v1_pic_mod <- lmer(rt ~ cue_c * mask_c + (1|subj_id),
                   data = filter(unilateral, version == 1, response_type == "pic"))
tidy(v1_pic_mod, effects = "fixed")

# ---- v1-pic-error-mod
v1_pic_error_mod <- glmer(is_error ~ cue_c * mask_c + (1|subj_id),
                          data = filter(unilateral, version == 1, response_type == "pic"),
                          family = binomial)
tidy(v1_pic_error_mod, effects = "fixed") %>%
  add_sig_stars

# ---- v1-word-mod
v1_word_mod <- lmer(rt ~ mask_c + cue_c + (1|subj_id),
                    data = filter(unilateral, version == 1, response_type == "word"))
tidy(v1_word_mod, effects = "fixed")

# ---- v1-word-error-mod
v1_word_error_mod <- glmer(is_error ~ mask_c + cue_c + (1|subj_id),
                           data = filter(unilateral, version == 1, response_type == "word"),
                           family = binomial)
tidy(v1_word_error_mod, effects = "fixed")

# ---- v1-plot
ggplot(filter(unilateral, version == 1),
       aes(x = mask_c, y = rt, color = cue_task)) +
  geom_point(stat = "summary", fun.y = "mean") +
  geom_line(stat = "summary", fun.y = "mean") +
  facet_wrap("response_label") +
  scale_x_mask +
  scale_y_rt +
  scale_color_cue_task +
  base_theme +
  ggtitle("V1: Effect of mask on RTs")

# ---- v1-error-plot
ggplot(filter(unilateral, version == 1),
       aes(x = mask_c, y = is_error, color = cue_task)) +
  geom_point(stat = "summary", fun.y = "mean") +
  geom_line(stat = "summary", fun.y = "mean") +
  facet_wrap("response_label") +
  scale_x_mask +
  scale_y_error +
  scale_color_cue_task +
  base_theme +
  ggtitle("V1: Effect of mask on errors")

# ---- v1-error-type-plot
error_types <- unilateral %>%
  filter(version == 1) %>%
  group_by(response_type) %>%
  summarize(
    wrong_type = sum(error_type == "wrong_type", na.rm = TRUE)/n(),
    wrong_key = sum(error_type == "wrong_key", na.rm = TRUE)/n()
  ) %>%
  gather(error_type, error_rate, -response_type)

ggplot(error_types, aes(x = response_type, y = error_rate, fill = error_type, order = rev(error_type))) +
  geom_bar(stat = "identity") +
  scale_y_error +
  base_theme

# Version 2 --------------------------------------------------------------------

# ---- v2-pic-mod
v2_pic_mod <- lmer(rt ~ cue_c * mask_c + (1|subj_id),
                   data = filter(unilateral, version == 2, response_type == "pic"))
tidy(v2_pic_mod, effects = "fixed")

# ---- v2-pic-error-mod
v2_pic_error_mod <- glmer(is_error ~ cue_c * mask_c + (1|subj_id),
                          data = filter(unilateral, version == 2, response_type == "pic"),
                          family = binomial)
tidy(v2_pic_error_mod, effects = "fixed")

# ---- v2-word-mod
v2_word_mod <- lmer(rt ~ mask_c + cue_c + (1|subj_id),
                    data = filter(unilateral, version == 2, response_type == "word"))
tidy(v2_word_mod, effects = "fixed")

# ---- v2-word-error-mod
v2_word_error_mod <- glmer(is_error ~ mask_c + cue_c + (1|subj_id),
                           data = filter(unilateral, version == 2, response_type == "word"),
                           family = binomial)
tidy(v2_word_error_mod, effects = "fixed")

# ---- v2-plot
ggplot(filter(unilateral, version == 2),
       aes(x = mask_c, y = rt, color = cue_task)) +
  geom_point(stat = "summary", fun.y = "mean") +
  geom_line(stat = "summary", fun.y = "mean") +
  facet_wrap("response_label") +
  scale_x_mask +
  scale_y_rt +
  scale_color_cue_task +
  base_theme +
  ggtitle("V2: Effect of mask on RTs")

# ---- v2-error-plot
ggplot(filter(unilateral, version == 2),
       aes(x = mask_c, y = is_error, color = cue_task)) +
  geom_point(stat = "summary", fun.y = "mean") +
  geom_line(stat = "summary", fun.y = "mean") +
  facet_wrap("response_label") +
  scale_x_mask +
  scale_y_error +
  scale_color_cue_task +
  base_theme +
  ggtitle("V2: Effect of mask on errors")

# ---- v2-error-type-plot
error_types <- unilateral %>%
  filter(version == 2) %>%
  group_by(response_type) %>%
  summarize(
    wrong_type = sum(error_type == "wrong_type", na.rm = TRUE)/n(),
    wrong_key = sum(error_type == "wrong_key", na.rm = TRUE)/n()
  ) %>%
  gather(error_type, error_rate, -response_type)

ggplot(error_types, aes(x = response_type, y = error_rate, fill = error_type, order = rev(error_type))) +
  geom_bar(stat = "identity") +
  scale_y_error +
  base_theme

# Overall ----------------------------------------------------------------------

# ---- pic-mod
pic_mod <- lmer(rt ~ cue_c * mask_c + (1|subj_id),
                data = filter(unilateral, response_type == "pic"))
tidy(pic_mod, effects = "fixed")

# ---- pic-error-mod
pic_error_mod <- glmer(is_error ~ cue_c * mask_c + (1|subj_id),
                       data = filter(unilateral, response_type == "pic"),
                       family = binomial)
tidy(pic_error_mod, effects = "fixed") %>%
  add_sig_stars

# ---- word-mod
word_mod <- lmer(rt ~ mask_c + cue_c + (1|subj_id),
                 data = filter(unilateral, response_type == "word"))
tidy(word_mod, effects = "fixed")

# ---- word-error-mod
word_error_mod <- glmer(is_error ~ mask_c + cue_c + (1|subj_id),
                        data = filter(unilateral, response_type == "word"),
                        family = binomial)
tidy(word_error_mod, effects = "fixed") %>%
  add_sig_stars

# ---- overall-plot
ggplot(unilateral, aes(x = mask_c, y = rt, color = cue_task)) +
  geom_point(stat = "summary", fun.y = "mean") +
  geom_line(stat = "summary", fun.y = "mean") +
  facet_wrap("response_label") +
  scale_x_mask +
  scale_y_rt +
  scale_color_cue_task +
  base_theme +
  ggtitle("Effect of mask on RTs")

# ---- overall-error-plot
ggplot(unilateral, aes(x = mask_c, y = is_error, color = cue_task)) +
  geom_point(stat = "summary", fun.y = "mean") +
  geom_line(stat = "summary", fun.y = "mean") +
  facet_wrap("response_label") +
  scale_x_mask +
  scale_y_error +
  scale_color_cue_task +
  base_theme +
  ggtitle("Effect of mask on errors")

# ---- mask-correlation-plot
cue_mask_coefs <- unilateral %>%
  filter(response_type == "pic") %>%
  group_by(subj_id) %>%
  do(rt_mod = lm(rt ~ cue_c * mask_c, data = .)) %>%
  tidy("rt_mod") %>%
  filter(term == "cue_c:mask_c") %>%
  select(subj_id, cue_mask = estimate)

word_mask_coefs <- unilateral %>%
  filter(response_type == "word") %>%
  group_by(subj_id) %>%
  do(rt_mod = lm(rt ~ mask_c + cue_c, data = .)) %>%
  tidy("rt_mod") %>%
  filter(term == "mask_c") %>%
  select(subj_id, word_mask = estimate)

mask_coefs <- merge(cue_mask_coefs, word_mask_coefs)

ggplot(mask_coefs, aes(x = cue_mask, y = word_mask)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous("Effect of mask on cueing effect") +
  scale_y_continuous("Effect of mask on word repetition") +
  base_theme

# ---- mask-correlation-mod
mask_correlation_mod <- lm(word_mask ~ cue_mask, data = mask_coefs)
tidy(mask_correlation_mod) %>%
  add_sig_stars

# ---- subjs
z_score <- function(x) (x - mean(x))/sd(x)

subj_means <- unilateral %>%
  group_by(subj_id) %>%
  summarize(
    rt = mean(rt, na.rm = TRUE),
    error = mean(is_error, na.rm = TRUE)
  ) %>%
  mutate(
    rt = z_score(rt),
    error = z_score(error)
  )

subj_ranks <- subj_means %>%
  transmute(
    subj_id,
    rt = rank(rt),
    error = rank(error, ties = "first")
  ) %>%
  gather(measure, rank, -subj_id)

subjs <- subj_means %>%
  gather(measure, value, -subj_id) %>%
  left_join(subj_ranks)

subjs_by_error <- subj_ranks %>%
  filter(measure == "error") %>%
  arrange(rank) %>%
  .$subj_id

subjs <- subjs %>%
  mutate(
    subj_id = factor(subj_id, levels = subjs_by_error),
    measure = factor(measure, levels = c("error", "rt"), labels = c("Errors", "Reaction times"))
  )

ggplot(subjs, aes(x = rank, y = value, color = subj_id)) +
  geom_point() +
  geom_text(aes(label = subj_id), hjust = -0.1, angle = 90, size = 4) +
  scale_x_continuous("Rank", breaks = c(1, seq(5, 30, by = 5))) +
  scale_y_continuous("z-score", breaks = -2:2) +
  geom_hline(aes(yintercept = yintercept),
             data = data_frame(yintercept = c(-2, 2)),
             lty = 2, alpha = 0.4) +
  facet_wrap("measure", ncol = 1) +
  coord_cartesian(ylim = c(-4, 8)) +
  base_theme +
  theme(legend.position = "none")