# ---- setup
library(dplyr)
library(tidyr)

library(lme4)
library(AICcmodavg)
library(broom)

library(ggplot2)
library(gridExtra)

# ---- data
library(orientationdiscrimination)
data(unilateral)

# ---- theme
scale_x_mask <- ggplot2::scale_x_continuous("", breaks = c(-0.5, 0.5),
                                            labels = c("Blank screen", "Visual interference"))
scale_alpha_mask <- scale_alpha_manual(values = c(0.4, 0.9))

colors <- RColorBrewer::brewer.pal(3, "Set2")
names(colors) <- c("green", "orange", "blue")

scale_y_rt <- ggplot2::scale_y_continuous("Reaction Time")
scale_y_error <- ggplot2::scale_y_continuous("Error Rate", labels = scales::percent)

cue_colors <- list(invalid = "#d7191c", valid = "#a6d96a")
scale_color_cue_type <- scale_color_manual("", labels = c("Invalid", "Valid"), values = unlist(cue_colors))
cue_task_colors <- c(cue_colors, list(word = "gray"))
scale_color_cue_task <- ggplot2::scale_color_manual(
  "",
  labels = c("Invalid", "Valid", "Word Repetition"),
  values = unlist(cue_task_colors)
)

base_theme <- theme_minimal() +
  ggplot2::theme(
    axis.ticks = ggplot2::element_blank(),
    panel.margin = grid::unit(2, "lines"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )


# ---- cueing-effect-mod
cue_mod <- lmerTest::lmer(rt ~ cue_c + (1|subj_id),
                data = filter(unilateral, response_type == "pic", mask_type == "nomask"))
tidy(cue_mod, effects = "fixed")

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

# ---- plot-preds
get_cue_mod_preds <- function(mod) {
  tidy(mod, effects = "fixed") %>%
    filter(term == "cue_c")
}

get_cueing_effects <- function() {
  pic_trials <- filter(unilateral, response_type == "pic")
  
  f <- formula(rt ~ cue_c + (1|subj_id))
  nomask <- lmer(f, data = filter(pic_trials, mask_type == "nomask"))
  mask <- lmer(f, data = filter(pic_trials, mask_type == "mask"))
  
  nomask_preds <- get_cue_mod_preds(nomask) %>% mutate(mask_c = -0.5)
  mask_preds <- get_cue_mod_preds(mask) %>% mutate(mask_c = 0.5)
  rbind(nomask_preds, mask_preds) %>% mutate(estimate = -estimate)
}

get_cue_mod_preds <- function(mod) {
  x_preds <- expand.grid(mask_c = c(-0.5, 0.5), cue_c = c(-0.5, 0.5))
  y_preds <- predictSE(mod, x_preds, se = TRUE)
  cbind(x_preds, y_preds)
}

format_cueing_effect <- function(preds) {
  list(
    error = preds,
    box = format_box(preds)
  )
}

format_box <- function(preds) {
  bar_width = 0.2
  preds %>%
    group_by(mask_c) %>%
    summarize(
      ymin = min(fit),
      ymax = max(fit)
    ) %>%
    mutate(
      xmin = mask_c - bar_width,
      xmax = mask_c + bar_width
    )
}

get_word_mod_preds <- function(mod) {
  x_preds <- expand.grid(mask_c = c(-0.5, 0.5), cue_c = 0.0)
  y_preds <- predictSE(mod, x_preds, se = TRUE)
  cbind(x_preds, y_preds)
}

cueing_effects <- get_cue_mod_preds(pic_mod) %>% format_cueing_effect
word_error_preds <- get_word_mod_preds(word_mod)

# ---- rt-plot
base_plot <- ggplot(mapping = aes(x = mask_c, alpha = factor(mask_c))) +
  scale_x_mask +
  scale_alpha_mask +
  base_theme +
  theme(legend.position = "none")

pic_trials_plot <- base_plot +
  geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            fill=colors[["blue"]], data = cueing_effects$box) +
  geom_point(aes(y = fit, shape = factor(cue_c)), data = cueing_effects$error,
             size = 4.0) +
  geom_errorbar(aes(ymin = fit-se.fit, ymax = fit+se.fit), data = cueing_effects$error,
                width = 0.4) +
  scale_y_continuous("Reaction time (ms)") +
  scale_shape_manual("", labels = c("Invalid cue", "Valid cue"), values = c(1, 16)) +
  coord_cartesian(ylim = c(600, 750)) +
  ggtitle("Cueing effect") +
  guides(alpha = "none") +
  theme(legend.position = "top")

word_trials_plot <- base_plot +
  geom_bar(aes(y = fit), data = word_error_preds, stat = "identity",
           width = 1.0, fill = colors[["green"]]) +
  geom_linerange(aes(y = fit, ymin = fit-se.fit, ymax = fit+se.fit),
                 data = word_error_preds) +
  scale_y_continuous("Reaction time (ms)") +
  coord_cartesian(ylim = c(750, 900)) +
  ggtitle("Word repetition")

grid.arrange(pic_trials_plot, word_trials_plot, nrow = 1)

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