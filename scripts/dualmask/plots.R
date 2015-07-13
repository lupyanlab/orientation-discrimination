library(dplyr)
library(lme4)
library(AICcmodavg)
library(ggplot2)
library(grid)
library(gridExtra)
library(scales)
library(png)

source("./helper/within-subj-error.R")
source("./figure/base_theme.R")

df <- read.csv("./dualmask/dualmask-final.csv", stringsAsFactors = FALSE)
df <- filter(df, subj_id != "MWP205a")

# ------------------------------------------------------------------------------
# RT estimates

mod <- lmer(rt ~ (cue_l + cue_q) * mask_c + (1|subj_id), data = df)
x_preds <- unique(df[,c("cue_type", "cue_l", "cue_q", "mask_type", "mask_c")])
y_preds <- predictSE.mer(mod, x_preds, type = "response", se.fit = TRUE)
rt_mean_se <- cbind(x_preds, y_preds) %.%
  mutate(
    cue_type = factor(cue_type, levels = c("invalid", "noise", "valid")),
    mask_type = factor(mask_type, levels = c("nomask", "mask"))
  ) %.%
  select(cue_type, mask_type, rt = fit, mean_se = se.fit) %.%
  arrange(mask_type, cue_type)

rt_diff_se <- df %.%
  summarySEwithin(measurevar = "rt", na.rm = TRUE, idvar = "subj_id",
                  withinvars = c("mask_type", "cue_type")) %.%
  mutate(
    mask_type = factor(mask_type, levels = c("nomask", "mask")),
    cue_type = factor(cue_type, levels = c("invalid", "noise", "valid"))
  ) %.% 
  select(mask_type, cue_type, diff_se = se) %.%
  arrange(mask_type, cue_type)

rt_means <- merge(rt_mean_se, rt_diff_se)

cue_type_dodge = position_dodge(width = 0.08)
rt_means_plot <- ggplot(rt_means, aes(x = mask_type, y = rt, color = cue_type)) +
  geom_linerange(aes(group = cue_type, ymin = rt - mean_se, ymax = rt + mean_se), 
    position = cue_type_dodge, color = "gray", lty = 2) +
  geom_errorbar(aes(ymin = rt - diff_se, ymax = rt + diff_se),
    position = cue_type_dodge, width = 0.1) +
  geom_point(position = cue_type_dodge,
    size = 2.8) +
  geom_line(aes(group = cue_type), position = cue_type_dodge, size = 0.8) +
  coord_cartesian(ylim = c(450, 560), xlim = c(0.4, 2.6)) +
  scale_y_continuous("Reaction Time (ms)") +
  scale_x_discrete("", labels = c("No Interference", "Visual Interference")) +
  scale_color_discrete("Auditory Cue", labels = c("Invalid", "Noise", "Valid")) +
  base_theme + theme(
    legend.position = c(0.92, 0.62)
  )
rt_means_plot

ggsave("./dualmask/figure/dualmask_rt_means.png", plot = rt_means_plot, width = 6.5, height = 4, units = "in")

# ------------------------------------------------------------------------------
# Effect of mask on cueing effect in RTs

get_rt_estimates <- function(lmermod, confint_method = "Wald") {
  estimates <- summary(lmermod)$coefficients
  estimates <- as.data.frame(estimates)
  estimates$param <- row.names(estimates); row.names(estimates) <- NULL
  
  intervals <- confint(lmermod, method = confint_method)
  intervals <- as.data.frame(intervals)
  intervals$param <- row.names(intervals); row.names(intervals) <- NULL
  
  merge(estimates, intervals)
}

df_nomask <- filter(df, mask_type == "nomask")
mod_nomask_rt <- lmer(rt ~ cue_l + cue_q + (1|subj_id), data = df_nomask)
nomask_est <- get_rt_estimates(mod_nomask_rt) %.% mutate(mask_type = "nomask")

df_mask <- filter(df, mask_type == "mask")
mod_mask_rt <- lmer(rt ~ cue_l + cue_q + (1|subj_id), data = df_mask)
mask_est <- get_rt_estimates(mod_mask_rt) %.% mutate(mask_type = "mask")

rt_estimates <- rbind(nomask_est, mask_est) %.%
  mutate(mask_type = factor(mask_type, levels = c("nomask", "mask"))) %.%
  plyr::rename(c("Estimate" = "cue_effect", "2.5 %" = "lwr", "97.5 %" = "upr",
    "Std. Error" = "se")) %.%
  filter(param == "cue_l") %.%
  select(cue_effect, se, lwr, upr, mask_type)

load_png_annotation <- function(image_pth, alpha = 0.2) {
  img <- readPNG(image_pth, info = TRUE)
  img_alpha <- matrix(rgb(img[,,1], img[,,2], img[,,3], alpha), nrow=dim(img)[1])
  img_grob <- rasterGrob(img_alpha, interpolate = TRUE)
  
  img_grob
}

annot_no_interference <- load_png_annotation("./figure/annotations/no_interference.png", alpha = 0.8)
annot_interference <- load_png_annotation("./figure/annotations/concurrent_interference.png", alpha = 0.8)

rt_cueing_plot <- ggplot(rt_estimates, aes(x = mask_type, y = cue_effect)) + 
  geom_bar(aes(fill = mask_type), stat = "identity", width = 0.4, color = "black") +
  #annotation_custom(annot_no_interference, xmin = 0.84, xmax = 1.16, ymin = 0, ymax = 18) +
  #annotation_custom(annot_interference, xmin = 1.84, xmax = 2.16, ymin = 0, ymax = 18) +
  geom_linerange(aes(ymin = cue_effect - se, ymax = cue_effect + se), size = 0.6) + 
  coord_cartesian(ylim = c(0, 80), xlim = c(0.4, 2.6)) +
  scale_x_discrete("", labels = c("No Interference", "Visual Interference")) +
  scale_y_continuous("Cueing Effect (ms)", breaks = seq(0, 100, by=20)) +
  scale_fill_manual(values = c("white", "gray")) +
  base_theme +
  theme(legend.position = "none")
rt_cueing_plot

ggsave("./dualmask/figure/dualmask_rt_cueing.png", plot = rt_cueing_plot, width = 4, height = 4, units = "in")

# ------------------------------------------------------------------------------

rt_means_grob <- ggplotGrob(rt_means_plot)
rt_cueing_grob <- ggplotGrob(rt_cueing_plot)

max_width = grid::unit.pmax(rt_means_grob$widths[2:5], rt_cueing_grob$widths[2:5])
rt_means_grob$widths[2:5] <- as.list(max_width)
rt_cueing_grob$widths[2:5] <- as.list(max_width)

dualmask_stacked <- arrangeGrob(rt_means_grob, rt_cueing_grob, nrow = 2,
                                heights = c(10, 6))
dualmask_stacked

png("./dualmask/figure/dualmask_estimates_stacked.png", width = 6.5, height = 8, units = "in", res = 150)
dualmask_stacked
dev.off()

# ------------------------------------------------------------------------------
# Effect of mask on cueing effect in error rates

get_error_estimates <- function(glmod) {
  mask_cond <- data.frame(mask_c = c(-0.5, 0.5))
  y_estimates <- predictSE(glmod, mask_cond, type = "response", se = TRUE)
  estimates <- cbind(mask_cond, y_estimates) %.% 
    plyr::rename(c("se.fit" = "se")) %.%
    mutate(mask_type = ifelse(mask_c == -0.5, "nomask", "mask")) %.%
    select(-mask_c) %.%
    summarize(
      effect_of_mask = fit[mask_type == "nomask"] - fit[mask_type == "mask"],
      ave_se = mean(se)
    ) %.%
    mutate(
      upr = effect_of_mask + (1.96 * ave_se),
      lwr = effect_of_mask - (1.96 * ave_se)
    )
  estimates
}

mod_noise <- glmer(is_error ~ mask_c + (1|subj_id), family = binomial,
                   data = filter(df, cue_type == "noise"))
noise_est <- get_error_estimates(mod_noise) %.% mutate(cue_type = "noise")

mod_invalid <- glmer(is_error ~ mask_c + (1|subj_id), family = binomial,
                     data = filter(df, cue_type == "invalid"))
invalid_est <- get_error_estimates(mod_invalid) %.% mutate(cue_type = "invalid")

mod_valid <- glmer(is_error ~ mask_c + (1|subj_id), family = binomial,
                   data = filter(df, cue_type == "valid"))
valid_est <- get_error_estimates(mod_valid) %.% mutate(cue_type = "valid")

err_estimates <- rbind(noise_est, invalid_est, valid_est) %.%
  mutate(cue_type = factor(cue_type, levels = c("invalid", "noise", "valid")))

err_estimates_plot <- ggplot(err_estimates, aes(x = cue_type, y = effect_of_mask, fill = cue_type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), 
           width = 1.0, color = "black") +
  geom_linerange(aes(ymin = lwr, ymax = upr), 
                 position = position_dodge(width = 0.8)) +
  geom_hline(yintercept = 0) +
  coord_cartesian(ylim = c(-0.03, 0.03), xlim = c(-.5, 4.5)) +
  scale_y_continuous("Effect of Mask on Accuracy", 
                     labels = percent) +
  scale_x_discrete("", labels = c("Invalid", "Noise", "Valid")) +
  base_theme + 
  theme(legend.position = "none")
err_estimates_plot

ggsave("./dualmask/figure/dualmask_err_estimates.png", plot = err_estimates_plot, width = 4, height = 4, units = "in")