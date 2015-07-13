library(dplyr)
library(lme4)
library(AICcmodavg)
library(ggplot2)
library(grid)
library(gridExtra)
library(scales)

source("./helper/within-subj-error.R")
source("./figure/base_theme.R")
source("./helper/helper-funcs.R")

df <- read.csv("./temporal/temporal-final.csv", stringsAsFactors = FALSE)
df <- filter(df, subj_id %nin% c("MWP502", "MWP524", "MWP518"))

# ------------------------------------------------------------------------------
# RT means

mod <- lmer(rt ~ (cue_l + cue_q) * (mask_m + mask_r) + (1|subj_id), data = df)
x_preds <- unique(df[,c("cue_type", "cue_l", "cue_q", "mask_type", "mask_m", "mask_r")])
y_preds <- predictSE.mer(mod, x_preds, type = "response", se.fit = TRUE)
rt_mean_se <- cbind(x_preds, y_preds) %.%
  mutate(
    cue_type = factor(cue_type, levels = c("invalid", "noise", "valid")),
    mask_type = factor(mask_type, levels = c("nomask", "overlap", "separate"))
  ) %.%
  select(cue_type, mask_type, rt = fit, mean_se = se.fit) %.%
  arrange(mask_type, cue_type)

rt_diff_se <- df %.%
  summarySEwithin(measurevar = "rt", na.rm = TRUE, idvar = "subj_id",
                  withinvars = c("mask_type", "cue_type")) %.%
  mutate(
    cue_type = factor(cue_type, levels = c("invalid", "noise", "valid")),
    mask_type = factor(mask_type, levels = c("nomask", "overlap", "separate"))
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
  coord_cartesian(ylim = c(425, 535), xlim = c(0.4, 3.6)) +
  scale_y_continuous("Reaction Time (ms)") +
  scale_x_discrete("", labels = c("No Interference", "Interference During Cue", "Interference After Cue")) +
  scale_color_discrete("Auditory Cue", labels = c("Invalid", "Noise", "Valid")) +
  base_theme + theme(
    legend.position = c(0.92, 0.62)
  )
rt_means_plot

ggsave("./temporal/figure/temporal_rt_means.png", plot = rt_means_plot, width = 8.5, height = 4, units = "in")

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

df_overlap <- filter(df, mask_type == "overlap")
mod_overlap_rt <- lmer(rt ~ cue_l + cue_q + (1|subj_id), data = df_overlap)
overlap_est <- get_rt_estimates(mod_overlap_rt) %.% mutate(mask_type = "overlap")

df_separate <- filter(df, mask_type == "separate")
mod_separate_rt <- lmer(rt ~ cue_l + cue_q + (1|subj_id), data = df_separate)
separate_est <- get_rt_estimates(mod_separate_rt) %.% mutate(mask_type = "separate")

rt_estimates <- rbind(nomask_est, overlap_est, separate_est) %.%
  mutate(mask_type = factor(mask_type, levels = c("nomask", "overlap", "separate"))) %.%
  plyr::rename(c("Estimate" = "cue_effect", "2.5 %" = "lwr", "97.5 %" = "upr")) %.%
  filter(param == "cue_l") %.%
  select(cue_effect, lwr, upr, mask_type)

rt_cueing_plot <- ggplot(rt_estimates, aes(x = mask_type, y = cue_effect)) + 
  geom_bar(aes(fill = mask_type), stat = "identity", width = 0.4, color = "black") +
  geom_linerange(aes(ymin = lwr, ymax = upr), size = 0.6) + 
  coord_cartesian(ylim = c(0, 80), xlim = c(0.4, 3.6)) +
  scale_x_discrete("", labels = c("No Interference", "During Cue", "After Cue")) +
  scale_y_continuous("Cueing Effect (ms)", breaks = seq(0, 100, by=20)) +
  scale_fill_manual(values = c("white", "gray", "gray")) +
  base_theme +
  theme(legend.position = "none")
rt_cueing_plot

ggsave("./temporal/figure/temporal_rt_cueing.png", plot = rt_cueing_plot, width = 6, height = 4, units = "in")

# ------------------------------------------------------------------------------

rt_means_grob <- ggplotGrob(rt_means_plot)
rt_cueing_grob <- ggplotGrob(rt_cueing_plot)

max_width = grid::unit.pmax(rt_means_grob$widths[2:5], rt_cueing_grob$widths[2:5])
rt_means_grob$widths[2:5] <- as.list(max_width)
rt_cueing_grob$widths[2:5] <- as.list(max_width)

temporal_stacked <- arrangeGrob(rt_means_grob, rt_cueing_grob, nrow = 2,
                                heights = c(10, 6))
temporal_stacked

png("./temporal/figure/temporal_estimates_stacked.png", width = 10.5, height = 8, units = "in", res = 150)
temporal_stacked
dev.off()
