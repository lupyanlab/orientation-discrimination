require(ggplot2)
require(grid)
require(AICcmodavg)
require(lme4)
require(plyr)
library(dplyr)
source("./helper/within-subj-error.R")

df <- read.csv("./dualmask/dualmask.csv")
df$cue_type <- factor(df$cue_type, levels = c("valid", "noise", "invalid"))
df$mask_type <- factor(df$mask_type, levels = c("nomask", "mask"))
df <- summarySEwithin(df, measurevar = "rt", idvar = "subj_id", na.rm = T,
                      withinvars = c("cue_type", "mask_type"))
df$lwr <- with(df, rt - se)
df$upr <- with(df, rt + se)

ggplot(df, aes(y = rt, ymin = lwr, ymax = upr, x = mask_type, color = cue_type)) +
  geom_pointrange(position = position_dodge(width = 0.1), size = 1.0) +
  geom_line(aes(group = cue_type), position = position_dodge(width = 0.1), size = 1.0) +
  coord_cartesian(ylim = c(400, 600)) +
  scale_y_continuous("Response Time (ms)") +
  scale_x_discrete("", labels = c("No Mask", "Mask")) +
  scale_color_discrete("Cue Type", labels = c("Valid", "Noise", "Invalid")) +
  guides(color = guide_legend(reverse = T)) +
  theme(text = element_text(color = 'black', size = 18),
        line = element_line(color = 'black'),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color = 'gray', linetype = 3, size = 0.6),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = 'black'),
        axis.text = element_text(color = 'black'),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_line(color = 'black'),
        axis.ticks.length = unit(8, 'points'),
        legend.position = "right",
        legend.direction = "vertical",
        legend.background = element_blank(),
        legend.key = element_blank()) +
  ggtitle("Raw RTs")

ggsave("./figures/dualmask/reactiontime-raw.png", width=6, height=6, units="in")


load("./dualmask/models-rt.RData")

x_preds <- expand.grid(cue_L = c(-0.5, 0.0, 0.5),
                       mask_C = c(-0.5, 0.5))
y_preds <- predictSE.mer(rt.inter, x_preds, type = "response", se = T)
preds <- cbind(x_preds, y_preds)
preds <- rename(preds, c("fit" = "rt"))
preds$cue_type <- factor(preds$cue_L, levels = c(-0.5, 0.0, 0.5), 
                         labels = c("valid", "noise", "invalid"))
preds$mask_type <- factor(preds$mask_C, levels = c(-0.5, 0.5),
                          labels = c("nomask", "mask"))
preds$lwr <- with(preds, rt - se.fit)
preds$upr <- with(preds, rt + se.fit)

ggplot(preds, aes(y = rt, ymin = lwr, ymax = upr, x = mask_type, color = cue_type)) +
  geom_pointrange(position = position_dodge(width = 0.1), size = 1.0) +
  geom_line(aes(group = cue_type), position = position_dodge(width = 0.1), size = 1.0) +
  coord_cartesian(ylim = c(400, 600)) +
  scale_y_continuous("Response Time (ms)") +
  scale_x_discrete("", labels = c("No Mask", "Mask")) +
  scale_color_discrete("Cue Type", labels = c("Valid", "Noise", "Invalid")) +
  guides(color = guide_legend(reverse = T)) +
  theme(text = element_text(color = 'black', size = 18),
        line = element_line(color = 'black'),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color = 'gray', linetype = 3, size = 0.6),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = 'black'),
        axis.text = element_text(color = 'black'),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_line(color = 'black'),
        axis.ticks.length = unit(8, 'points'),
        legend.position = "right",
        legend.direction = "vertical",
        legend.background = element_blank(),
        legend.key = element_blank()) +
  ggtitle("Fitted RTs")

ggsave("./figures/dualmask/reactiontime-fitted.png", width=6, height=6, units="in")

preds2 <- preds %.%
  group_by(mask_type) %.%
  mutate(
    rt = rt - rt[cue_type == "noise"]
  ) %.%
  filter(cue_type != "noise")

preds2$lwr <- with(preds2, rt - se.fit)
preds2$upr <- with(preds2, rt + se.fit)

ggplot(preds2, aes(y = rt, ymin = lwr, ymax = upr, x = mask_type, fill = cue_type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.0), color = "black") +
  geom_errorbar(position = position_dodge(width = -0.05), width = 0.0) +
  coord_cartesian(ylim = c(-50, 50)) +
  scale_y_continuous("Response Time Relative To Baseline (ms)") +
  scale_x_discrete("", labels = c("No Mask", "Mask")) +
  scale_fill_discrete("Cue Type", labels = c("Valid", "Invalid")) +
  guides(fill = guide_legend(reverse = T)) +
  theme(text = element_text(color = 'black', size = 18),
        line = element_line(color = 'black'),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color = 'gray', linetype = 3, size = 0.6),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = 'black'),
        axis.text = element_text(color = 'black'),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_line(color = 'black'),
        axis.ticks.length = unit(8, 'points'),
        legend.position = "right",
        legend.direction = "vertical",
        legend.background = element_blank(),
        legend.key = element_blank()) +
  ggtitle("Priming Effect")

ggsave("./figures/dualmask/reactiontime-priming.png", width=6, height=6, units="in")
