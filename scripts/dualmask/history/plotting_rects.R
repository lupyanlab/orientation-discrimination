cue <- df %.% 
  filter(cue_type != "noise") %.%
  group_by(subj_id, cue_l, mask_c) %.% 
  summarize(rt = mean(rt, na.rm = TRUE))

cue.rt <- lmer(rt ~ cue_l * mask_c + (cue_l * mask_c|subj_id), data = cue, REML = FALSE)

x_preds <- expand.grid(cue_l = c(-0.5, 0.5), mask_c = c(-0.5, 0.5))
y_preds <- predictSE.mer(cue.rt, x_preds, type = "response", se = TRUE)
preds <- cbind(x_preds, y_preds)

means <- df %.% group_by(cue_type, mask_type, cue_l, mask_c) %.% summarize(rt = mean(rt, na.rm = TRUE))
preds <- merge(preds, means, all.y = TRUE)

# get SE around noise baseline trials
x_preds.noise <- data.frame(mask_c = c(-0.5, 0.5))
y_preds.noise <- predictSE.mer(noise.rt, x_preds.noise, type = "response", se = TRUE)
preds.noise <- cbind(x_preds.noise, y_preds.noise)

compute_range <- function(rts) {
  # select the row of preds that has the same mean rt
  chunk <- merge(preds, data.frame(rt = rts), all.y = TRUE)
  
  # select the row of preds that has the baseline rt
  baseline <- preds[preds$mask_c == chunk$mask_c[1] & preds$cue_type == "noise", ]
  
  if(chunk$cue_type[1] == "noise") {
    rect_aes <- c("xmin" = chunk$mask_c[1] - 0.05,
                  "xmax" = chunk$mask_c[1] + 0.05,
                  "ymin" = chunk$rt - 0.1,
                  "ymax" = chunk$rt + 0.1)
  } else {
    rect_aes <- c("xmin" = chunk$mask_c[1] - 0.05,
                  "xmax" = chunk$mask_c[1] + 0.05,
                  "ymin" = min(c(chunk$rt,baseline$rt)),
                  "ymax" = max(c(chunk$rt,baseline$rt)))
  rect_aes
  }
}

ggplot(preds, aes(x = mask_c, y = rt)) +
  stat_summary(data = preds, aes(fill = cue_type), fun.data = compute_range, geom = "rect", alpha = 0.5) +
  geom_pointrange(aes(ymin = fit - se.fit, ymax = fit + se.fit, color = factor(cue_type)), position = position_dodge(width = 0.0), size = 0.8) +
  coord_cartesian(xlim = c(-1, 1)) +
  scale_color_manual("Cue Type", labels = c("Invalid", "Noise", "Valid"), values = c("#66c2a5", "black", "#8da0cb")) +
  scale_fill_manual("", values = c("#66c2a5", "black", "#8da0cb")) +
  guides(fill = "none") +
  scale_x_continuous("Mask Condition", breaks = c(-0.5, 0.5), labels = c("No Mask", "Mask")) +
  scale_y_continuous("Reaction Time (ms)", breaks = seq(400,600,by=10)) +
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
        legend.position = c(0.9, 0.9),
        legend.direction = "vertical",
        legend.background = element_blank(),
        legend.key = element_blank())