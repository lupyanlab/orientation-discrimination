source("scripts/dualmask_data.R")

library(ggplot2)
library(scales)

subjs <- dualmask %>%
  group_by(subj_id) %>% 
  summarize(
    rt = mean(rt, na.rm = T),
    err = mean(is_error, na.rm = T)
  ) %>% 
  mutate(
    rank_rt = rank(rt, ties.method = "random"),
    rank_err = rank(err, ties.method = "random")
  )

subj_rts <- ggplot(subjs, aes(x = rank_rt, y = rt, label = subj_id, color = subj_id)) +
  geom_point() +
  geom_text(hjust = 0, vjust = -0.5, angle = 45, size = 4) +
  coord_cartesian(xlim = c(0.5, 28), ylim = c(200, 900)) +
  scale_x_continuous("Subject (Ranked By RT)", breaks = c(1, seq(5, 25, by = 5))) +
  scale_y_continuous("Average RT (ms)") +
  theme(legend.position = "none")
subj_rts

subj_err <- ggplot(subjs, aes(x = rank_err, y = err, label = subj_id, color = subj_id)) +
  geom_point() +
  geom_text(hjust = 0, vjust = -0.5, angle = 45, size = 4) +
  coord_cartesian(xlim = c(0.5, 28), ylim = c(0.0, 0.15)) +
  scale_x_continuous("Subject (Ranked By Error Rate)", breaks = c(1, seq(5, 25, by = 5))) +
  scale_y_continuous("Error Rate", label = percent) +
  theme(legend.position = "none")
subj_err

# Notes
# -----
# MWP205a has both the slowest mean RT and the highest error rate.

ggsave("plots/dualmask/descriptives/subj-rts.png", subj_rts, width = 10, height = 6)
ggsave("plots/dualmask/descriptives/subj-err.png", subj_err, width = 10, height = 6)
