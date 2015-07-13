require(dplyr)
require(ggplot2)
require(scales)    # for percent axes
require(gridExtra) # arrange two ggplots

df <- read.csv("./data/mwp5-2014-04-25.csv")

# By Subject

subjs <- df %.% 
  group_by(subj_id) %.% 
  summarize(
    obs = length(rt),
    rt = mean(rt, na.rm = T),
    acc = mean(is_correct, na.rm = T)
  ) %.% 
  mutate(
    rank_rt = rank(rt, ties.method = "random"),
    rank_acc = rank(-acc, ties.method = "random")
  )

## Response Times

rts <- ggplot(subjs, aes(x = rank_rt, y = rt, label = subj_id, color = subj_id)) +
  geom_point() +
  geom_text(hjust = 0, vjust = -0.5, angle = 45, size = 4) +
  coord_cartesian(xlim = c(0.5, 39), ylim = c(300, 860)) +
  scale_x_continuous("Subject (Ranked)", breaks = c(1, seq(5, 40, by = 5))) +
  scale_y_continuous("Average RT (ms)") +
  theme(legend.position = "none")

## Accuracies

accs <- ggplot(subjs, aes(x = rank_acc, y = acc, label = subj_id, color = subj_id)) +
  geom_point() +
  geom_text(hjust = 0, vjust = -0.5, angle = 45, size = 4) +
  coord_cartesian(xlim = c(0.5, 39), ylim = c(0.7, 1.12)) +
  scale_x_continuous("Subject (Ranked)", breaks = c(1, seq(5, 40, by = 5))) +
  scale_y_continuous("Accuracy", label = percent, breaks = seq(0,1,by=0.1)) +
  theme(legend.position = "none")

(outliers <- arrangeGrob(rts, accs, nrow = 2))

ggsave("./temporal/1-outliers.png", plot = outliers, width = 12, height = 8, units = "in")
