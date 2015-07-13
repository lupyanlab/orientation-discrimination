require(dplyr)
require(ggplot2)
require(scales)  # for percent axes

df <- read.csv('./data/mwp2-2014-05-05.csv')

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
    rank_acc = rank(acc, ties.method = "random")
  )

## Response Times

ggplot(subjs, aes(x = rank_rt, y = rt, label = subj_id, color = subj_id)) +
  geom_point() +
  geom_text(hjust = 0, vjust = -0.5, angle = 45, size = 4) +
  coord_cartesian(xlim = c(0.5, 27), ylim = c(300, 800)) +
  scale_x_continuous("Subject (Ranked)", breaks = c(1, seq(5, 25, by = 5))) +
  scale_y_continuous("Average RT (ms)") +
  theme(legend.position = "none")

# ggsave("./figures/dualmask/subj-rt.png", width = 8, height = 5, units = "in")

## Accuracies

ggplot(subjs, aes(x = rank_acc, y = acc, label = subj_id, color = subj_id)) +
  geom_point() +
  geom_text(hjust = 0, vjust = -0.5, angle = 45, size = 4) +
  coord_cartesian(xlim = c(0.5, 28), ylim = c(0.7, 1.05)) +
  scale_x_continuous("Subject (Ranked)", breaks = c(1, seq(5, 25, by = 5))) +
  scale_y_continuous("Accuracy", label = percent) +
  theme(legend.position = "none")

# ggsave("./figures/dualmask/subj-acc.png", width = 8, height = 5, units = "in")

# By Picture

pics <- df %.% 
  group_by(pic) %.%
  summarize(
    obs = length(unique(subj_id)),
    rt = mean(rt, na.rm = T),
    acc = mean(is_correct, na.rm = T)
  ) %.%
  mutate(
    rank_rt = rank(rt, ties.method = "random"),
    rank_acc = rank(acc, ties.method = "random")
  )

## Reaction Times

ggplot(pics, aes(x = rank_rt, y = rt, label = pic, color = pic, size = obs)) +
  geom_point() +
  geom_text(hjust = 0, vjust = -0.8, angle = 45, size = 4) +
  coord_cartesian(xlim = c(0.5, 40), ylim = c(300, 800)) +
  scale_x_continuous("Picture (Ranked)", breaks = c(1, seq(5, 35, by=5), 39)) +
  scale_y_continuous("Average RT (ms)") +
  scale_size_continuous("# Subjects") +
  guides(color = "none") +
  theme(legend.position = c(0.9, 0.2),
        legend.background = element_blank())

# ggsave("./figures/dualmask/pic-rt.png", width = 8, height = 5, units = "in")

## Accuracies

ggplot(pics, aes(x = rank_acc, y = acc, label = pic, color = pic, size = obs)) +
  geom_point() +
  geom_text(hjust = 0, vjust = -0.5, angle = 45, size = 4) +
  coord_cartesian(xlim = c(0.5, 40), ylim = c(0.7, 1.05)) +
  scale_x_continuous("Picture (Ranked)", breaks = c(1, seq(5, 35, by=5), 39)) +
  scale_y_continuous("Accuracy", label = percent) +
  scale_size_continuous("# Subjects") +
  guides(color = "none") +
  theme(legend.position = c(0.9, 0.2),
        legend.background = element_blank())

# ggsave("./figures/dualmask/pic-acc.png", width = 8, height = 5, units = "in")