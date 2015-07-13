library(lme4)
library(dplyr)
library(reshape2)
library(ggplot2)

df <- read.csv("dualmask/dualmask-final.csv")

# Cue effect on the no make trials
df.nomask <- filter(df, df$mask_type == "nomask")

df.nomask %.% 
  group_by(cue_type) %.%
  summarize(rt = mean(rt, na.rm = TRUE)) %.%
  mutate(round(rt, 0))

mod.nomask.rt <- lmer(rt ~ cue_l + cue_q + (cue_l + cue_q|subj_id), data = df, 
                      REML = FALSE)
anova(mod.nomask.rt, update(mod.nomask.rt, . ~ . - cue_l))

# Effect of mask on noise trials
df.noise <- filter(df, df$cue_type == "noise")

df.noise %.%
  group_by(mask_type) %.%
  summarize(rt = mean(rt, na.rm = TRUE)) %.%
  mutate(rt = round(rt, 0))

mod.noise.rt <- lmer(rt ~ mask_c + (mask_c|subj_id), data = df.noise, REML = FALSE)
anova(mod.noise.rt, update(mod.noise.rt, . ~ . - mask_c))

# Effect of the mask on cued trials
df.cue <- filter(df, df$cue_type != "noise")

df.cue %.%
  group_by(cue_type, mask_type) %.%
  summarize(rt = mean(rt, na.rm = TRUE)) %.%
  dcast(mask_type ~ cue_type, value.var = "rt") %.%
  mutate(prime = invalid - valid)

mod.cue.rt <- lmer(rt ~ cue_l * mask_c + (cue_l * mask_c|subj_id), data = df.cue, 
                   REML = FALSE, control=lmerControl(optCtrl=list(maxfun=20000)))
summary(mod.cue.rt)
anova(mod.cue.rt, update(mod.cue.rt, . ~ . - cue_l:mask_c))

df.bysubj <- df %.%
  group_by(subj_id, cue_type, mask_type) %.%
  summarize(rt = mean(rt, na.rm = TRUE)) %.%
  ungroup() %.% group_by(subj_id, mask_type) %.%
  mutate(rt_diff = rt - rt[cue_type == "noise"]) %.%
  ungroup() %.%
  filter(cue_type != "noise") %.%
  mutate(type = paste(subj_id, cue_type, sep=":"))

ggplot(df.bysubj, aes(x = mask_type, y = rt_diff, linetype = cue_type)) +
  geom_line(aes(group = type, color = subj_id))

