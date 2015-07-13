require(lme4)
require(ggplot2)

df <- read.csv("./dualmask/dualmask-final.csv")
head(df)

df.nona <- df[!is.na(df$is_correct), ]

# plot just the means
ggplot(df.nona, aes(x = mask_c, y = is_correct, color = cue_type)) +
  stat_summary(fun.y = mean, geom = "point", size = 3.0)

# fit a model with just the linear interaction
mod1 <- glmer(is_correct ~ cue_l * mask_c + (cue_l * mask_c|subj_id), data = df, family = binomial)
summary(mod1)

ggplot(df.nona, aes(x = mask_c, y = is_correct, color = cue_type)) +
  stat_summary(fun.y = mean, geom = "point", size = 3.0) +
  stat_summary(aes(y = fitted(mod1)), fun.data = mean_se, geom = "line")

# fit a model with both the linear and the quadratic terms
mod2 <- glmer(is_correct ~ (cue_l + cue_q) * mask_c + ((cue_l + cue_q) * mask_c|subj_id), data = df, family = binomial)
summary(mod2)

ggplot(df.nona, aes(x = mask_c, y = is_correct, color = cue_type)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(aes(y = fitted(mod1)), fun.data = mean_se, geom = "line") +
  stat_summary(aes(y = fitted(mod2)), fun.data = mean_se, geom = "line", linetype = 2)

library(AICcmodavg)

