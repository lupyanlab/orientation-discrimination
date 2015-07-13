require(lme4)
require(ggplot2)

df <- read.csv("./dualmask/dualmask-final.csv")
head(df)

df.nona <- df[!is.na(df$rt), ]

# plot just the means
ggplot(df.nona, aes(x = mask_c, y = rt, color = cue_type)) +
  stat_summary(fun.y = mean, geom = "point", size = 3.0)

# fit a model with just the linear interaction
mod1 <- lmer(rt ~ cue_l * mask_c + (1|subj_id) + (1|pic), data = df, REML = FALSE)
summary(mod1)

# model comparison
mod1.chisqr <- anova(mod1, update(mod1, . ~ . - cue_l:mask_c))
mod1.chisqr

ggplot(df.nona, aes(x = mask_c, y = rt, color = cue_type)) +
  stat_summary(fun.y = mean, geom = "point", size = 3.0) +
  stat_summary(aes(y = fitted(mod1)), fun.data = mean_se, geom = "line")

# fit a model with both the linear and the quadratic terms
mod2 <- lmer(rt ~ (cue_l + cue_q) * mask_c + (1|subj_id) + (1|pic), data = df.nona, REML = FALSE)
summary(mod2)

# model comparison
mod2.chisqr <- anova(mod2, update(mod2, . ~ . - cue_l:mask_c))
mod2.chisqr

ggplot(df.nona, aes(x = mask_c, y = rt, color = cue_type)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(aes(y = fitted(mod1)), fun.data = mean_se, geom = "line") +
  stat_summary(aes(y = fitted(mod2)), fun.data = mean_se, geom = "line", linetype = 2)

library(AICcmodavg)

x_preds <- unique(df.nona[,c("cue_type", "mask_c", "cue_l", "cue_q")])
row.names(x_preds) <- NULL

y_preds <- predictSE.mer(mod2, x_preds, type = "response", se = TRUE)
preds <- cbind(x_preds, y_preds)

library(dplyr)
raw_means <- df.nona %.% 
  group_by(cue_type, mask_c) %.%
  summarize(rt = mean(rt, na.rm = T))

preds$lwr <- with(preds, fit - se.fit)
preds$upr <- with(preds, fit + se.fit)
preds <- plyr::rename(preds, c("fit" = "rt"))

preds_new <- merge(preds[,c("cue_type", "mask_c", "se.fit")], raw_means)
ggplot(preds_new, aes(x = mask_c, y = rt, color = cue_type, ymin = rt - se.fit, ymax = rt + se.fit)) +
  geom_errorbar(width = 0.2) +
  geom_line()


ggplot(df.nona) + 
  stat_summary(aes(x = mask_c, y = rt, color = cue_type), fun.y = mean, geom = "line") +
  geom_errorbar(data = preds, aes(x = mask_c, y = rt, color = cue_type, ymin = lwr, ymax = upr), width = 0.1)

