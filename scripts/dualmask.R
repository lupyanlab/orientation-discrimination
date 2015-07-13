

# data manipulation
library(dplyr)
library(reshape2)
library(car)

# plots
library(ggplot2)
library(scales)
library(grid)

# make a data.frame of unique trial types
# used to code factors and to generate model predictions
trial_types <- expand.grid(cue_type = c("valid", "noise", "invalid"),
                           is_cue_masked = c(0, 1))
trial_types$cue_m <- recode(trial_types$cue_type, "'valid' = -1/2; 'noise' = 0; 'invalid' = 1/2", as.factor.result = F)
trial_types$cue_r <- recode(trial_types$cue_type, "'valid' = -1/3; 'noise' = 2/3; 'invalid' = -1/3", as.factor.result = F)
trial_types$mask_c <- recode(trial_types$is_cue_masked, "0 = -1/2; 1 = 1/2", as.factor.result = F)

df <- merge(df, trial_types, by = c("cue_type", "is_cue_masked"))

# df <- read.csv("./data/mwp2-final.csv")
####################################################################################################
# stats
library(lme4)
library(car)
library(AICcmodavg)
library(pbkrtest)
####################################################################################################
# Is there a linear effect of cue_type such that RTs to valid < noise < invalid?
if (FALSE) {
  set.seed(439)
  cue <- lmer(rt ~ cue_m + cue_r + (cue_m + cue_r|subj_id), data = df)
  
  cue_nolinear <- update(cue, . ~ . - cue_m)
  cue_linear_chi <- anova(cue, cue_nolinear)
  cue_linear_anova <- KRmodcomp(cue, cue_nolinear)
  
  cue_noquad <- update(cue, . ~ . - cue_r)
  cue_quad_chi <- anova(cue, cue_noquad)
  cue_quad_anova <- NULL # KRmodcomp(cue, cue_noquad)
  
  save("cue", "cue_nolinear", "cue_linear_chi", "cue_linear_anova",
       "cue_noquad", "cue_quad_chi", "cue_quad_anova",
       file = "./models/dualmask/cue.RData")
}

load("./models/dualmask/cue.RData")

summary(cue)

cue_linear_chi
cue_linear_anova

cue_quad_chi
cue_quad_anova

####################################################################################################
# Does the mask affect RTs?
if (FALSE) {
  set.seed(321)
  mask_rt <- lmer(rt ~ mask_c + (mask_c|subj_id), data = df)
  mask_rt_nomask <- update(mask_rt, . ~ . - mask_c)
  mask_rt_chi <- anova(mask_rt, mask_rt_nomask)
  mask_rt_anova <- KRmodcomp(mask_rt, mask_rt_nomask)
  
  save("mask_rt", "mask_rt_nomask", "mask_rt_chi", "mask_rt_anova",
       file = "./models/dualmask/mask_rt.RData")
}

load("./models/dualmask/mask_rt.RData")

summary(mask_rt)
mask_rt_chi
mask_rt_anova

# Does the mask affect accuracy?
if (FALSE) {
  set.seed(532)
  mask_acc <- glmer(is_correct ~ mask_c + (mask_c|subj_id), family = "binomial", data = df)
  mask_acc_nomask <- update(mask_acc, . ~ . - mask_c)
  mask_acc_chi <- anova(mask_acc, mask_acc_nomask)
  
  save("mask_acc", "mask_acc_nomask", "mask_acc_chi",
       file = "./models/dualmask/mask_acc.RData")
}

load("./models/dualmask/mask_acc.RData")

summary(mask_acc)
mask_acc_chi

####################################################################################################
# Does the effect of the cue type vary by masking condition?

df2 <- df %.% 
  group_by(subj_id) %.% 
  filter(rt > mean(rt, na.rm = T) - sd(rt, na.rm = T), 
         rt < mean(rt, na.rm = T) + sd(rt, na.rm = T))

df3 <- df %.%
  group_by(subj_id) %.%
  mutate(
    baseline = mean(rt[cue_m == 0.0], na.rm = T),
    rt2 = rt - mean(rt[cue_m == 0.0], na.rm = T)
  ) %.%
  filter(cue_m != 0)

if (FALSE) {
  set.seed(683)
  inter_rt <- lmer(rt ~ cue_m * mask_c + (cue_m * mask_c|subj_id), data = df3)
  inter_rt2 <- lmer(rt ~ cue_m * mask_c + (1|subj_id), data = df3)
  inter_rt_nointer <- update(inter_rt, . ~ . - cue_m:mask_c)
  inter_rt_chi <- anova(inter_rt, inter_rt_nointer)
  inter_rt_anova <- NULL # KRmodcomp(inter_rt, inter_rt_nointer)
  
  save("inter_rt", "inter_rt_nointer", "inter_rt_chi", "inter_rt_anova",
       file = "./models/dualmask/inter_rt.RData")
}

load("./models/dualmask/inter_rt.RData")

summary(inter_rt)
inter_rt_chi
inter_rt_anova

if (FALSE) {
  set.seed(112)
  inter_acc <- glmer(is_correct ~ cue_m * mask_c + (cue_m * mask_c|subj_id), family = "binomial", data = df)
  inter_acc_nointer <- update(inter_acc, . ~ . - cue_m:mask_c)
  inter_acc_chi <- anova(inter_acc, inter_acc_nointer)
  
  save("inter_acc", "inter_acc_nointer", "inter_acc_chi",
       file = "./models/dualmask/inter_acc.RData")
}

load("./models/dualmask/inter_acc.RData")

summary(inter_acc)
inter_acc_chi

####################################################################################################
if (FALSE) {
  load("./models/dualmask/inter_rt.RData")
  
  trial_types <- expand.grid(cue_type = c("valid", "noise", "invalid"),
                             is_cue_masked = c(0, 1))
  trial_types$cue_m <- recode(trial_types$cue_type, "'valid' = -1/2; 'noise' = 0; 'invalid' = 1/2", as.factor.result = F)
  trial_types$cue_r <- recode(trial_types$cue_type, "'valid' = -1/3; 'noise' = 2/3; 'invalid' = -1/3", as.factor.result = F)
  trial_types$mask_c <- recode(trial_types$is_cue_masked, "0 = -1/2; 1 = 1/2", as.factor.result = F)
}

y_preds <- predictSE.mer(inter_rt, newdata = trial_types, se.fit = T, type = "response")
preds <- cbind(trial_types, y_preds)
preds$upr <- with(preds, fit + se.fit)
preds$lwr <- with(preds, fit - se.fit)

ggplot(preds, aes(y = fit, ymin = lwr, ymax = upr, x = mask_c, color = cue_type)) +
  geom_pointrange(position = position_dodge(width = -0.1), size = 1.0) +
  geom_line(position = position_dodge(width = -0.1), size = 1.0) +
  coord_cartesian(xlim = c(-1, 1), ylim = c(400, 600)) +
  scale_y_continuous("Response Time (ms)") +
  scale_x_continuous("", breaks = c(-0.5, 0.5), labels = c("No Mask", "Mask")) +
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
        legend.position = c(0.16, 0.88),
        legend.background = element_blank(),
        legend.key = element_blank())

ggsave("figures/dualmask/reactiontime.png", width = 6, height = 6, units = "in")

####################################################################################################
fixed_efs <- as.data.frame(summary(inter_rt)$coefficients)[1:2]
colnames(fixed_efs) <- c("beta", "se")
fixed_efs$param <- row.names(fixed_efs)
row.names(fixed_efs) <- NULL

rand_efs <- coef(inter_rt)$subj_id
rand_efs$subj_id <- row.names(rand_efs)
row.names(rand_efs) <- NULL

rand_efs$r1 <- rank(rand_efs[,1], ties.method = "random")
rand_efs$r2 <- rank(rand_efs[,2], ties.method = "random")
rand_efs$r3 <- rank(rand_efs[,3], ties.method = "random")
rand_efs$r4 <- rank(rand_efs[,4], ties.method = "random")

rand_efs <- melt(rand_efs, measure.vars = c("(Intercept)", "cue_m", "mask_c", "cue_m:mask_c"), 
                 id.vars = c("subj_id", "r1", "r2", "r3", "r4"), variable.name = "param", value.name = "beta")

ggplot(rand_efs, aes(y = beta)) +
  geom_point(aes(x = r4, color = subj_id)) +
  geom_pointrange(aes(x = 12.5, ymin = beta - se, ymax = beta + se), data = fixed_efs, size = 1.2) +
  coord_cartesian(xlim = c(0.5, 25.5)) +
  scale_x_continuous("Subject (Ranked)", breaks = c(1, seq(5, 25, by = 5))) +
  scale_y_continuous("Parameter Estimate") +
  geom_hline(yintercept = 0, lty = 2) +
  facet_grid(param ~ ., scales = "free_y") +
  theme(text = element_text(color = 'black', size = 18),
        line = element_line(color = 'black'),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = 'gray', linetype = 3, size = 0.6),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = 'black'),
        axis.text = element_text(color = 'black'),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_line(color = 'black'),
        axis.ticks.length = unit(8, 'points'),
        legend.position = "none")

ggsave("./figures/dualmask/randomeffects_rt.png", width = 10, height = 9, units = "in")

####################################################################################################
if (FALSE) {
  load("./models/dualmask/inter_acc.RData")
  
  trial_types <- expand.grid(cue_type = c("valid", "noise", "invalid"),
                             is_cue_masked = c(0, 1))
  trial_types$cue_m <- recode(trial_types$cue_type, "'valid' = -1/2; 'noise' = 0; 'invalid' = 1/2", as.factor.result = F)
  trial_types$cue_r <- recode(trial_types$cue_type, "'valid' = -1/3; 'noise' = 2/3; 'invalid' = -1/3", as.factor.result = F)
  trial_types$mask_c <- recode(trial_types$is_cue_masked, "0 = -1/2; 1 = 1/2", as.factor.result = F)
}

y_preds <- predictSE.mer(inter_acc, newdata = trial_types, se.fit = T, type = "response")
preds <- cbind(trial_types, y_preds)
preds$upr <- with(preds, fit + se.fit)
preds$lwr <- with(preds, fit - se.fit)

ggplot(preds, aes(y = fit, ymin = lwr, ymax = upr, x = mask_c, color = cue_type)) +
  geom_pointrange(position = position_dodge(width = -0.1), size = 1.0) +
  geom_line(position = position_dodge(width = -0.1), size = 1.0) +
  coord_cartesian(xlim = c(-1, 1), ylim = c(0.9, 1.0)) +
  scale_y_continuous("Accuracy", label = percent) +
  scale_x_continuous("", breaks = c(-0.5, 0.5), labels = c("No Mask", "Mask")) +
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
        legend.position = c(0.16, 0.88),
        legend.background = element_blank(),
        legend.key = element_blank())

ggsave("figures/dualmask/accuracy.png", width = 6, height = 6, units = "in")

####################################################################################################
fixed_efs <- as.data.frame(summary(inter_acc)$coefficients)[1:2]
colnames(fixed_efs) <- c("beta", "se")
fixed_efs$param <- row.names(fixed_efs)
row.names(fixed_efs) <- NULL

rand_efs <- coef(inter_acc)$subj_id
rand_efs$subj_id <- row.names(rand_efs)
row.names(rand_efs) <- NULL

rand_efs$r1 <- rank(rand_efs[,1], ties.method = "random")
rand_efs$r2 <- rank(rand_efs[,2], ties.method = "random")
rand_efs$r3 <- rank(rand_efs[,3], ties.method = "random")
rand_efs$r4 <- rank(rand_efs[,4], ties.method = "random")

rand_efs <- melt(rand_efs, measure.vars = c("(Intercept)", "cue_m", "mask_c", "cue_m:mask_c"), 
                 id.vars = c("subj_id", "r1", "r2", "r3", "r4"), variable.name = "param", value.name = "beta")

ggplot(rand_efs, aes(y = beta)) +
  geom_point(aes(x = r1, color = subj_id)) +
  geom_pointrange(aes(x = 12.5, ymin = beta - se, ymax = beta + se), data = fixed_efs, size = 1.2) +
  coord_cartesian(xlim = c(0.5, 25.5)) +
  scale_x_continuous("Subject (Ranked)", breaks = c(1, seq(5, 25, by = 5))) +
  scale_y_continuous("Parameter Estimate") +
  geom_hline(yintercept = 0, lty = 2) +
  facet_grid(param ~ ., scales = "free_y") +
  theme(text = element_text(color = 'black', size = 18),
        line = element_line(color = 'black'),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = 'gray', linetype = 3, size = 0.6),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = 'black'),
        axis.text = element_text(color = 'black'),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_line(color = 'black'),
        axis.ticks.length = unit(8, 'points'),
        legend.position = "none")

ggsave("./figures/dualmask/randomeffects_acc.png", width = 10, height = 9, units = "in")
