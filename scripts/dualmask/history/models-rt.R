require(lme4)

df <- read.csv("./dualmask/dualmask-final.csv")

# Is there an overall effect of the mask on RTs?
rt_mask <- lmer(rt ~ mask_c + (mask_c|subj_id), data = df, REML = FALSE)
rt_mask.chisqr <- anova(rt_mask, update(rt_mask, . ~ . - mask_c))

rt_mask.satt <- lmerTest::lmer(rt ~ mask_c + (mask_c|subj_id), data = df)
rt_mask.satt.summ <- lmerTest::summary(rt_mask.satt)
rt_mask.satt.anova <- lmerTest::anova(rt_mask.satt)

# Is the effect of mask moderated by cue type?
rt_inter <- lmer(rt ~ (cue_l + cue_q) * mask_c + (1|subj_id) + (1|pic), data = df, REML = FALSE)
rt_inter.chisqr <- anova(rt_inter, update(rt_inter, . ~ . - cue_l:mask_c))

################################################################################
require(dplyr)
df.agg <- df %.%
  filter(cue_type != "noise") %.%
  group_by(subj_id, cue_type, cue_l, mask_c) %.%
  summarize(
    rt = mean(rt, na.rm = TRUE)
  ) %.% ungroup()

mod <- lmer(rt ~ cue_l * mask_c + (cue_l * mask_c|subj_id), data = df.agg)
summary(mod)

# Is there a linear effect of cue on RT such that valid < noise < invalid?
rt_cue <- lmer(rt ~ cue_l + cue_q + (cue_l + cue_q|subj_id), data = df)
rt_cue_l.chisqr <- anova(rt_cue, update(rt_cue, . ~ . - cue_l))
rt_cue_q.chisqr <- anova(rt_cue, update(rt_cue, . ~ . - cue_q))

rt_cue.satt <- lmerTest::lmer(rt ~ cue_l + cue_q + (cue_l + cue_q|subj_id), data = df)
rt_cue.satt.summ <- lmerTest::summary(rt_cue.satt)
rt_cue.satt.anova <- lmerTest::anova(rt_cue.satt)

# Is there an overall effect of the mask on RTs?
rt_mask <- lmer(rt ~ mask_c + (mask_c|subj_id), data = df)
rt_mask.chisqr <- anova(rt_mask, update(rt_mask, . ~ . - mask_c))

rt_mask.satt <- lmerTest::lmer(rt ~ mask_c + (mask_c|subj_id), data = df)
rt_mask.satt.summ <- lmerTest::summary(rt_mask.satt)
rt_mask.satt.anova <- lmerTest::anova(rt_mask.satt)

# What is the priming effect of the cue on the `no mask` trials?
df.nomask <- subset(df, mask_type == "nomask")
rt_cue.nomask <- lmer(rt ~ cue_l + (cue_l|subj_id), data = df.nomask)

# What is the priming effect of the cue on the `mask` trials?
df.mask <- subset(df, mask_type == "mask")
rt_cue.mask <- lmer(rt ~ cue_l + (cue_l|subj_id), data = df.mask)

# Does the effect of the cue on RTs vary by mask?
rt_inter <- lmer(rt ~ cue_l * mask_c + (cue_l * mask_c|subj_id), data = df)
rt_inter.chisqr <- anova(rt_inter, update(rt_inter, . ~ . - cue_l:mask_c))

rt_inter.satt <- lmerTest::lmer(rt ~ cue_l * mask_c + (cue_l * mask_c|subj_id), data = df)
rt_inter.satt.summ <- lmerTest::summary(rt_inter.satt)
rt_inter.satt.anova <- lmerTest::anova(rt_inter.satt)

objs <- c("rt_cue",   "rt_cue_l.chisqr", "rt_cue_q.chisqr",    "rt_cue.satt.summ",   "rt_cue.satt.anova", 
          "rt_mask",  "rt_mask.chisqr",  "rt_mask.satt.summ",  "rt_mask.satt.anova",
          "rt_cue.nomask", "rt_cue.mask",
          "rt_inter", "rt_inter.chisqr", "rt_inter.satt.summ", "rt_inter.satt.anova")
save(list = objs, file = "./dualmask/models-rt.RData")
