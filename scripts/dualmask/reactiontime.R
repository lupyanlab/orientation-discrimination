require(lme4)

df <- read.csv("./data/dualmask.csv")

# Is there a linear effect of cue on RT such that valid < noise < invalid?
rt.cue <- lmer(rt ~ cue_L + cue_Q + (cue_L + cue_Q|subj_id), data = df)
rt.cue_L.chisqr <- anova(rt.cue, update(rt.cue, . ~ . - cue_L))
rt.cue_Q.chisqr <- anova(rt.cue, update(rt.cue, . ~ . - cue_Q))

# Is there an overall effect of the mask on RTs?
rt.mask <- lmer(rt ~ mask_C + (mask_C|subj_id), data = df)
rt.mask.chisqr <- anova(rt.mask, update(rt.mask, . ~ . - mask_C))

# Does the effect of the cue on RTs vary by mask?
rt.inter <- lmer(rt ~ cue_L * mask_C + (cue_L * mask_C|subj_id), data = df)
rt.inter.chisqr <- anova(rt.inter, update(rt.inter, . ~ . - cue_L:mask_C))

# objs <- ls()[ls() != "df"]
# save(list = objs, file = "./dualmask/models-rt.RData")
