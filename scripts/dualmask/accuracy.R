require(lme4)

df <- read.csv("./data/dualmask.csv")

# Is there a linear effect of cue on accuracy such that valid > noise > invalid?
acc.cue <- glmer(is_correct ~ cue_L + cue_Q + (cue_L + cue_Q|subj_id), 
                 data = df, family = "binomial")
acc.cue_L.chisqr <- anova(acc.cue, update(acc.cue, . ~ . - cue_L))
acc.cue_Q.chisqr <- anova(acc.cue, update(acc.cue, . ~ . - cue_Q))

# Is there an overall effect of the mask on accuracy?
acc.mask <- glmer(is_correct ~ mask_C + (mask_C|subj_id), 
                  data = df, family = "binomial")
acc.mask.chisqr <- anova(acc.mask, update(acc.mask, . ~ . - mask_C))

# Does the effect of the cue on accuracy vary by mask?
acc.inter <- glmer(is_correct ~ cue_L * mask_C + (cue_L * mask_C|subj_id), 
                   data = df, family = "binomial")
acc.inter.chisqr <- anova(acc.inter, update(acc.inter, . ~ . - cue_L:mask_C))

# objs <- ls()[ls() != "df"]
# save(list = objs, file = "./dualmask/models-acc.RData")
