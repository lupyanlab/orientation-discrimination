require(lme4)

df <- read.csv("./dualmask/dualmask-final.csv")

# Is there a linear effect of cue on accuracy such that valid > noise > invalid?
acc_cue <- glmer(is_correct ~ cue_l + cue_q + (cue_l + cue_q|subj_id), 
                 data = df, family = "binomial")
acc_cue_l.chisqr <- anova(acc_cue, update(acc_cue, . ~ . - cue_l))
acc_cue_q.chisqr <- anova(acc_cue, update(acc_cue, . ~ . - cue_q))

# Is there an overall effect of the mask on accuracy?
acc_mask <- glmer(is_correct ~ mask_c + (mask_c|subj_id), 
                  data = df, family = "binomial")
acc_mask.chisqr <- anova(acc_mask, update(acc_mask, . ~ . - mask_c))

# Does the effect of the cue on accuracy vary by mask?
acc_inter <- glmer(is_correct ~ cue_l * mask_c + (cue_l * mask_c|subj_id), 
                   data = df, family = "binomial")
acc_inter.chisqr <- anova(acc_inter, update(acc_inter, . ~ . - cue_l:mask_c))

# save model objects
objs <- c("acc_cue", "acc_cue_l.chisqr", "acc_cue_q.chisqr", 
          "acc_mask", "acc_mask.chisqr",
          "acc_inter", "acc_inter.chisqr")
save(list = objs, file = "./dualmask/models-acc.RData")
