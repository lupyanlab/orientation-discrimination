require(dplyr)
require(car)

df <- read.csv("./data/mwp5-2014-04-25.csv")

df <- df %.%
  filter(
    block_ix != -1             # remove practice trials
  ) %.% 
  mutate(
    rt = ifelse(is_correct == 0, NA, rt),
    is_correct = ifelse(response == "timeout", NA, is_correct),
    
    # convert accuracy to error rate
    is_error = ifelse(is_correct == 0, 1, 0),
    
    # recode cue_type for linear models
    cue_l = recode(cue_type, "'valid'=-1/2; 'noise'=0.0; 'invalid'=1/2"),
    cue_q = recode(cue_type, "'valid'=-1/3; 'noise'=2/3; 'invalid'=-1/3"),
    
    # recode mask_type for linear models
    mask_type = ifelse(is_cue_masked == 0, "nomask", 
                       ifelse(is_mask_overlap == 1, "during", "after")),
    mask_c = recode(mask_type, "'nomask'=-1/2; else=1/2"),
    mask_m = recode(mask_type, "'nomask'=-2/3; else=1/3"),
    mask_r = recode(mask_type, "'nomask'=0.0; 'during'=-1/2; 'after'=1/2")
  ) %.%
  select(subj_id, block_ix, trial_ix, 
         cue, cue_type, cue_l, cue_q, 
         mask_type, mask_c, mask_m, mask_r,
         pic, up_pic, 
         response, rt, is_correct, is_error) %.%
  arrange(subj_id, block_ix, trial_ix)

write.csv(df, file = "./temporal/temporal-final.csv", row.names = FALSE)
