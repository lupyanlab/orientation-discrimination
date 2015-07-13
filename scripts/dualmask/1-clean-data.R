require(dplyr)
require(car)

df <- read.csv("./data/mwp2-2014-05-05.csv")

df <- df %.%
  filter(block_ix != -1) %.%   # remove practice trials
  mutate(
    # don't evaluate RTs for incorrect responses
    rt = ifelse(is_correct == 0, NA, rt), 
    
    # don't interpret timeouts as incorrect responses
    is_correct = ifelse(response == "timeout", NA, is_correct),
    
    # convert accuracy to error rate; NAs are maintained
    is_error = ifelse(is_correct == 0, 1, 0),
    
    # recode cue_type (valid, invalid, noise) for linear models
    cue_l = recode(cue_type, "'valid'=-1/2; 'noise'=0; 'invalid'=1/2"),
    cue_q = recode(cue_type, "'valid'=-1/3; 'noise'=2/3; 'invalid'=-1/3"),
    
    # recode mask_type (nomask, mask) for linear models
    mask_type = ifelse(is_cue_masked == 0, "nomask", "mask"),
    mask_c = recode(mask_type, "'nomask'=-0.5; 'mask'=0.5")
  ) %.%
  select(subj_id, block_ix, trial_ix, 
         cue, cue_type, cue_l, cue_q, 
         mask_type, mask_c,
         pic, up_pic, 
         response, rt, is_correct, is_error) %.%
  arrange(subj_id, block_ix, trial_ix)

write.csv(df, file = "./dualmask/dualmask-final.csv", row.names = FALSE)
