require(dplyr)
require(car)
source("./helper/helper-funcs.R") # import `%nin%` function for filtering

df <- read.csv("./data/mwp2-2014-05-05.csv")

invalid_subjs <- c("MWP205a")  # slow response times and < 90% accuracy

df <- df %.%
  mutate(
    mask_type = ifelse(is_cue_masked == 0, "nomask", "mask"),
    rt = ifelse(is_correct == 0, NA, rt),
    is_correct = ifelse(response == "timeout", NA, is_correct)
  ) %.%
  filter(
    block_ix != -1,             # remove practice trials
    subj_id %nin% invalid_subjs # `%nin%` is the opposite of `%in%`
  ) %.% 
  select(subj_id, block_ix, trial_ix, 
         cue, cue_type, mask_type, 
         pic, up_pic, 
         response, rt, is_correct) %.%
  arrange(subj_id, block_ix, trial_ix)

df$cue_L <- recode(df$cue_type, "'valid'=-0.5; 'noise'=0.0; 'invalid'=0.5")
df$cue_Q <- recode(df$cue_type, "'valid'=-0.333; 'noise'=0.667; 'invalid'=-0.333")
df$mask_C <- recode(df$mask_type, "'nomask'=-0.5; 'mask'=0.5")

write.csv(df, file = "./dualmask/dualmask.csv", row.names = FALSE)
