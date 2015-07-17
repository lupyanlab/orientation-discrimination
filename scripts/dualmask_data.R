options(stringsAsFactors = FALSE)
source("scripts/compile.R")

# check global flag
if (!exists("CLEAR_GLOBAL_ENVIRONMENT")) {
  CLEAR_GLOBAL_ENVIRONMENT <- TRUE
}

dualmask <- compile("data/dualmask/data", key = "MWP2",
                     headername = "_header.txt")

# Remove practice trials
# ----------------------
dualmask <- filter(dualmask, block_ix != -1)

# Exclude RTs on incorrect responses and timeout trials
# -----------------------------------------------------
dualmask$rt <- with(dualmask, ifelse(is_correct == 0, NA, rt))

# Exclude accuracy on timeout trials
# ----------------------------------
dualmask$is_correct <- with(dualmask, ifelse(response == "timeout", NA, is_correct))

# Create an is_error column
# -------------------------
dualmask$is_error <- with(dualmask, ifelse(is_correct == 0, 1, 0))

# Recode mask_type
# ----------------
dualmask$mask_type <- with(dualmask, ifelse(is_cue_masked == 0, "nomask", "mask"))

# Put columns in the correct order
# --------------------------------
dualmask <- dualmask %>%
  select(subj_id, block_ix, trial_ix,
         cue, cue_type,
         mask_type,
         pic, up_pic,
         response,
         rt,
         is_correct, is_error) %>%
  arrange(subj_id, block_ix, trial_ix)

if (CLEAR_GLOBAL_ENVIRONMENT == TRUE) {
  # Remove unneeded variables
  # -------------------------
  rm(list = setdiff(ls(), "dualmask"))
}