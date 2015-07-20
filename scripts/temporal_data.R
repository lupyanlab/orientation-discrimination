options(stringsAsFactors = FALSE)
source("scripts/compile.R")

# check global flag
if (!exists("CLEAR_GLOBAL_ENVIRONMENT")) {
  CLEAR_GLOBAL_ENVIRONMENT <- TRUE
}

temporal <- compile("data/temporal/data", key = "MWP5",
                    headername = "_header.txt")

# Remove practice trials
# ----------------------
temporal <- filter(temporal, block_ix != -1)

# Exclude RTs on incorrect responses and timeout trials
# -----------------------------------------------------
temporal$rt <- with(temporal, ifelse(is_correct == 0, NA, rt))

# Exclude accuracy on timeout trials
# ----------------------------------
temporal$is_correct <- with(temporal, ifelse(response == "timeout", NA, is_correct))

# Create an is_error column
# -------------------------
temporal$is_error <- with(temporal, ifelse(is_correct == 0, 1, 0))

# Recode mask_type
# ----------------
temporal$mask_type <- with(temporal, ifelse(is_cue_masked == 0, "nomask",
                                            ifelse(is_mask_overlap == 1, "during", "after")))

# Put columns in the correct order
# --------------------------------
temporal <- temporal %>%
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
  rm(list = setdiff(ls(), "temporal"))
}