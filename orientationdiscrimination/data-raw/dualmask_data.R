
# Compile the dualmask data

devtools::load_all()

dualmask <- compile("data-raw/dualmask/data", regex_key = "MWP2",
                     header_file = "_header.txt")

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

devtools::use_data(dualmask)
