
# Compile the modality_blocked data

devtools::load_all()

modality_blocked <- compile("data-raw/modality-blocked/data", regex_key = "ODM",
                            header_file = "_header.txt")

# Remove practice trials
# ----------------------
modality_blocked <- filter(modality_blocked, block_ix != -1)

# Exclude RTs on incorrect responses and timeout trials
# -----------------------------------------------------
modality_blocked$rt <- with(modality_blocked, ifelse(is_correct == 0, NA, rt))

# Exclude accuracy on timeout trials
# ----------------------------------
modality_blocked$is_correct <- with(modality_blocked, ifelse(response == "timeout", NA, is_correct))

# Create an is_error column
# -------------------------
modality_blocked$is_error <- with(modality_blocked, ifelse(is_correct == 0, 1, 0))

# Put columns in the correct order
# --------------------------------
modality_blocked <- modality_blocked %>%
  select(subj_id, block_ix, trial_ix,
         cue, cue_type,
         mask_type,
         pic, up_pic,
         response,
         rt,
         is_correct, is_error) %>%
  arrange(subj_id, block_ix, trial_ix)

devtools::use_data(modality_blocked)
