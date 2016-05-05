
# Compile the modality data

devtools::load_all()

modality_noise <- compile("data-raw/modality-noise/data", regex_key = "ODM",
                          header_file = "_header.txt")

# Remove practice trials
# ----------------------
modality_noise <- filter(modality_noise, block_ix != -1)

# Exclude RTs on incorrect responses and timeout trials
# -----------------------------------------------------
modality_noise$rt <- with(modality_noise, ifelse(is_correct == 0, NA, rt))

# Exclude accuracy on timeout trials
# ----------------------------------
modality_noise$is_correct <- with(modality_noise, ifelse(response == "timeout", NA, is_correct))

# Create an is_error column
# -------------------------
modality_noise$is_error <- with(modality_noise, ifelse(is_correct == 0, 1, 0))

# Put columns in the correct order
# --------------------------------
modality_noise <- modality_noise %>%
  select(subj_id, block_ix, trial_ix,
         cue, cue_type,
         mask_type,
         pic, up_pic,
         response,
         rt,
         is_correct, is_error) %>%
  arrange(subj_id, block_ix, trial_ix)

devtools::use_data(modality_noise)
