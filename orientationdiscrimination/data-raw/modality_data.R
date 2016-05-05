
# Compile the modality data

devtools::load_all()

modality <- compile("data-raw/modality/data", regex_key = "ODM",
                    header_file = "_header.txt")

# Remove practice trials
# ----------------------
modality <- filter(modality, block_ix != -1)

# Exclude RTs on incorrect responses and timeout trials
# -----------------------------------------------------
modality$rt <- with(modality, ifelse(is_correct == 0, NA, rt))

# Exclude accuracy on timeout trials
# ----------------------------------
modality$is_correct <- with(modality, ifelse(response == "timeout", NA, is_correct))

# Create an is_error column
# -------------------------
modality$is_error <- with(modality, ifelse(is_correct == 0, 1, 0))

# Put columns in the correct order
# --------------------------------
modality <- modality %>%
  select(subj_id, block_ix, trial_ix,
         cue, cue_type,
         mask_type,
         pic, up_pic,
         response,
         rt,
         is_correct, is_error) %>%
  arrange(subj_id, block_ix, trial_ix)

devtools::use_data(modality)
