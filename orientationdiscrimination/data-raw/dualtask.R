library(devtools)
library(dplyr)

source("R/compile.R")

make_unilateral <- function(overwrite = FALSE) {
  unilateral_dir <- "data-raw/dualtask/unilateral/"

  make_unilateral_version <- function(version) {
    regex_keys <- list("MOW1", "MOW3")
    compile(unilateral_dir, regex_keys[[version]]) %>%
      clean %>% recode %>% mutate(version = version)
  }

  unilateral <- plyr::rbind.fill(
    make_unilateral_version(1),
    make_unilateral_version(2)
  )

  use_data(unilateral, overwrite = overwrite)
}

make_bilateral <- function(overwrite = FALSE) {
  bilateral <- compile("data-raw/dualtask/bilateral/") %>%
    clean %>% recode

  use_data(bilateral, overwrite = overwrite)
}


clean <- function(frame) {
  frame %>%
    filter(
      # Remove practice trials
      block_type != "practice",

      # Drop weird RTs
      rt < 2000
    ) %>%
    mutate(
      # Drop RT on incorrect response trials
      rt = ifelse(is_correct, rt, NA),

      # Drop accuracy on timeout trials
      is_correct = ifelse(response == "timeout", NA, is_correct),

      # Create is_error from is_correct
      is_error = ifelse(is_correct, 0, 1)
    )
}


#' Recode all variables for this experiment.
#'
#' @param frame data.frame to recode.
#' @importFrom dplyr `%>%`
#' @export
recode <- function(frame) {
  frame %>%
    recode_cue_type %>%
    recode_mask_type %>%
    recode_response_type %>%
    recode_error_type
}

recode_cue_type <- function(frame) {
  cue_type_map <- dplyr::data_frame(
    cue_type = c("invalid", "valid", "", NA),
    cue_c = c(-0.5, 0.5, NA, NA),
    cue_task = c("invalid", "valid", "word", "word")
  )
  try(frame <- dplyr::left_join(frame, cue_type_map))

  if ("response_type" %in% colnames(frame)) {
    # Relabel invalid/valid as word for unilateral experiments
    unilateral_word_trials <- with(frame,
                                   (response_type == "word") & (cue_type %in% c("valid", "invalid"))
    )

    frame[unilateral_word_trials, "cue_task"] <- "word"

    unilateral_levels = c("invalid", "valid", "word")
    frame$cue_task <- factor(frame$cue_task, levels = unilateral_levels)
  }

  frame
}

recode_mask_type <- function(frame) {
  mask_type_map <- dplyr::data_frame(
    mask_type = c("nomask", "mask"),
    mask_c = c(-0.5, 0.5)
  )
  try(frame <- dplyr::left_join(frame, mask_type_map))
  frame
}

recode_response_type <- function(frame) {
  response_type_map <- dplyr::data_frame(
    response_type = c("pic", "word"),
    response_label = c("Upright picture", "Verify word"),
    response_c = c(-0.5, 0.5)
  )
  try(frame <- dplyr::left_join(frame, response_type_map))
  frame
}

recode_error_type <- function(frame) {
  all_responses <- c("left", "right", "same", "different", "up", "down")

  pic_responses <- expand.grid(
    response_type = "pic",
    response = all_responses,
    correct_response = c("left", "right"),
    stringsAsFactors = FALSE
  ) %>% mutate(
    error_type = ifelse(
      response == correct_response, NA,
      ifelse(!(response %in% c("left", "right")), "wrong_type", "wrong_key")
    )
  )

  word_responses <- expand.grid(
    response_type = "word",
    response = all_responses,
    correct_response = all_responses,
    stringsAsFactors = FALSE
  ) %>% mutate(
    error_type = ifelse(
      response == correct_response, NA,
      ifelse(
        # bilateral version: only wrong key errors
        correct_response %in% c("left", "right"), "wrong_key",
        ifelse(
          # unilateral version: wrong key or wrong type errors
          response %in% c("left", "right"), "wrong_type", "wrong_key"
        )
      )
    )
  )

  error_type_map <- rbind(pic_responses, word_responses)

  try(frame <- dplyr::left_join(frame, error_type_map))

  # Process keys pressed, if available
  try({
    # Determine if more than 1 key was pressed
    multiple_keys <- stringr::str_split(frame$keys_pressed, " ") %>%
      lapply(function(x) length(x) > 1) %>% unlist

    frame[multiple_keys, "error_type"] <- "wrong_type"
  })

  frame
}

