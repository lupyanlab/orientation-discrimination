
#' @import dplyr
#' @export
recode_mask_type <- function(frame) {
  mask_type_map <- data_frame(
    mask_type = c("nomask", "mask"),
    mask_c = c(-0.5, 0.5)
  )

  # Temporal experiment
  temporal_mask_type_map <- data_frame(
    mask_type = c("nomask", "during", "after"),
    mask_c = c(-0.5, 0.5, 0.5),
    mask_m = c(-2/3, 1/3, 1/3),
    mask_r = c(0, 1/2, -1/2)
  )

  # Modality experiment
  modality_mask_type_map <- data_frame(
    mask_type = c("nomask", "visual", "auditory"),
    mask_c = c(-0.5, 0.5, NA),
    visual_v_nomask = c(0, 1, 0),
    auditory_v_nomask = c(0, 0, 1)
  )

  all_mask_type_map <- mask_type_map %>%
    merge(temporal_mask_type_map, all = TRUE) %>%
    merge(modality_mask_type_map, all = TRUE)

  frame %>% left_join(all_mask_type_map)
}

#' @import dplyr
#' @export
recode_cue_type <- function(frame) {
  cue_type_map <- data_frame(
    cue_type = c("valid", "invalid", "noise"),
    cue_c = c(-1/2, 1/2, NA),
    cue_l = c(-1/2, 1/2, 0),
    cue_q = c(-1/3, -1/3, 2/3),

    # Treatment contrasts
    valid_v_baseline = c(1, 0, 0),
    invalid_v_baseline = c(0, 1, 0)
  )

  # For nocue experiments, copy noise recodes
  cue_type_map[4, ] <- cue_type_map[3, ]
  cue_type_map[4, "cue_type"] <- "nocue"

  # Merge map and return
  frame %>% left_join(cue_type_map)
}
