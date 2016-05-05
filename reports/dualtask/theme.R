scale_x_mask <- ggplot2::scale_x_continuous("", breaks = c(-0.5, 0.5),
                                            labels = c("No mask", "Mask"))

scale_y_rt <- ggplot2::scale_y_continuous("Reaction Time")
scale_y_error <- ggplot2::scale_y_continuous("Error Rate", labels = scales::percent)

cue_colors <- list(invalid = "#d7191c", valid = "#a6d96a")
scale_color_cue_type <- ggplot2::scale_color_manual("",
                                                    labels = c("Invalid", "Valid"),
                                                    values = unlist(cue_colors))

cue_task_colors <- c(cue_colors, list(word = "gray"))
scale_color_cue_task <- ggplot2::scale_color_manual(
  "",
  labels = c("Invalid", "Valid", "Word Repetition"),
  values = unlist(cue_task_colors)
)

base_theme <- ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.ticks = ggplot2::element_blank(),
    panel.margin = grid::unit(2, "lines")
  )
