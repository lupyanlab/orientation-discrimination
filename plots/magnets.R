library(ggplot2)
library(extrafont)
loadfonts()

#' Turn a single row with columns for invalid, noise, and valid
#' into a data.frame with two rows, one for the valid priming
#' effect and another for the invalid priming effect, and
#' columns for ymin and ymax.
bar_width <- 0.08
error_bar_width <- bar_width
make_rects <- function(row) {
  with(row, 
       data.frame(
         ymin = c(valid, noise), 
         ymax = c(noise, invalid), 
         cue_type = c("valid", "invalid"),
         mask_type = mask_type,
         mask_c = mask_c
       ) %>%
         mutate(xmin = mask_c - bar_width/2, xmax = mask_c + bar_width/2)
  )
}

make_rects_modality <- function(row) {
  with(row, 
       data.frame(
         ymin = c(valid, nocue), 
         ymax = c(nocue, invalid), 
         cue_type = c("valid", "invalid"),
         mask_type = mask_type,
         mask_c = mask_c
       ) %>%
         mutate(xmin = mask_c - bar_width/2, xmax = mask_c + bar_width/2)
  )
}


default_y_lim <- c(425, 575)
x_lim <- c(-1, 1)
magnet_plot <- function(preds, rects, error, x_breaks, y_lim = default_y_lim) {
  ggplot(preds) +
    geom_rect(aes(fill = cue_type, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
      data = rects, alpha = 0.4) +
    geom_errorbar(aes(x = mask_c, ymin = min, ymax = max),
        data = error, width = error_bar_width, color = error_bar_color) +
    geom_point(aes(x = mask_c, y = rt, color = cue_type), size = 3,
               data = preds) +
    scale_color_manual(values = unlist(magnet_color_scheme)) +
    scale_fill_manual(values = unlist(magnet_color_scheme)) +
    scale_x_continuous("", breaks = x_breaks, 
      labels = c("Blank screen", "Visual interference")) +
    scale_y_continuous("Reaction Time (ms)", breaks = seq(y_lim[1], y_lim[2], by = 25)) +
    coord_cartesian(ylim = y_lim, xlim = x_lim) +
    theme_minimal(base_size = 10) +
    theme(
      text = element_text("Arial"),
      legend.position = "none",
      axis.ticks.x = element_blank(),
      axis.title.x = element_text(hjust = 0.82, size = 6),
      axis.line = element_line(color = "black"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      plot.margin = unit(c(0.5, 0.5, 0.0, 0.0), units = "lines")
    )
}
