# ---- orientation-discrimination-exp1
library(dplyr)
library(magrittr)
library(lme4)
library(AICcmodavg)
library(reshape2)
library(purrr)
library(ggplot2)
library(extrafont)
loadfonts()

library(orientationdiscrimination)
data("dualmask")

dualmask %<>%
  recode_mask_type %>%
  recode_cue_type %>%
  label_outliers %>%
  filter(is_subj_outlier == 0)

# Predict rts from mask_type and cue_type
rt_mod <- lmer(rt ~ mask_c * (cue_l + cue_q) + (1|subj_id), data = dualmask)

# Get the values for the plot
dv_columns <- c("cue_type", "cue_l", "cue_q", "mask_type", "mask_c")
points <- dualmask[, dv_columns] %>% unique()
values <- predictSE(rt_mod, points, se.fit = TRUE, type = "response") %>%
  as.data.frame() %>% select(rt = fit, se = se.fit) %>% 
  cbind(points, .)

# Squish bars inward
squish <- 0.1
values <- values %>% mutate(mask_c = ifelse(mask_c < 0, mask_c + squish, mask_c - squish))

# Create a data.frame to use to draw the rects.
# - requires xmin, xmax, ymin, ymax, and a grouping column (mask_type)

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


#error_bar_color <- "#fc8d62"  # orange, from colorbrewer
dark_gray <- "#636363"

error_bar_color <- dark_gray # dark gray
line_color <- dark_gray

light_blue <- "#9ecae1"
dark_blue <- "#08519c"
light_green <- "#a1d99b"
dark_green <- "#006d2c"

color_scheme <- list(
  visualmask = dark_blue,
  visualnomask = light_blue,
  nonvisualmask = dark_green,
  nonvisualnomask = light_green
)

imagery_color_scheme <- list(mask = dark_blue, nomask = light_blue)
facts_color_scheme <- list(mask = dark_green, nomask = light_green)

magnet_color_scheme <- list(invalid = "#d7191c", valid = "#a6d96a",
                            noise = dark_gray)



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

y_lim <- default_y_lim

means <- dcast(values, mask_type + mask_c ~ cue_type, value.var = "rt")

rect_points <- means %>%
  split(.$mask_type) %>%
  map(make_rects) %>%
  bind_rows(.)

# Create a data.frame to draw the error lines.
# - requires grouping columns for mask_type and cue_type and (x, y) columns

error <- dcast(values, mask_type + mask_c ~ cue_type, value.var = "se")
error_points <- means %>% 
  select(-noise) %>%
  mutate(
    minimum = valid - error$valid,
    maximum = invalid + error$invalid,
    valid = valid + error$valid,
    invalid = invalid - error$invalid
  )

error_lines <- error_points %>% 
  melt(id.vars = c("mask_type", "mask_c")) %>%
  arrange(mask_type, value)

# hack!
error_lines$cue_type <- rep(c("valid", "valid", "invalid", "invalid"), times=2)
error_lines$group <- with(error_lines, paste(mask_type, cue_type, sep = ":"))
error_lines$variable <- rep(c("min", "max"), times = 4)

error_bars <- error_lines %>% dcast(mask_c + group ~ variable, value.var = "value")

# Create the plot
magnet_gg <- magnet_plot(values, rect_points, error_bars, x_breaks = c(-0.4, 0.4))
magnet_gg

# ------------------------------------------------------------------------------
# Add labels
heights <- values %>% filter(mask_type == "nomask") %>% select(cue_type, rt)

left_end_of_rect <- min(values$mask_c) - bar_width/2
line_length <- 0.16
spacer <- 0.02
right_end_of_line <- left_end_of_rect - spacer
left_end_of_line <- right_end_of_line - line_length

label_lines <- data.frame(cue_type = c("invalid", "noise", "valid"))
label_lines <- label_lines %>% left_join(heights)
label_lines <- label_lines %>% left_join(
  expand.grid(cue_type = c("invalid", "noise", "valid"),
              x = c(right_end_of_line, left_end_of_line))
)

text_right <- left_end_of_line - spacer
labels <- data.frame(
  cue_type = c("invalid", "noise", "valid"),
  label = c("Invalid words", "Noise cues", "Valid words"),
  x = text_right
) %>% left_join(heights)

labeled_gg <- magnet_gg +
  geom_text(aes(x = x, y = rt, label = label), 
            hjust = 1, vjust = 0.4, size = 2.2, data = labels) +
  geom_line(aes(x = x, y = rt, group = cue_type), 
            size = 0.2, data = label_lines) +
  coord_cartesian(ylim = y_lim, xlim = x_lim - 0.2) +
  scale_x_continuous("", breaks = c(-0.4, 0.4), 
                     labels = c("Blank screen", "Visual interference"))
labeled_gg