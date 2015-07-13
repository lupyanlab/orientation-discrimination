library(dplyr)
library(magrittr)
library(tidyr)

options(stringsAsFactors = FALSE)

# ------------------------------------------------------------------------------
# Summarize the data.

dualmask <- read.csv("./dualmask/dualmask-final.csv")

# remove slow, low accuracy subject
dualmask <- filter(dualmask, subj_id != "MWP205a")

# fit main model
library(lme4)
rts <- lmer(rt ~ (cue_l + cue_q) * mask_c + (1|subj_id), data = dualmask)

# get the values for the plot
library(AICcmodavg)
dv_columns <- c("cue_type", "cue_l", "cue_q", "mask_type", "mask_c")
points <- dualmask[, dv_columns] %>% unique()
values <- predictSE(rts, points, se.fit = TRUE, type = "response") %>%
  as.data.frame() %>% select(rt = fit, se = se.fit) %>% 
  cbind(points, .)

# ------------------------------------------------------------------------------
# Squish bars inward
squish <- 0.1
values <- values %>% mutate(mask_c = ifelse(mask_c < 0, mask_c + squish, mask_c - squish))

# ------------------------------------------------------------------------------
# Create a data.frame to use to draw the rects.
# - requires xmin, xmax, ymin, ymax, and a grouping column (mask_type)

# spread means and error so that each cue is its own column
# should have two rows and three columns
library(reshape2)

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

library(purrr)  # go functional programming!!

means <- dcast(values, mask_type + mask_c ~ cue_type, value.var = "rt")
  
rect_points <- means %>%
  split(.$mask_type) %>%
  map(make_rects) %>%
  rbind_all(.)

# ------------------------------------------------------------------------------
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

# ------------------------------------------------------------------------------
# Create grobs from images to use as axis labels.

source("./new-graphs/axis-icons.R")
icon_top <- 234

# ------------------------------------------------------------------------------
# Color scheme.

source("./new-graphs/colorscheme.R")

# ------------------------------------------------------------------------------
# Main plot

library(ggplot2)

y_lim <- c(425, 575)
x_lim <- c(-1, 1)
magnet_plot <- function(preds, rects, error, x_breaks) {
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
    scale_y_continuous("Reaction Time (ms)", breaks = seq(450, 550, by = 25)) +
    coord_cartesian(ylim = y_lim, xlim = x_lim) +
    theme_bw(base_size = 10) +
    theme(
      legend.position = "none",
      axis.ticks.x = element_blank(),
      axis.title.x = element_text(hjust = 0.82, size = 6),
      axis.line = element_line(color = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      plot.margin = unit(c(0.5, 0.5, 0.0, 0.0), units = "lines")
    )
}

magnet_gg <- magnet_plot(values, rect_points, error_bars, x_breaks = c(-0.4, 0.4))
magnet_gg

if (FALSE) {
  png("./new-graphs/magnets-main.png", width = 6, height = 6, units = "in", res = 200)
  print(magnet_gg)
  dev.off()
}

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

if (FALSE) {
  png("./new-graphs/magnets-labeled.png", width = 6, height = 6, units = "in", res = 200)
  print(labeled_gg)
  dev.off()
}

# ------------------------------------------------------------------------------
# Temporal plot
source("./helper/helper-funcs.R")  # import `%nin%` for filtering

temporal <- read.csv("./temporal/temporal-final.csv")
temporal <- filter(temporal, subj_id != "MWP502")
temporal <- filter(temporal, subj_id %nin% c("MWP524", "MWP518"))

# ------------------------------------------------------------------------------
# Get the predictions
mod <- lmer(rt ~ (cue_l + cue_q) * (mask_m + mask_r) + (1|subj_id), data = temporal)
x_preds <- unique(temporal[,c("cue_type", "cue_l", "cue_q", "mask_type", "mask_m", "mask_r")])
y_preds <- predictSE(mod, x_preds, type = "response", se.fit = TRUE) %>%
  as.data.frame(.) %>% select(rt = fit, se = se.fit)
preds <- cbind(x_preds, y_preds)

# ------------------------------------------------------------------------------
# Set the x-values
main_gutter <- 1.2  # distance between no interference and center of interferences
minor_gutter <- 0.45

mask_x_map <- data.frame(
  mask_type = c("nomask", "during", "after"),
  mask_c = c(-main_gutter/2, 
             main_gutter/2 - minor_gutter, 
             main_gutter/2))

preds <- preds %>% left_join(mask_x_map)

# ------------------------------------------------------------------------------
# Cast the predictions and make the rects
means_temporal <- dcast(preds, mask_type + mask_c ~ cue_type, value.var = "rt")
rect_points_temporal <- means_temporal %>%
  split(.$mask_type) %>%
  map(make_rects) %>%
  rbind_all(.)

# ------------------------------------------------------------------------------
# Create the error lines.
# - requires grouping columns for mask_type and cue_type and (x, y) columns
temporal_error <- dcast(preds, mask_type + mask_c ~ cue_type, value.var = "se")
temporal_error_points <- means_temporal %>% 
  select(-noise) %>%
  mutate(
    minimum = valid - temporal_error$valid,
    maximum = invalid + temporal_error$invalid,
    valid = valid + temporal_error$valid,
    invalid = invalid - temporal_error$invalid
  )

temporal_error_lines <- temporal_error_points %>% 
  melt(id.vars = c("mask_type", "mask_c")) %>%
  arrange(mask_type, value)

# hack!
temporal_error_lines$cue_type <- rep(c("valid", "valid", "invalid", "invalid"), times=3)
temporal_error_lines$group <- with(temporal_error_lines, paste(mask_type, cue_type, sep = ":"))
temporal_error_lines$variable <- rep(c("min", "max"), times = 6)

temporal_error_bars <- temporal_error_lines %>% dcast(mask_c + group ~ variable, value.var = "value")

# ------------------------------------------------------------------------------
# Arrange the axis icons
rightmost_icon_x <- 0.5
rightmost_icon_xmin <- rightmost_icon_x - icon_width/2
rightmost_icon_xmax <- rightmost_icon_x + icon_width/2

rightmid_icon_x <- 0.5 - minor_gutter
rightmid_icon_xmin <- rightmid_icon_x - icon_width/2
rightmid_icon_xmax <- rightmid_icon_x + icon_width/2

# ------------------------------------------------------------------------------
# Make the temporal plot

spaces <- 8  # int
interference_type <- paste0("during word", paste(rep(" ", times = spaces), collapse = ""), "after word")
temporal_gg <- magnet_plot(preds, rect_points_temporal, temporal_error_bars, c(-0.6, 0.375)) +
  scale_x_continuous(interference_type, breaks = c(-0.6, 0.375),
    labels = c("Blank screen", "Visual interference")) +
  theme(
    axis.title.x = element_text(hjust = 0.70, size = 6)
  )
temporal_gg

if (FALSE) {
  png("./new-graphs/magnets-temporal.png", width = 6, height = 6, units = "in", res = 200)
  print(temporal_gg)
  dev.off()
}