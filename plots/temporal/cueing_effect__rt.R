source("models/temporal/cueing_effect__rt_for_plot.R")

library(AICcmodavg)
x_preds <- unique(temporal[,c("cue_type", "cue_l", "cue_q", "mask_type", "mask_m", "mask_r")])
y_preds <- predictSE(rt_mod_full, x_preds, type = "response", se.fit = TRUE) %>%
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
# Read in functions from magnets.R
source("plots/magnets.R")

# ------------------------------------------------------------------------------
# Cast the predictions and make the rects
library(reshape2)
library(purrr)

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
# Create grobs from images to use as axis labels.
source("plots/axis-icons.R")
icon_top <- 234

# ------------------------------------------------------------------------------
# Color scheme.
source("plots/colorscheme.R")


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


png("plots/temporal/cueing_effect__rt.png", width = 6, height = 6, units = "in", res = 200)
print(temporal_gg)
dev.off()
