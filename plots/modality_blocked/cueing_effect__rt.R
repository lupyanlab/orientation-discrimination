source("models/modality_blocked/cueing_effect__rt_for_plot.R")

library(AICcmodavg)
x_preds <- unique(modality_blocked[,c("cue_type", "cue_l", "cue_q", "mask_type", "helmert_main", "helmert_residual")])
y_preds <- predictSE(rt_mod_full, x_preds, type = "response", se.fit = TRUE) %>%
  as.data.frame(.) %>% select(rt = fit, se = se.fit)
preds <- cbind(x_preds, y_preds)

# ------------------------------------------------------------------------------
# Set the x-values
main_gutter <- 1.2  # distance between no interference and center of interferences
minor_gutter <- 0.45

mask_x_map <- data.frame(
  mask_type = c("nomask", "auditory", "visual"),
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

means_modality_blocked <- dcast(preds, mask_type + mask_c ~ cue_type, value.var = "rt")
rect_points_modality_blocked <- means_modality_blocked %>%
  split(.$mask_type) %>%
  map(make_rects) %>%
  rbind_all(.)

# ------------------------------------------------------------------------------
# Create the error lines.
# - requires grouping columns for mask_type and cue_type and (x, y) columns
modality_blocked_error <- dcast(preds, mask_type + mask_c ~ cue_type, value.var = "se")
modality_blocked_error_points <- means_modality_blocked %>% 
  select(-noise) %>%
  mutate(
    minimum = valid - modality_blocked_error$valid,
    maximum = invalid + modality_blocked_error$invalid,
    valid = valid + modality_blocked_error$valid,
    invalid = invalid - modality_blocked_error$invalid
  )

modality_blocked_error_lines <- modality_blocked_error_points %>% 
  melt(id.vars = c("mask_type", "mask_c")) %>%
  arrange(mask_type, value)

# hack!
modality_blocked_error_lines$cue_type <- rep(c("valid", "valid", "invalid", "invalid"), times=3)
modality_blocked_error_lines$group <- with(modality_blocked_error_lines, paste(mask_type, cue_type, sep = ":"))
modality_blocked_error_lines$variable <- rep(c("min", "max"), times = 6)

modality_blocked_error_bars <- modality_blocked_error_lines %>% dcast(mask_c + group ~ variable, value.var = "value")

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
# Make the modality_blocked plot

modality_blocked_gg <- magnet_plot(preds, rect_points_modality_blocked, modality_blocked_error_bars, c(-0.6, 0.375),
                                   y_lim = c(425, 625)) +
  scale_x_continuous("", breaks = c(-0.6, 0.15, 0.6),
                     labels = c("Blank screen", "Auditory interference", "Visual interference"))
modality_blocked_gg


png("plots/modality_blocked/cueing_effect__rt.png", width = 6, height = 6, units = "in", res = 200)
print(modality_blocked_gg)
dev.off()