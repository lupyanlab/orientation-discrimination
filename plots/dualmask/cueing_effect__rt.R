source("models/dualmask/cueing_effect__rt_for_plot.R")

# get the values for the plot
library(AICcmodavg)
dv_columns <- c("cue_type", "cue_l", "cue_q", "mask_type", "mask_c")
points <- dualmask[, dv_columns] %>% unique()
values <- predictSE(rt_mod, points, se.fit = TRUE, type = "response") %>%
  as.data.frame() %>% select(rt = fit, se = se.fit) %>% 
  cbind(points, .)

# ------------------------------------------------------------------------------
# Squish bars inward
squish <- 0.1
values <- values %>% mutate(mask_c = ifelse(mask_c < 0, mask_c + squish, mask_c - squish))

# ------------------------------------------------------------------------------
# Create a data.frame to use to draw the rects.
# - requires xmin, xmax, ymin, ymax, and a grouping column (mask_type)

source("plots/magnets.R")
y_lim <- default_y_lim

library(reshape2)
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
source("plots/axis-icons.R")
icon_top <- 234

# ------------------------------------------------------------------------------
# Color scheme.
source("plots/colorscheme.R")

# ------------------------------------------------------------------------------
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


png("plots/dualmask/cueing_effect__rt.png", width = 6, height = 6, units = "in", res = 200)
print(labeled_gg)
dev.off()
