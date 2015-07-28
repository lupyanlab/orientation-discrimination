# ------------------------------------------------------------------------------
# Create grobs from images to use as axis labels.

library(grid)
library(png)

png_to_grob <- function(png_image, alpha = 0.2) {
  img <- readPNG(png_image, info = TRUE)
  img_alpha <- matrix(rgb(img[,,1], img[,,2], img[,,3], alpha), nrow=dim(img)[1])
  img_grob <- rasterGrob(img_alpha, interpolate = TRUE)
  img_grob
}

grob_icon <- function(name) {
  full_name <- file.path("plots/annotations/", paste(name, "png", sep = "."))
  png_to_grob(full_name, alpha = 1.0)
}

blank_screen <- png_to_grob("plots/annotations/blank.png", alpha = 1.0)
with_interference <- png_to_grob("plots/annotations/interference.png", alpha = 1.0)

left_icon_x <- -0.5
right_icon_x <- 0.5

icon_width <- 0.2
left_icon_xmin <- left_icon_x - icon_width/2
left_icon_xmax <- left_icon_x + icon_width/2
right_icon_xmin <- right_icon_x - icon_width/2
right_icon_xmax <- right_icon_x + icon_width/2

turn_off_clipping <- function(gg) {
  gt <- ggplot_gtable(ggplot_build(gg))
  gt$layout$clip[gt$layout$name == "panel"] <- "off"
  gt
}
