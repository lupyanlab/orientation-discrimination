source("./new-graphs/magnets.R")

SAVE_AS <- FALSE

temporal_gg <- temporal_gg + 
  theme(
    axis.text.y = element_blank(), 
    axis.title.y = element_blank(),
    axis.title.x = element_text(hjust = 0.82, size = 6)
  )

#source("./new-graphs/magnets-key.R")
source("./new-graphs/axis-icons.R")

trialstructure <- png_to_grob("./new-graphs/orientation-discrimination-overlap.png", alpha = 1.0)

library(gridExtra)

if (SAVE_AS == "png") {
  png("./new-graphs/fig2.png", width = 8, height = 3, units = "in", res = 200)
  grid.arrange(trialstructure, labeled_gg, temporal_gg, ncol = 3)
  dev.off()
}