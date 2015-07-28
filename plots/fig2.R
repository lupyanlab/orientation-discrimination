library(gridExtra)
source("plots/axis-icons.R")

# Dualmask experiment
source("plots/dualmask/cueing_effect__rt.R")
dualmask_trial_structure <- png_to_grob("plots/dualmask/trial-structure.png", alpha = 1.0)

grid.arrange(dualmask_trial_structure, labeled_gg, nrow = 1)

# Temporal experiment
source("plots/temporal/cueing_effect__rt.R")
temporal_trial_structure <- png_to_grob("plots/temporal/trial-structure.png", alpha = 1.0)

grid.arrange(temporal_trial_structure, temporal_gg, nrow = 1)

# Four panel plot
grid.arrange(dualmask_trial_structure, labeled_gg,
             temporal_trial_structure, temporal_gg,
             nrow = 2)
