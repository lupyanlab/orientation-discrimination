library(gridExtra)
source("plots/axis-icons.R")

# Dualmask experiment
# -------------------
source("plots/dualmask/cueing_effect__rt.R")
dualmask_trial_structure <- png_to_grob("plots/dualmask/trial-structure.png", alpha = 1.0)

png("plots/dualmask/fig2_dualmask.png", width = 8, height = 3, units = "in", res = 400)
grid.arrange(dualmask_trial_structure, labeled_gg, nrow = 1)
dev.off()

# Temporal experiment
# -------------------
source("plots/temporal/cueing_effect__rt.R")
temporal_trial_structure <- png_to_grob("plots/temporal/trial-structure.png", alpha = 1.0)

png("plots/temporal/fig2_temporal.png", width = 8, height = 3, units = "in", res = 400)
grid.arrange(temporal_trial_structure, temporal_gg, nrow = 1)
dev.off()

# Trial structures only
# ---------------------
png("plots/fig2_trials.png", width = 4, height = 6, units = "in", res = 400)
grid.arrange(dualmask_trial_structure, temporal_trial_structure)
dev.off()

# Data only
# ---------
png("plots/fig2_data.png", width = 8, height = 3, units = "in", res = 400)
grid.arrange(labeled_gg, temporal_gg, nrow = 1)
dev.off()

# Four panel plot
# ---------------
png("plots/fig2.png", width = 8, height = 6, units = "in", res = 400)
grid.arrange(dualmask_trial_structure, temporal_trial_structure,
             labeled_gg, temporal_gg,
             nrow = 2)
dev.off()
