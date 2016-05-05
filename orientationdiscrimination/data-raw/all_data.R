
source("data-raw/dualmask_data.R")
source("data-raw/temporal_data.R")
source("data-raw/modality_data.R")
source("data-raw/modality_noise_data.R")
source("data-raw/modality_blocked_data.R")
source("data-raw/dualtask.R")

make_unilateral(TRUE)
make_bilateral(TRUE)

dualmask$exp <- "dualmask"
temporal$exp <- "temporal"
modality$exp <- "modality"
modality_noise$exp <- "modality_noise"
modality_blocked$exp <- "modality_blocked"

orientation <- rbind_list(dualmask, temporal, modality, modality_noise, modality_blocked)

devtools::use_data(orientation)
