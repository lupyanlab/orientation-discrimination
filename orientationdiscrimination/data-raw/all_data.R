
source("data-raw/dualmask_data.R")
source("data-raw/temporal_data.R")

dualmask$exp <- "dualmask"
temporal$exp <- "temporal"

orientation <- rbind_list(dualmask, temporal)

devtools::use_data(orientation)
