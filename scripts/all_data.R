
CLEAR_GLOBAL_ENVIRONMENT <- FALSE
source("scripts/dualmask_data.R")
source("scripts/temporal_data.R")

dualmask$exp <- "dualmask"
temporal$exp <- "temporal"

orientation <- rbind_list(dualmask, temporal)

rm(list = setdiff(ls(), "orientation"))
