
# return x's that are not in y's
# usage: filter(my_data, subj_id %nin% outliers)
"%nin%" <- function(x, y) {
  !(x %in% y)
}

dualmask_outliers <- c("MWP205a")

temporal_outliers <- c(
  "MWP502",  # incomplete data
  "MWP524",
  "MWP518"
)