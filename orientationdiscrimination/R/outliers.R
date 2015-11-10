library(tidyr)

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

post_survey_strategies <- data_frame(
  ODM309 = "one-side",
  ODM310 = "one-side",
  ODM312 = "one-side",
  ODM313 = "one-side",
  ODM316 = "one-side",
  ODM311 = "central",
  ODM315 = "central",
  ODM314 = "central"
) %>% gather(subj_id, strategy)
