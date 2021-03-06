#' return x's that are not in y's
#' usage: filter(my_data, subj_id %nin% outliers)
#' @export
"%nin%" <- function(x, y) {
  !(x %in% y)
}

#' @import dplyr
#' @export
label_outliers <- function(frame) {
  dualmask_outliers <- c("MWP205a")
  temporal_outliers <- c(
    "MWP502",  # incomplete data
    "MWP524",
    "MWP518"
  )
  outliers <- c(dualmask_outliers, temporal_outliers)
  frame %>%
    mutate(is_subj_outlier = ifelse(subj_id %in% outliers, 1, 0))
}

post_survey_strategies <- dplyr::data_frame(
  ODM309 = "one-side",
  ODM310 = "one-side",
  ODM312 = "one-side",
  ODM313 = "one-side",
  ODM316 = "one-side",
  ODM311 = "central",
  ODM315 = "central",
  ODM314 = "central",
  ODM318 = "one-side",
  ODM319 = "one-side",
  ODM317 = "one-side",
  ODM321 = "central",
  ODM322 = "central",
  ODM320 = "central",
  ODM323 = "one-side",
  ODM325 = NA,
  ODM324 = "central",
  ODM327 = "central",
  ODM330 = "central",
  ODM329 = NA,
  ODM328 = NA,
  ODM331 = NA,
  ODM332 = "one-side",
  ODM334 = "one-side",
  ODM333 = "central",
  ODM335 = NA,
  ODM336 = "central",
  ODM337 = NA,
  ODM338 = "central",
  ODM340 = NA,
  ODM339 = "central",
  ODM341 = "one-side",
  ODM343 = NA,
  ODM342 = "one-side",
  ODM345 = NA,
  ODM344a = "central",
  ODM346 = "central"
)

post_survey_strategies <- tidyr::gather(post_survey_strategies, subj_id, strategy)
