library(broom)

#' @import dplyr
#' @import broom
#' @export
report_glmer_effect <- function(mod, param) {
  parameter_stats <- mod %>%
    tidy(effects = "fixed") %>%
    filter(term == param)
  estimate <- parameter_stats$estimate
  z_value <- parameter_stats$statistic
  p_value <- parameter_stats$p.value

  interval <- confint(mod)[param, ]
  lwr <- interval[1]
  upr <- interval[2]

  sprintf("%.2f log-odds, 95%% CI [%.2f, %.2f], _z_ = %.4f, _p_ = %.4f",
          estimate,
          lwr,
          upr,
          z_value,
          p_value)
}

#' @import dplyr
report_lmerTest_effect <- function(mod, param) {
  parameter_stats <- lmerTest::summary(mod)$coefficients %>% as.data.frame %>% .[param, ]
  estimate <- parameter_stats[["Estimate"]]
  z_value <- parameter_stats[["t value"]]
  p_value <- parameter_stats[["Pr(>|t|)"]]

  interval <- confint(mod)[param, ]
  lwr <- interval[1]
  upr <- interval[2]

  sprintf("%.2f ms., 95%% CI [%.2f, %.2f], _z_ = %.4f, _p_ = %.4f",
          estimate,
          lwr,
          upr,
          z_value,
          p_value)
}

#' @import dplyr
add_sig_stars <- function(frame) {
  frame %>% mutate(
    sig = ifelse(p.value > 0.05, "",
                 ifelse(p.value > 0.01, "*",
                        ifelse(p.value > 0.001, "**",
                               "***"))))
}
