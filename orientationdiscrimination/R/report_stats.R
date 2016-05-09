library(broom)

#' @import dplyr
#' @import broom
#' @export
report_glmer_effect <- function(mod, param) {
  parameter_stats <- mod %>%
    tidy(effects = "fixed") %>%
    filter(term == param)
  estimate <- parameter_stats$estimate
  p_value <- parameter_stats$p.value

  interval <- confint(mod)[param, ]
  lwr <- interval[1]
  upr <- interval[2]

  sprintf("%.2f log-odds, 95%% CI [%.2f, %.2f], p = %.4f",
          estimate,
          lwr,
          upr,
          p_value)
}

#' @import dplyr
report_lmerTest_effect <- function(mod, param) {
  parameter_stats <- summary(mod)$coefficients[param, ] %>% as.data.frame
  estimate <- parameter_stats["Estimate", ]
  p_value <- parameter_stats[5, ]
  
  interval <- confint(mod)[param, ]
  lwr <- interval[1]
  upr <- interval[2]

  sprintf("%.2f ms., 95%% CI [%.2f, %.2f], p = %.4f",
          estimate,
          lwr,
          upr,
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
