library(broom)

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

report_lmerTest_effect <- function(mod, param) {
  parameter_stats <- summary(mod)$coefficients[param, ] %>% as.data.frame
  estimate <- parameter_stats["Estimate", ]
  p_value <- parameter_stats["Pr(>|t|)", ]
  
  interval <- confint(mod)[param, ]
  lwr <- interval[1]
  upr <- interval[2]
  
  sprintf("%.2f ms., 95%% CI [%.2f, %.2f], p = %.4f",
          estimate,
          lwr,
          upr,
          p_value)
}