
recode_mask_type <- function(frame) {
  mask_type_map <- data.frame(
    mask_type = c("nomask", "mask", "during", "after"),
    mask_c = c(-0.5, 0.5, 0.5, 0.5))

  # Helmert contrasts for comparing nomask v (during, after) and during v after
  mask_type_map <- mask_type_map %>% mutate(
    mask_m = car::recode(mask_type, "'nomask'=-2/3; else=1/3", as.factor.result = FALSE, as.numeric.result = TRUE),
    mask_r = car::recode(mask_type, "'nomask'=0; 'after'=1/2; else=-1/2", as.factor.result = FALSE, as.numeric.result = TRUE)
  )

  merge(frame, mask_type_map, all.x = TRUE)
}

recode_cue_type <- function(frame) {
  frame %>% mutate(
    cue_l = car::recode(cue_type, "'valid'=-1/2; 'noise'=0; 'invalid'=1/2", as.factor.result = FALSE, as.numeric.result = TRUE),
    cue_q = car::recode(cue_type, "'valid'=-1/3; 'noise'=2/3; 'invalid'=-1/3", as.factor.result = FALSE, as.numeric.result = TRUE),
    cue_c = car::recode(cue_type, "'noise'=-1/2; else=1/2", as.factor.result = FALSE, as.numeric.result = TRUE)
  )
}
