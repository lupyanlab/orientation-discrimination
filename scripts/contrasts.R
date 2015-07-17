
recode_mask_type <- function(frame) {
  mask_type_map <- data.frame(
    mask_type = c("nomask", "mask"),
    mask_c = c(-0.5, 0.5))
  merge(frame, mask_type_map, all.x = TRUE)
}

recode_cue_type <- function(frame) {
  frame %>% mutate(
    cue_l = car::recode(cue_type, "'valid'=-1/2; 'noise'=0; 'invalid'=1/2"),
    cue_q = car::recode(cue_type, "'valid'=-1/3; 'noise'=2/3; 'invalid'=-1/3"))
}
