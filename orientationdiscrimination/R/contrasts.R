
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

recode_modality_mask_type <- function(frame) {
  mask_types <- c("nomask", "auditory", "visual")
  
  mask_type_map <- data_frame(
    mask_type = mask_types,
    mask_c = ifelse(mask_type == "nomask", -0.5, 0.5)
  )
  
  mask_type_treatment <- contr.treatment(mask_types, base = 1) %>%
    as.data.frame %>%
    rename(auditory_v_nomask = auditory, visual_v_nomask = visual) %>%
    broom::fix_data_frame(newcol = "mask_type")
  
  mask_type_map <- left_join(mask_type_map, mask_type_treatment)
  
  mask_type_helmert <- contr.helmert(mask_types) %>%
    as.data.frame %>%
    rename(helmert_residual = V1, helmert_main = V2) %>%
    broom::fix_data_frame(newcol = "mask_type")
  
  mask_type_map <- left_join(mask_type_map, mask_type_helmert)
  
  frame %>% left_join(mask_type_map)
}

recode_modality_cue_type <- function(frame) {
  cue_types <- c("valid", "nocue", "invalid")
  
  cue_type_poly <- contr.poly(cue_types) %>%
    as.data.frame %>%
    rename(cue_l = `.L`, cue_q = `.Q`) %>%
    mutate(cue_type = cue_types)
  
  frame %>% left_join(cue_type_poly)
}
