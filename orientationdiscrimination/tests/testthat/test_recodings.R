library(testthat)
devtools::load_all()

context("Recode cue type")

test_that("cue type works for dualmask experiment", {
  dualmask <- data_frame(cue_type = c("valid", "noise", "invalid")) %>%
    recode_cue_type

  expect_equal(dualmask$cue_c, c(-1/2, NA, 1/2))
  expect_equal(dualmask$cue_l, c(-1/2, 0, 1/2))
  expect_equal(dualmask$cue_q, c(-1/3, 2/3, -1/3))

  expect_equal(dualmask$valid_v_baseline, c(1, 0, 0))
  expect_equal(dualmask$invalid_v_baseline, c(0, 0, 1))
})

test_that("cue type works for modality experiment", {
  modality <- data_frame(cue_type = c("valid", "nocue", "invalid")) %>%
    recode_cue_type

  expect_equal(modality$cue_l, c(-1/2, 0, 1/2))
  expect_equal(modality$cue_q, c(-1/3, 2/3, -1/3))
})

context("Recode mask type")

test_that("mask type works for dualmask experiment", {
  dualmask <- data_frame(mask_type = c("nomask", "mask")) %>%
    recode_mask_type

  expect_equal(dualmask$mask_c, c(-0.5, 0.5))
})

test_that("mask type works for temporal experiment", {
  temporal <- data_frame(mask_type = c("nomask", "during", "after")) %>%
    recode_mask_type

  expect_equal(temporal$mask_c, c(-0.5, 0.5, 0.5))
  expect_equal(temporal$mask_m, c(-2/3, 1/3, 1/3))
  expect_equal(temporal$mask_r, c(0, 1/2, -1/2))
})

test_that("mask type works for modality experiment", {
  modality <- data_frame(mask_type = c("nomask", "visual", "auditory")) %>%
    recode_mask_type

  expect_equal(modality$mask_c, c(-0.5, 0.5, NA))
  expect_equal(modality$visual_v_nomask, c(0, 1, 0))
  expect_equal(modality$auditory_v_nomask, c(0, 0, 1))
})
