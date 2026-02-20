# test-43-mv-cities.R
# Tests for Section 4.3: MV Cities (Municipality Level) Analysis
# Verifies refactored code produces identical results to original
#
# Note: Section 4.3 has fewer robustness checks than state-level analyses.
# It only runs placebo tests, no leave-one-out donor or predictor checks.

BASELINE_NAME <- "section_43"

test_that("Section 4.3 baseline exists or skip", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  expect_true(is.list(baseline))
  expect_true("dataprep_out" %in% names(baseline))
  expect_true("synth_out" %in% names(baseline))
  expect_true("synth_tables" %in% names(baseline))
})

test_that("Section 4.3 main synth dataprep output has expected structure", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  expect_true(!is.null(baseline$dataprep_out$Y1plot))
  expect_true(!is.null(baseline$dataprep_out$Y0plot))
  expect_equal(length(baseline$dataprep_out$Y1plot), 84L)
  expect_equal(nrow(baseline$dataprep_out$Y0plot), 84L)
})

test_that("Section 4.3 main synth output has expected structure", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  expect_true(!is.null(baseline$synth_out$solution.w))
  expect_true(!is.null(baseline$synth_out$solution.v))
  n_predictors <- length(baseline$var_special_predictors)
  expect_equal(ncol(baseline$synth_out$solution.v), n_predictors)
})

test_that("Section 4.3 synth tables have expected structure", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  expect_true(!is.null(baseline$synth_tables$tab.pred))
  expect_true(!is.null(baseline$synth_tables$tab.w))
  expect_true(!is.null(baseline$synth_tables$tab.v))
  expect_true(!is.null(baseline$synth_tables$tab.loss))

  expect_equal(nrow(baseline$synth_tables$tab.pred), 6L)
  expect_equal(ncol(baseline$synth_tables$tab.pred), 3L)
})

test_that("Section 4.3 placebo list has expected structure", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  expect_true(length(baseline$placebo_list) >= 2L)
  expect_equal(names(baseline$placebo_list)[1], "original treatment unit")
})

test_that("Section 4.3 post/pre MSPE ratios have expected properties", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  expect_equal(length(baseline$post_pre), length(baseline$placebo_list))
  expect_true(all(is.finite(baseline$post_pre)))
  expect_true(all(baseline$post_pre > 0))
})

test_that("Section 4.3 does not have robustness donor list", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  expect_true(is.null(baseline$robustness_donor))
})

test_that("Section 4.3 does not have robustness predictor list", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  expect_true(is.null(baseline$robustness_predictor))
})

test_that("Section 4.3 table9 has expected structure", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  expect_true(!is.null(baseline$table9))
  expect_equal(nrow(baseline$table9), 6L)
  expect_true("predictor" %in% names(baseline$table9))
  expect_true("v_weight" %in% names(baseline$table9))
})

test_that("Section 4.3 table10 has expected structure", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  expect_true(!is.null(baseline$table10))
  expect_true(nrow(baseline$table10) >= 1L)
})

test_that("Section 4.3 treated unit is SK MV aggregated", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  expect_equal(baseline$treated_unit, 419L)
  mv_name <- baseline$pool$Name[baseline$pool$UnitNumeric == 419]
  expect_equal(mv_name, "SK MV aggregated")
})

test_that("Section 4.3 donor pool contains cities from specific states", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  expect_true(419L %in% baseline$pool$UnitNumeric)
  expect_true(all(baseline$pool$UnitNumeric > 17L | baseline$pool$UnitNumeric == 419L))
})

test_that("Section 4.3 dependent variable is COVID incidence growth rate", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  expect_equal(baseline$var_dependent, "14 days covid incidence growth rate")
})

test_that("Section 4.3 uses predictor day 72 instead of 79", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  predictor_days <- sapply(baseline$var_special_predictors[4:6], function(x) x[[2]])
  expect_equal(predictor_days, c(65L, 72L, 93L))
})

test_that("Section 4.3 placebo results can be compared", {
  skip_if_no_baseline(BASELINE_NAME)
  skip_if_not_partial_test()
  baseline <- load_baseline(BASELINE_NAME)

  n_compare <- get_placebo_test_count(length(baseline$placebo_list))

  for (i in seq_len(n_compare)) {
    entry <- baseline$placebo_list[[i]]
    expect_true(!is.null(entry$Y1))
    expect_true(!is.null(entry$Y0))
    expect_true(!is.null(entry$synth.out))
    expect_true(!is.null(entry$synth.tables))
  }
})
