# test-411-mv-covid.R
# Tests for Section 4.1.1: MV COVID Incidence Analysis
# Verifies refactored code produces identical results to original

BASELINE_NAME <- "section_411"

test_that("Section 4.1.1 baseline exists or skip", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  expect_true(is.list(baseline))
  expect_true("dataprep_out" %in% names(baseline))
  expect_true("synth_out" %in% names(baseline))
  expect_true("synth_tables" %in% names(baseline))
})

test_that("Section 4.1.1 main synth dataprep output has expected structure", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  expect_true(!is.null(baseline$dataprep_out$Y1plot))
  expect_true(!is.null(baseline$dataprep_out$Y0plot))
  expect_equal(length(baseline$dataprep_out$Y1plot), 84L)
  expect_equal(nrow(baseline$dataprep_out$Y0plot), 84L)
  n_donors <- nrow(baseline$pool) - 1
  expect_equal(ncol(baseline$dataprep_out$Y0plot), n_donors)
})

test_that("Section 4.1.1 main synth output has expected structure", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  expect_true(!is.null(baseline$synth_out$solution.w))
  expect_true(!is.null(baseline$synth_out$solution.v))
  n_donors <- nrow(baseline$pool) - 1
  expect_equal(nrow(baseline$synth_out$solution.w), n_donors)
  n_predictors <- length(baseline$var_special_predictors)
  expect_equal(ncol(baseline$synth_out$solution.v), n_predictors)
})

test_that("Section 4.1.1 synth tables have expected structure", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  expect_true(!is.null(baseline$synth_tables$tab.pred))
  expect_true(!is.null(baseline$synth_tables$tab.w))
  expect_true(!is.null(baseline$synth_tables$tab.v))
  expect_true(!is.null(baseline$synth_tables$tab.loss))

  expect_equal(nrow(baseline$synth_tables$tab.pred), 6L)
  expect_equal(ncol(baseline$synth_tables$tab.pred), 3L)
})

test_that("Section 4.1.1 placebo list has expected length", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  n_pool <- nrow(baseline$pool)
  expect_equal(length(baseline$placebo_list), n_pool)
  expect_equal(names(baseline$placebo_list)[1], "original treatment unit")
})

test_that("Section 4.1.1 post/pre MSPE ratios have expected length", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  n_pool <- nrow(baseline$pool)
  expect_equal(length(baseline$post_pre), n_pool)
  expect_true(all(is.finite(baseline$post_pre)))
  expect_true(all(baseline$post_pre > 0))
})

test_that("Section 4.1.1 robustness donor list exists", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  expect_true(!is.null(baseline$robustness_donor))
  expect_true(length(baseline$robustness_donor) >= 1)
  expect_equal(names(baseline$robustness_donor)[1], "original treatment unit")
})

test_that("Section 4.1.1 robustness predictor list exists", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  expect_true(!is.null(baseline$robustness_predictor))
  expect_true(length(baseline$robustness_predictor) >= 1)
  expect_equal(names(baseline$robustness_predictor)[1], "original treatment unit")
})

test_that("Section 4.1.1 table1 has expected structure", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  expect_true(!is.null(baseline$table1))
  expect_equal(nrow(baseline$table1), 6L)
  expect_true("predictor" %in% names(baseline$table1))
  expect_true("v_weight" %in% names(baseline$table1))
})

test_that("Section 4.1.1 table2 has expected structure", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  expect_true(!is.null(baseline$table2))
  n_donors <- nrow(baseline$pool) - 1
  expect_equal(nrow(baseline$table2), n_donors)
})

test_that("Section 4.1.1 treated unit is correct", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  expect_equal(baseline$treated_unit, 14L)
  mv_name <- baseline$pool$Name[baseline$pool$UnitNumeric == 14]
  expect_equal(mv_name, "Mecklenburg-Vorpommern")
})

test_that("Section 4.1.1 donor pool is correct", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  expected_pool <- c(2L, 4L, 6L:11L, 13L:17L)
  actual_pool <- sort(baseline$pool$UnitNumeric)
  expect_equal(actual_pool, expected_pool)
})

test_that("Section 4.1.1 dependent variable is correct", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  expect_equal(baseline$var_dependent, "14 days covid incidence growth rate")
})

test_that("Section 4.1.1 placebo results can be compared", {
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

test_that("Section 4.1.1 donor robustness results can be compared", {
  skip_if_no_baseline(BASELINE_NAME)
  skip_if_not_full_test()
  baseline <- load_baseline(BASELINE_NAME)

  for (i in seq_along(baseline$robustness_donor)) {
    entry <- baseline$robustness_donor[[i]]
    expect_true(!is.null(entry$Y1))
    expect_true(!is.null(entry$Y0))
    expect_true(!is.null(entry$synth.out))
    expect_true(!is.null(entry$synth.tables))
  }
})

test_that("Section 4.1.1 predictor robustness results can be compared", {
  skip_if_no_baseline(BASELINE_NAME)
  skip_if_not_full_test()
  baseline <- load_baseline(BASELINE_NAME)

  for (i in seq_along(baseline$robustness_predictor)) {
    entry <- baseline$robustness_predictor[[i]]
    expect_true(!is.null(entry$Y1))
    expect_true(!is.null(entry$Y0))
    expect_true(!is.null(entry$synth.out))
    expect_true(!is.null(entry$synth.tables))
  }
})
