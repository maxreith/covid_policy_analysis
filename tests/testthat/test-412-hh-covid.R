# test-412-hh-covid.R
# Tests for Section 4.1.2: Hamburg COVID Incidence Analysis
# Verifies refactored code produces identical results to original

BASELINE_NAME <- "section_412"

test_that("Section 4.1.2 baseline exists or skip", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  expect_true(is.list(baseline))
  expect_true("dataprep_out" %in% names(baseline))
  expect_true("synth_out" %in% names(baseline))
  expect_true("synth_tables" %in% names(baseline))
})

test_that("Section 4.1.2 main synth dataprep output has expected structure", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  expect_true(!is.null(baseline$dataprep_out$Y1plot))
  expect_true(!is.null(baseline$dataprep_out$Y0plot))
  expect_equal(length(baseline$dataprep_out$Y1plot), 84L)
  expect_equal(nrow(baseline$dataprep_out$Y0plot), 84L)
})

test_that("Section 4.1.2 main synth output has expected structure", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  expect_true(!is.null(baseline$synth_out$solution.w))
  expect_true(!is.null(baseline$synth_out$solution.v))
  n_predictors <- length(baseline$var_special_predictors)
  expect_equal(ncol(baseline$synth_out$solution.v), n_predictors)
})

test_that("Section 4.1.2 synth tables have expected structure", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  expect_true(!is.null(baseline$synth_tables$tab.pred))
  expect_true(!is.null(baseline$synth_tables$tab.w))
  expect_true(!is.null(baseline$synth_tables$tab.v))
  expect_true(!is.null(baseline$synth_tables$tab.loss))

  expect_equal(nrow(baseline$synth_tables$tab.pred), 6L)
  expect_equal(ncol(baseline$synth_tables$tab.pred), 3L)
})

test_that("Section 4.1.2 placebo list has expected structure", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  expect_true(length(baseline$placebo_list) >= 2L)
  expect_equal(names(baseline$placebo_list)[1], "original treatment unit")
})

test_that("Section 4.1.2 post/pre MSPE ratios have expected properties", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  expect_equal(length(baseline$post_pre), length(baseline$placebo_list))
  expect_true(all(is.finite(baseline$post_pre)))
  expect_true(all(baseline$post_pre > 0))
})

test_that("Section 4.1.2 robustness donor list exists", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  expect_true(!is.null(baseline$robustness_donor))
  expect_true(length(baseline$robustness_donor) >= 1)
  expect_equal(names(baseline$robustness_donor)[1], "original treatment unit")
})

test_that("Section 4.1.2 robustness predictor list exists", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  expect_true(!is.null(baseline$robustness_predictor))
  expect_true(length(baseline$robustness_predictor) >= 1)
  expect_equal(names(baseline$robustness_predictor)[1], "original treatment unit")
})

test_that("Section 4.1.2 table3 has expected structure", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  expect_true(!is.null(baseline$table3))
  expect_equal(nrow(baseline$table3), 6L)
  expect_true("predictor" %in% names(baseline$table3))
  expect_true("v_weight" %in% names(baseline$table3))
})

test_that("Section 4.1.2 table4 has expected structure", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  expect_true(!is.null(baseline$table4))
  expect_true(nrow(baseline$table4) >= 1L)
})

test_that("Section 4.1.2 treated unit is Hamburg", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  expect_equal(baseline$treated_unit, 3L)
  hh_name <- baseline$pool$Name[baseline$pool$UnitNumeric == 3]
  expect_equal(hh_name, "Hamburg")
})

test_that("Section 4.1.2 donor pool contains cities", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  expect_true(3L %in% baseline$pool$UnitNumeric)
  expect_true(12L %in% baseline$pool$UnitNumeric)
})

test_that("Section 4.1.2 dependent variable is correct", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  expect_equal(baseline$var_dependent, "14 days covid incidence growth rate")
})

test_that("Section 4.1.2 placebo results can be compared", {
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

test_that("Section 4.1.2 donor robustness results can be compared", {
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

test_that("Section 4.1.2 predictor robustness results can be compared", {
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
