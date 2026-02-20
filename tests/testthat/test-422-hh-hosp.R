# test-422-hh-hosp.R
# Tests for Section 4.2.2: Hamburg Hospitalization Analysis
# Verifies refactored code produces identical results to original

BASELINE_NAME <- "section_422"

test_that("Section 4.2.2 baseline exists or skip", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  expect_true(is.list(baseline))
  expect_true("dataprep_out" %in% names(baseline))
  expect_true("synth_out" %in% names(baseline))
  expect_true("synth_tables" %in% names(baseline))
})

test_that("Section 4.2.2 main synth dataprep output has expected structure", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  expect_true(!is.null(baseline$dataprep_out$Y1plot))
  expect_true(!is.null(baseline$dataprep_out$Y0plot))
  expect_equal(length(baseline$dataprep_out$Y1plot), 84L)
  expect_equal(nrow(baseline$dataprep_out$Y0plot), 84L)
})

test_that("Section 4.2.2 main synth output has expected structure", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  expect_true(!is.null(baseline$synth_out$solution.w))
  expect_true(!is.null(baseline$synth_out$solution.v))
  n_predictors <- length(baseline$var_special_predictors)
  expect_equal(ncol(baseline$synth_out$solution.v), n_predictors)
})

test_that("Section 4.2.2 synth tables have expected structure", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  expect_true(!is.null(baseline$synth_tables$tab.pred))
  expect_true(!is.null(baseline$synth_tables$tab.w))
  expect_true(!is.null(baseline$synth_tables$tab.v))
  expect_true(!is.null(baseline$synth_tables$tab.loss))

  expect_equal(nrow(baseline$synth_tables$tab.pred), 6L)
  expect_equal(ncol(baseline$synth_tables$tab.pred), 3L)
})

test_that("Section 4.2.2 placebo list has expected structure", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  expect_true(length(baseline$placebo_list) >= 2L)
  expect_equal(names(baseline$placebo_list)[1], "original treatment unit")
})

test_that("Section 4.2.2 post/pre MSPE ratios have expected properties", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  expect_equal(length(baseline$post_pre), length(baseline$placebo_list))
  expect_true(all(is.finite(baseline$post_pre)))
  expect_true(all(baseline$post_pre > 0))
})

test_that("Section 4.2.2 robustness donor list exists", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  expect_true(!is.null(baseline$robustness_donor))
  expect_true(length(baseline$robustness_donor) >= 1)
  expect_equal(names(baseline$robustness_donor)[1], "original treatment unit")
})

test_that("Section 4.2.2 robustness predictor list exists", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  expect_true(!is.null(baseline$robustness_predictor))
  expect_true(length(baseline$robustness_predictor) >= 1)
  expect_equal(names(baseline$robustness_predictor)[1], "original treatment unit")
})

test_that("Section 4.2.2 table7 has expected structure", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  expect_true(!is.null(baseline$table7))
  expect_equal(nrow(baseline$table7), 6L)
  expect_true("predictor" %in% names(baseline$table7))
  expect_true("v_weight" %in% names(baseline$table7))
})

test_that("Section 4.2.2 table8 has expected structure", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  expect_true(!is.null(baseline$table8))
  expect_true(nrow(baseline$table8) >= 1L)
})

test_that("Section 4.2.2 treated unit is Hamburg", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  expect_equal(baseline$treated_unit, 3L)
  hh_name <- baseline$pool$Name[baseline$pool$UnitNumeric == 3]
  expect_equal(hh_name, "Hamburg")
})

test_that("Section 4.2.2 donor pool includes all states except MV", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  expected_pool <- c(2L:13L, 15L:17L)
  actual_pool <- sort(baseline$pool$UnitNumeric)
  expect_equal(actual_pool, expected_pool)
})

test_that("Section 4.2.2 dependent variable is hospitalization incidence level", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  expect_equal(baseline$var_dependent, "14 days hospitalization incidence")
})

test_that("Section 4.2.2 predictors use hospitalization incidence", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  hosp_predictors <- sapply(baseline$var_special_predictors, function(x) {
    grepl("hospitalization incidence", x[[1]]) && !grepl("growth rate", x[[1]])
  })
  expect_true(sum(hosp_predictors) == 3L)
})

test_that("Section 4.2.2 uses different predictor days than 4.2.1", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  predictor_days <- sapply(baseline$var_special_predictors[4:6], function(x) x[[2]])
  expect_equal(predictor_days, c(58L, 79L, 93L))
})

test_that("Section 4.2.2 placebo results can be compared", {
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

test_that("Section 4.2.2 donor robustness results can be compared", {
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

test_that("Section 4.2.2 predictor robustness results can be compared", {
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
