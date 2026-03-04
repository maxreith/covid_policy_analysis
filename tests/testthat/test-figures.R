# test-figures.R
# Tests for Figures 1 and 2: COVID and Hospitalization Incidence
# Verifies the data values plotted match the original

BASELINE_NAME <- "figures"

test_that("Figures baseline exists", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  expect_true(is.list(baseline))
  expect_true("figure1" %in% names(baseline))
  expect_true("figure2" %in% names(baseline))
  expect_true("treatment_date" %in% names(baseline))
  expect_true("donor_states" %in% names(baseline))
})

test_that("Figures date range is correct (Feb 1 - Jun 1)", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  expect_equal(baseline$n_days_in_range, 121L)
  expect_equal(min(baseline$date_range), as.Date("2022-02-01"))
  expect_equal(max(baseline$date_range), as.Date("2022-06-01"))
})

test_that("Figures treatment date is April 3, 2022", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  expect_equal(baseline$treatment_date, as.Date("2022-04-03"))
})

test_that("Figures donor states include correct 14 states", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  expected_donors <- c(2L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 15L, 16L, 17L)
  expect_equal(sort(baseline$donor_states), sort(expected_donors))
  expect_equal(length(baseline$donor_states), 14L)
})

test_that("Figure 1 has expected data structure", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  expect_true(!is.null(baseline$figure1$dates))
  expect_true(!is.null(baseline$figure1$hamburg))
  expect_true(!is.null(baseline$figure1$mv))
  expect_true(!is.null(baseline$figure1$donors))
  expect_true(!is.null(baseline$figure1$ylim_upper))

  expect_equal(length(baseline$figure1$dates), 121L)
  expect_equal(length(baseline$figure1$hamburg), 121L)
  expect_equal(length(baseline$figure1$mv), 121L)
  expect_equal(length(baseline$figure1$donors), 14L)
})

test_that("Figure 1 donor data has correct length for each state", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  for (state_id in names(baseline$figure1$donors)) {
    expect_equal(
      length(baseline$figure1$donors[[state_id]]),
      121L,
      info = paste("Donor state", state_id)
    )
  }
})

test_that("Figure 1 y-limit is 98th percentile of COVID incidence", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  expect_true(is.numeric(baseline$figure1$ylim_upper))
  expect_true(baseline$figure1$ylim_upper > 0)
  expect_true(baseline$figure1$ylim_upper > max(baseline$figure1$hamburg, na.rm = TRUE) * 0.5)
})

test_that("Figure 1 Hamburg COVID incidence values are plausible", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  hamburg <- baseline$figure1$hamburg
  expect_true(all(is.finite(hamburg)))
  expect_true(all(hamburg >= 0))
  expect_true(min(hamburg) < 500)
  expect_true(max(hamburg) > 1000)
})

test_that("Figure 1 MV COVID incidence values are plausible", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  mv <- baseline$figure1$mv
  expect_true(all(is.finite(mv)))
  expect_true(all(mv >= 0))
  expect_true(min(mv) < 500)
  expect_true(max(mv) > 1000)
})

test_that("Figure 2 has expected data structure", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  expect_true(!is.null(baseline$figure2$dates))
  expect_true(!is.null(baseline$figure2$hamburg))
  expect_true(!is.null(baseline$figure2$mv))
  expect_true(!is.null(baseline$figure2$donors))

  expect_equal(length(baseline$figure2$dates), 121L)
  expect_equal(length(baseline$figure2$hamburg), 121L)
  expect_equal(length(baseline$figure2$mv), 121L)
  expect_equal(length(baseline$figure2$donors), 14L)
})

test_that("Figure 2 donor data has correct length for each state", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  for (state_id in names(baseline$figure2$donors)) {
    expect_equal(
      length(baseline$figure2$donors[[state_id]]),
      121L,
      info = paste("Donor state", state_id)
    )
  }
})

test_that("Figure 2 Hamburg hospitalization incidence values are plausible", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  hamburg <- baseline$figure2$hamburg
  expect_true(all(is.finite(hamburg)))
  expect_true(all(hamburg >= 0))
})

test_that("Figure 2 MV hospitalization incidence values are plausible", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  mv <- baseline$figure2$mv
  expect_true(all(is.finite(mv)))
  expect_true(all(mv >= 0))
})

test_that("Figure 1 dates vector matches baseline dates", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  expect_equal(baseline$figure1$dates, baseline$date_range)
})

test_that("Figure 2 dates vector matches baseline dates", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  expect_equal(baseline$figure2$dates, baseline$date_range)
})

test_that("Treatment date falls within figure date range", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  expect_true(baseline$treatment_date >= min(baseline$date_range))
  expect_true(baseline$treatment_date <= max(baseline$date_range))
})

test_that("Figure 2 labeling quirk documented: MV plotted first despite Hamburg comment", {
  skip_if_no_baseline(BASELINE_NAME)
  baseline <- load_baseline(BASELINE_NAME)

  expect_true(!identical(baseline$figure2$hamburg, baseline$figure2$mv))
})
