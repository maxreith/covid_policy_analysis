# test-placeholder.R
# Placeholder test to verify testthat setup works

test_that("testthat is configured correctly", {
  expect_true(TRUE)
})

test_that("config.R loads without errors", {
  expect_true(exists("PATHS"))
  expect_true(exists("TIME"))
  expect_true(exists("UNITS"))
  expect_true(exists("SYNTH"))
})
