# test-synthdata-comparison.R
# Tests verifying refactored synthdata matches original exactly

test_that("refactored synthdata matches original exactly", {
  root <- find_project_root()
  original_path <- file.path(root, "original_project/Data/processed data/synthdata.xlsx")
  refactored_path <- file.path(root, "data/processed/synthdata.parquet")

  skip_if_not(file.exists(original_path), "Original synthdata.xlsx not found")
  skip_if_not(file.exists(refactored_path), "Refactored synthdata.parquet not found")

  original <- load_original_synthdata()
  refactored <- load_refactored_synthdata()
  mapping <- get_column_mapping()

  expect_equal(nrow(refactored), nrow(original), info = "Row count mismatch")
  expect_equal(ncol(refactored), ncol(original), info = "Column count mismatch")

  expect_identical(
    as.integer(original$UnitNumeric),
    as.integer(refactored$unit_numeric),
    info = "Row order mismatch (UnitNumeric)"
  )
  expect_identical(
    as.integer(original$DateNumeric),
    as.integer(refactored$date_numeric),
    info = "Row order mismatch (DateNumeric)"
  )

  text_cols <- c("AdmUnitId", "StateId", "Name", "County type")
  date_cols <- c("Date")

  for (old_name in names(mapping)) {
    new_name <- mapping[[old_name]]
    orig_col <- original[[old_name]]
    ref_col <- refactored[[new_name]]

    if (old_name %in% text_cols) {
      orig_trimmed <- trimws(as.character(orig_col))
      ref_trimmed <- trimws(as.character(ref_col))
      orig_trimmed[is.na(orig_col)] <- NA_character_
      ref_trimmed[is.na(ref_col)] <- NA_character_
      expect_identical(
        ref_trimmed, orig_trimmed,
        info = paste("Text mismatch:", old_name)
      )
    } else if (old_name %in% date_cols) {
      expect_identical(
        as.Date(ref_col), as.Date(orig_col),
        info = paste("Date mismatch:", old_name)
      )
    } else {
      compare_numeric_column(old_name, orig_col, ref_col, tolerance = 1e-9)
    }
  }
})

test_that("refactored synthdata has expected dimensions", {
  root <- find_project_root()
  refactored_path <- file.path(root, "data/processed/synthdata.parquet")
  skip_if_not(file.exists(refactored_path), "Refactored synthdata.parquet not found")

  refactored <- load_refactored_synthdata()

  expect_equal(
    nrow(refactored), UNITS$n_total * TIME$n_days,
    info = "Expected 419 units × 245 days = 102,305 rows"
  )
  expect_equal(ncol(refactored), 56L, info = "Expected 56 columns")
})

test_that("column mapping covers all original columns", {
  root <- find_project_root()
  original_path <- file.path(root, "original_project/Data/processed data/synthdata.xlsx")
  skip_if_not(file.exists(original_path), "Original synthdata.xlsx not found")

  original <- load_original_synthdata()
  mapping <- get_column_mapping()

  original_cols <- names(original)
  mapped_cols <- names(mapping)

  missing <- setdiff(original_cols, mapped_cols)
  expect_true(
    length(missing) == 0,
    info = paste("Unmapped columns:", paste(missing, collapse = ", "))
  )
})
