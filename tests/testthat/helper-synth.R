# helper-synth.R
# Helper functions for comparing synthetic control results

#' Compare two numeric matrices or vectors with tolerance
#'
#' @param actual Actual values (matrix or vector)
#' @param expected Expected values (matrix or vector)
#' @param tolerance Numeric tolerance for comparison
#' @param name Name for error messages
#' @return TRUE if all values match within tolerance, stops with error otherwise
compare_numeric_values <- function(actual, expected, tolerance, name = "values") {
  if (!identical(dim(actual), dim(expected))) {
    stop(sprintf(
      "%s: dimension mismatch. Actual: %s, Expected: %s",
      name,
      paste(dim(actual), collapse = "x"),
      paste(dim(expected), collapse = "x")
    ))
  }

  actual_vec <- as.vector(actual)
  expected_vec <- as.vector(expected)

  both_na <- is.na(actual_vec) & is.na(expected_vec)
  actual_na_only <- is.na(actual_vec) & !is.na(expected_vec)
  expected_na_only <- !is.na(actual_vec) & is.na(expected_vec)

  if (any(actual_na_only)) {
    stop(sprintf("%s: %d values are NA in actual but not expected", name, sum(actual_na_only)))
  }
  if (any(expected_na_only)) {
    stop(sprintf("%s: %d values are NA in expected but not actual", name, sum(expected_na_only)))
  }

  both_finite <- !both_na & is.finite(actual_vec) & is.finite(expected_vec)
  diff <- abs(actual_vec - expected_vec)
  exceeds_tol <- both_finite & (diff > tolerance)

  if (any(exceeds_tol)) {
    first_idx <- which(exceeds_tol)[1]
    max_diff_idx <- which.max(diff * exceeds_tol)
    stop(sprintf(
      "%s: %d mismatches (tol=%.1e). First at idx %d: actual=%.10g, expected=%.10g, diff=%.2e. Max diff at idx %d: %.2e",
      name, sum(exceeds_tol), tolerance, first_idx,
      actual_vec[first_idx], expected_vec[first_idx], diff[first_idx],
      max_diff_idx, diff[max_diff_idx]
    ))
  }

  TRUE
}

#' Compare dataprep output objects
#'
#' Compares Y1plot (treated outcomes) and Y0plot (donor outcomes) matrices.
#'
#' @param actual Actual dataprep.out object
#' @param expected Expected dataprep.out object
#' @param tolerance Numeric tolerance (default 1e-10 for direct data extraction)
#' @return TRUE if all components match
compare_dataprep_out <- function(actual, expected, tolerance = 1e-10) {
  compare_numeric_values(actual$Y1plot, expected$Y1plot, tolerance, "Y1plot")
  compare_numeric_values(actual$Y0plot, expected$Y0plot, tolerance, "Y0plot")
  TRUE
}

#' Compare synth output objects
#'
#' Compares solution.w (donor weights) and solution.v (predictor weights).
#'
#' @param actual Actual synth.out object
#' @param expected Expected synth.out object
#' @param tolerance Numeric tolerance (default 1e-6 for optimization results)
#' @return TRUE if all components match
compare_synth_out <- function(actual, expected, tolerance = 1e-6) {
  compare_numeric_values(
    actual$solution.w, expected$solution.w,
    tolerance, "solution.w"
  )
  compare_numeric_values(
    actual$solution.v, expected$solution.v,
    tolerance, "solution.v"
  )
  TRUE
}

#' Compare synth.tab output objects
#'
#' Compares tab.pred (predictor balance), tab.w (weights), tab.v, and tab.loss.
#'
#' @param actual Actual synth.tables object
#' @param expected Expected synth.tables object
#' @param tolerance Numeric tolerance (default 1e-8 for derived calculations)
#' @return TRUE if all components match
compare_synth_tables <- function(actual, expected, tolerance = 1e-8) {
  actual_pred <- as.matrix(actual$tab.pred)
  expected_pred <- as.matrix(expected$tab.pred)
  compare_numeric_values(actual_pred, expected_pred, tolerance, "tab.pred")

  actual_w <- as.matrix(actual$tab.w)
  expected_w <- as.matrix(expected$tab.w)
  compare_numeric_values(actual_w, expected_w, tolerance, "tab.w")

  actual_v <- as.numeric(unlist(actual$tab.v))
  expected_v <- as.numeric(unlist(expected$tab.v))
  compare_numeric_values(actual_v, expected_v, tolerance, "tab.v")

  actual_loss <- as.numeric(actual$tab.loss)
  expected_loss <- as.numeric(expected$tab.loss)
  compare_numeric_values(actual_loss, expected_loss, tolerance, "tab.loss")

  TRUE
}

#' Compare a single placebo result
#'
#' Compares Y1, Y0, synth.out, and synth.tables from a placebo estimation.
#'
#' @param actual Actual placebo result list
#' @param expected Expected placebo result list
#' @param name Name for error messages
#' @param dataprep_tol Tolerance for Y1/Y0 comparison
#' @param synth_tol Tolerance for synth.out comparison
#' @param table_tol Tolerance for synth.tables comparison
#' @return TRUE if all components match
compare_placebo_result <- function(
  actual,
  expected,
  name = "placebo",
  dataprep_tol = 1e-10,
  synth_tol = 1e-6,
  table_tol = 1e-8
) {
  compare_numeric_values(actual$Y1, expected$Y1, dataprep_tol, paste(name, "Y1"))
  compare_numeric_values(actual$Y0, expected$Y0, dataprep_tol, paste(name, "Y0"))
  compare_synth_out(actual$synth.out, expected$synth.out, synth_tol)
  compare_synth_tables(actual$synth.tables, expected$synth.tables, table_tol)
  TRUE
}

#' Compare placebo list results
#'
#' Compares full placebo test results stored in a named list.
#'
#' @param actual Actual placebo list
#' @param expected Expected placebo list
#' @param n_compare Number of placebo results to compare (NULL = all)
#' @param dataprep_tol Tolerance for Y1/Y0 comparison
#' @param synth_tol Tolerance for synth.out comparison
#' @param table_tol Tolerance for synth.tables comparison
#' @return TRUE if all compared components match
compare_placebo_list <- function(
  actual,
  expected,
  n_compare = NULL,
  dataprep_tol = 1e-10,
  synth_tol = 1e-6,
  table_tol = 1e-8
) {
  if (is.null(n_compare)) {
    n_compare <- length(expected)
  }
  n_compare <- min(n_compare, length(expected), length(actual))

  if (n_compare == 0) {
    return(TRUE)
  }

  for (i in seq_len(n_compare)) {
    name <- names(expected)[i]
    if (is.null(name)) name <- as.character(i)

    compare_placebo_result(
      actual[[i]],
      expected[[i]],
      name = paste("placebo", name),
      dataprep_tol = dataprep_tol,
      synth_tol = synth_tol,
      table_tol = table_tol
    )
  }
  TRUE
}

#' Compare Post/Pre MSPE ratio vectors
#'
#' @param actual Actual PostPre vector
#' @param expected Expected PostPre vector
#' @param tolerance Numeric tolerance (default 1e-6)
#' @param n_compare Number of values to compare (NULL = all)
#' @return TRUE if values match
compare_post_pre <- function(actual, expected, tolerance = 1e-6, n_compare = NULL) {
  if (is.null(n_compare)) {
    n_compare <- length(expected)
  }
  n_compare <- min(n_compare, length(expected), length(actual))

  compare_numeric_values(
    actual[seq_len(n_compare)],
    expected[seq_len(n_compare)],
    tolerance,
    "PostPre MSPE"
  )
  TRUE
}

#' Compare table output data frames
#'
#' Compares a refactored output table (potentially with different column names)
#' against a baseline table.
#'
#' @param actual Actual data frame
#' @param expected Expected data frame
#' @param col_mapping Optional named vector mapping actual column names to expected
#' @param tolerance Numeric tolerance for numeric columns
#' @return TRUE if tables match
compare_table_output <- function(actual, expected, col_mapping = NULL, tolerance = 1e-8) {
  if (!identical(nrow(actual), nrow(expected))) {
    stop(sprintf(
      "Table row count mismatch. Actual: %d, Expected: %d",
      nrow(actual), nrow(expected)
    ))
  }

  if (!is.null(col_mapping)) {
    actual_cols <- names(col_mapping)
    expected_cols <- unname(col_mapping)
  } else {
    actual_cols <- names(actual)
    expected_cols <- names(expected)
  }

  for (i in seq_along(actual_cols)) {
    actual_col <- actual_cols[i]
    expected_col <- expected_cols[i]

    if (!actual_col %in% names(actual)) {
      stop(sprintf("Column '%s' not found in actual table", actual_col))
    }
    if (!expected_col %in% names(expected)) {
      stop(sprintf("Column '%s' not found in expected table", expected_col))
    }

    actual_vals <- actual[[actual_col]]
    expected_vals <- expected[[expected_col]]

    if (is.numeric(actual_vals) && is.numeric(expected_vals)) {
      compare_numeric_values(
        actual_vals, expected_vals, tolerance,
        paste("Column", actual_col)
      )
    } else {
      if (!identical(as.character(actual_vals), as.character(expected_vals))) {
        stop(sprintf(
          "Column '%s' text mismatch",
          actual_col
        ))
      }
    }
  }
  TRUE
}

#' Compare robustness check results (leave-one-out donor or predictor)
#'
#' @param actual Actual robustness list
#' @param expected Expected robustness list
#' @param n_compare Number of results to compare (NULL = all)
#' @param dataprep_tol Tolerance for Y1/Y0 comparison
#' @param synth_tol Tolerance for synth.out comparison
#' @param table_tol Tolerance for synth.tables comparison
#' @return TRUE if all compared components match
compare_robustness_list <- function(
  actual,
  expected,
  n_compare = NULL,
  dataprep_tol = 1e-10,
  synth_tol = 1e-6,
  table_tol = 1e-8
) {
  compare_placebo_list(
    actual, expected, n_compare,
    dataprep_tol, synth_tol, table_tol
  )
}
