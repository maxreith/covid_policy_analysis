# helper-baselines.R
# Helper functions for loading and saving analysis baselines

#' Get the path to the baselines directory
#'
#' @return Character path to tests/baselines/
get_baselines_dir <- function() {
  file.path(find_project_root(), "tests", "baselines")
}

#' Get the path to a baseline file
#'
#' @param name Baseline name (e.g., "section_411")
#' @return Character path to the baseline RDS file
get_baseline_path <- function(name) {
  file.path(get_baselines_dir(), paste0(name, "_baseline.rds"))
}

#' Check if a baseline file exists
#'
#' @param name Baseline name (e.g., "section_411")
#' @return Logical TRUE if baseline exists
baseline_exists <- function(name) {
  file.exists(get_baseline_path(name))
}

#' Load a baseline from RDS file
#'
#' @param name Baseline name (e.g., "section_411")
#' @return List containing baseline data
load_baseline <- function(name) {
  path <- get_baseline_path(name)
  if (!file.exists(path)) {
    stop(sprintf("Baseline '%s' not found at %s", name, path))
  }
  readRDS(path)
}

#' Save a baseline to RDS file
#'
#' @param name Baseline name (e.g., "section_411")
#' @param data List of baseline data to save
#' @return Invisible NULL
save_baseline <- function(name, data) {
  path <- get_baseline_path(name)
  dir <- dirname(path)
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
  saveRDS(data, path)
  invisible(NULL)
}

#' Skip test if baseline does not exist
#'
#' @param name Baseline name (e.g., "section_411")
skip_if_no_baseline <- function(name) {
  if (!baseline_exists(name)) {
    skip(sprintf("Baseline '%s' not found. Run scripts/generate_baselines.R first.", name))
  }
}

#' Skip test unless full synth test mode is enabled
#'
#' Full tests run all placebo and robustness checks, which take ~4 hours.
#' Enable with environment variable SYNTH_FULL_TEST=true
skip_if_not_full_test <- function() {
  full_test <- Sys.getenv("SYNTH_FULL_TEST", "false")
  if (!identical(tolower(full_test), "true")) {
    skip("Skipping full test (set SYNTH_FULL_TEST=true to run)")
  }
}

#' Skip test unless partial synth test mode is enabled
#'
#' Partial tests run the first 3 placebo units. Faster than full but more
#' thorough than fast mode. Enable with SYNTH_FULL_TEST=partial or
#' SYNTH_FULL_TEST=true
skip_if_not_partial_test <- function() {
  full_test <- Sys.getenv("SYNTH_FULL_TEST", "false")
  if (!tolower(full_test) %in% c("true", "partial")) {
    skip("Skipping partial test (set SYNTH_FULL_TEST=partial or true to run)")
  }
}

#' Get test mode from environment
#'
#' @return Character: "fast", "partial", or "full"
get_test_mode <- function() {
  env_val <- Sys.getenv("SYNTH_FULL_TEST", "false")
  switch(
    tolower(env_val),
    "true" = "full",
    "partial" = "partial",
    "fast"
  )
}

#' Get number of placebo units to test based on test mode
#'
#' @param total_units Total number of placebo units available
#' @return Integer number of units to test
get_placebo_test_count <- function(total_units) {
  mode <- get_test_mode()
  switch(
    mode,
    "full" = total_units,
    "partial" = min(3L, total_units),
    1L
  )
}
