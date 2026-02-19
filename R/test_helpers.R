# test_helpers.R
# Helper functions for testing synthdata comparison

#' Find the project root directory
#'
#' @return Path to project root (directory containing R/)
find_project_root <- function() {
  dir <- getwd()
  while (!dir.exists(file.path(dir, "R"))) {
    parent <- dirname(dir)
    if (parent == dir) stop("Could not find project root")
    dir <- parent
  }
  dir
}

#' Get column types for reading original synthdata.xlsx
#'
#' @return Character vector of column types for readxl
get_original_col_types <- function() {
  c(
    "numeric",   # UnitNumeric
    "text",      # AdmUnitId
    "text",      # StateId
    "text",      # Name
    "numeric",   # DateNumeric
    "date",      # Date
    "numeric",   # AnzFallNeu
    "numeric",   # AnzFallVortag
    "numeric",   # AnzFallErkrankung
    "numeric",   # AnzFallMeldung
    "numeric",   # KumFall
    "numeric",   # covid incidence
    "numeric",   # incidence growth rate
    "numeric",   # 14 days covid incidence
    "numeric",   # 14 days covid incidence growth rate
    "numeric",   # Area in Square Kilometers
    "numeric",   # Population
    "numeric",   # Male Population
    "numeric",   # Female Population
    "numeric",   # Population Density
    "numeric",   # Unemployed
    "numeric",   # Unemployed Foreigners
    "numeric",   # Unemployed Age 15-20
    "numeric",   # Unemployed Age 15-25
    "numeric",   # Unemployed Age 55-65
    "numeric",   # Long-term Unemployed
    "numeric",   # Unemployment rate in relation to employed labor force
    "numeric",   # Unemployment rate in relation to total labor force
    "numeric",   # Unemployment rate of men...
    "numeric",   # Unemployment rate of women...
    "numeric",   # Unemployment rate of foreigners...
    "numeric",   # Unemployment rate of people aged 15-25...
    "numeric",   # First dose vaccinations
    "numeric",   # Second dose vaccinations
    "numeric",   # Third dose vaccinations
    "numeric",   # Fourth dose vaccinations
    "numeric",   # Fifth dose vaccinations
    "numeric",   # Sixth dose vaccinations
    "numeric",   # Hospitalizations 00+
    "numeric",   # Hospitalizations 00-04
    "numeric",   # Hospitalizations 05-14
    "numeric",   # Hospitalizations 15-34
    "numeric",   # Hospitalizations 35-59
    "numeric",   # Hospitalizations 60-79
    "numeric",   # Hospitalizations 80+
    "numeric",   # Hospitalization incidence 00+
    "numeric",   # Hospitalization incidence 00-04
    "numeric",   # Hospitalization incidence 05-14
    "numeric",   # Hospitalization incidence 15-34
    "numeric",   # Hospitalization incidence 35-59
    "numeric",   # Hospitalization incidence 60-79
    "numeric",   # Hospitalization incidence 80+
    "numeric",   # hospitalization inc. growth rate
    "numeric",   # 14 days hospitalization incidence
    "numeric",   # 14 days hospitalization incidence growth rate
    "text"       # County type
  )
}

#' Load original synthdata from Excel
#'
#' @return Data frame with original synthdata
load_original_synthdata <- function() {
  path <- file.path(
    find_project_root(),
    "original_project/Data/processed data/synthdata.xlsx"
  )
  readxl::read_excel(path, col_types = get_original_col_types())
}

#' Load refactored synthdata from parquet
#'
#' @return Data frame with refactored synthdata
load_refactored_synthdata <- function() {
  path <- file.path(
    find_project_root(),
    "data/processed/synthdata.parquet"
  )
  arrow::read_parquet(path)
}

#' Get mapping from original column names to new snake_case names
#'
#' @return Named list mapping old names to new names
get_column_mapping <- function() {
  list(
    "UnitNumeric" = "unit_numeric",
    "AdmUnitId" = "adm_unit_id",
    "StateId" = "state_id",
    "Name" = "name",
    "DateNumeric" = "date_numeric",
    "Date" = "date",
    "AnzFallNeu" = "cases_new",
    "AnzFallVortag" = "cases_previous_day",
    "AnzFallErkrankung" = "cases_by_onset",
    "AnzFallMeldung" = "cases_by_report",
    "KumFall" = "cases_cumulative",
    "covid incidence" = "covid_incidence_7d",
    "incidence growth rate" = "covid_incidence_growth_rate_7d",
    "14 days covid incidence" = "covid_incidence_14d",
    "14 days covid incidence growth rate" = "covid_incidence_growth_rate_14d",
    "Area in Square Kilometers" = "area_sq_km",
    "Population" = "population",
    "Male Population" = "population_male",
    "Female Population" = "population_female",
    "Population Density" = "population_density",
    "Unemployed" = "unemployed",
    "Unemployed Foreigners" = "unemployed_foreigners",
    "Unemployed Age 15-20" = "unemployed_age_15_20",
    "Unemployed Age 15-25" = "unemployed_age_15_25",
    "Unemployed Age 55-65" = "unemployed_age_55_65",
    "Long-term Unemployed" = "unemployed_long_term",
    "Unemployment rate in relation to employed labor force" = "unemployment_rate_employed",
    "Unemployment rate in relation to total labor force" = "unemployment_rate_total",
    "Unemployment rate of men in relation to total male labor force" = "unemployment_rate_male",
    "Unemployment rate of women in relation to total female labor force" = "unemployment_rate_female",
    "Unemployment rate of foreigners in relation to total foreign labor force" = "unemployment_rate_foreigners",
    "Unemployment rate of people aged 15-25 in relation to total labor force aged 15-25" = "unemployment_rate_youth",
    "First dose vaccinations" = "vaccination_dose_1",
    "Second dose vaccinations" = "vaccination_dose_2",
    "Third dose vaccinations" = "vaccination_dose_3",
    "Fourth dose vaccinations" = "vaccination_dose_4",
    "Fifth dose vaccinations" = "vaccination_dose_5",
    "Sixth dose vaccinations" = "vaccination_dose_6",
    "Hospitalizations 00+" = "hospitalizations_00plus",
    "Hospitalizations 00-04" = "hospitalizations_00_04",
    "Hospitalizations 05-14" = "hospitalizations_05_14",
    "Hospitalizations 15-34" = "hospitalizations_15_34",
    "Hospitalizations 35-59" = "hospitalizations_35_59",
    "Hospitalizations 60-79" = "hospitalizations_60_79",
    "Hospitalizations 80+" = "hospitalizations_80plus",
    "Hospitalization incidence 00+" = "hospitalization_incidence_00plus",
    "Hospitalization incidence 00-04" = "hospitalization_incidence_00_04",
    "Hospitalization incidence 05-14" = "hospitalization_incidence_05_14",
    "Hospitalization incidence 15-34" = "hospitalization_incidence_15_34",
    "Hospitalization incidence 35-59" = "hospitalization_incidence_35_59",
    "Hospitalization incidence 60-79" = "hospitalization_incidence_60_79",
    "Hospitalization incidence 80+" = "hospitalization_incidence_80plus",
    "hospitalization inc. growth rate" = "hospitalization_incidence_growth_rate_7d",
    "14 days hospitalization incidence" = "hospitalization_incidence_14d",
    "14 days hospitalization incidence growth rate" = "hospitalization_incidence_growth_rate_14d",
    "County type" = "county_type"
  )
}

#' Compare numeric columns with proper NA/Inf/tolerance handling
#'
#' @param col_name Name of the column (for error messages)
#' @param original Original column values
#' @param refactored Refactored column values
#' @param tolerance Numeric tolerance for comparison (default 1e-9)
#' @return TRUE if columns match, otherwise stops with error
compare_numeric_column <- function(col_name, original, refactored, tolerance = 1e-9) {
  both_na <- is.na(original) & is.na(refactored)
  orig_na_only <- is.na(original) & !is.na(refactored)
  ref_na_only <- !is.na(original) & is.na(refactored)

  if (any(orig_na_only)) {
    stop(sprintf(
      "Column '%s': %d values are NA in original but not refactored",
      col_name, sum(orig_na_only)
    ))
  }
  if (any(ref_na_only)) {
    stop(sprintf(
      "Column '%s': %d values are NA in refactored but not original",
      col_name, sum(ref_na_only)
    ))
  }

  both_finite <- !both_na & is.finite(original) & is.finite(refactored)
  diff <- abs(original - refactored)
  exceeds_tol <- both_finite & (diff > tolerance)

  if (any(exceeds_tol)) {
    first_idx <- which(exceeds_tol)[1]
    stop(sprintf(
      "Column '%s': %d mismatches. First at row %d: original=%.10g, refactored=%.10g, diff=%.2e",
      col_name, sum(exceeds_tol), first_idx,
      original[first_idx], refactored[first_idx], diff[first_idx]
    ))
  }

  TRUE
}
