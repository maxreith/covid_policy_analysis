# analysis_421_mv_hosp.R
# Section 4.2.1: MV Hospitalization Growth Rate Analysis
#
# This script runs synthetic control analysis for Mecklenburg-Vorpommern
# using 14-day hospitalization incidence growth rate as the dependent variable.

if (!exists("load_synthdata")) {
  source_file <- sys.frame(1)$ofile
  if (!is.null(source_file)) {
    source(file.path(dirname(source_file), "synth_helpers.R"))
  }
}

#' Run Section 4.2.1 analysis.
#'
#' @param save_outputs Logical, whether to save tables/figures (default TRUE)
#' @param verbose Logical, print progress (default FALSE)
#' @param use_parquet Logical, load from parquet vs Excel (default TRUE)
#' @return List with all analysis results for baseline
main <- function(save_outputs = TRUE, verbose = FALSE, use_parquet = TRUE) {
  config <- get_analysis_config()

  if (verbose) message("Loading synthdata...")
  synthdata <- load_synthdata(use_parquet = use_parquet)

  pool <- get_state_pool(synthdata)
  treated_unit <- pool$UnitNumeric[pool$Name == config$treated_name]

  var_special_predictors <- build_predictors(
    config$var_dependent,
    config$predictor_days
  )

  results <- run_complete_analysis(
    synthdata = synthdata,
    pool = pool,
    treated_unit = treated_unit,
    var_dependent = config$var_dependent,
    var_special_predictors = var_special_predictors,
    run_robustness = TRUE,
    verbose = verbose
  )

  results$table5 <- format_predictor_table(results$synth_tables)
  results$table6 <- format_weights_table(results$synth_tables)

  if (save_outputs) {
    save_analysis_outputs(results, synthdata, config)
  }

  results
}

#' Get configuration for this analysis.
#'
#' @return List with analysis configuration
get_analysis_config <- function() {
  list(
    name = "section_421",
    treated_name = "Mecklenburg-Vorpommern",
    var_dependent = "14 days hospitalization incidence growth rate",
    predictor_days = c(65L, 72L, 93L),
    figure_prefix = "MV Hospitalization Growth Rate"
  )
}

#' Save analysis outputs (tables and figures).
#'
#' @param results Analysis results
#' @param synthdata Data frame with synthdata
#' @param config Analysis configuration
save_analysis_outputs <- function(results, synthdata, config) {
  root <- find_project_root()
  output_dir <- file.path(root, "results")
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  arrow::write_parquet(
    results$table5,
    file.path(output_dir, "table5_mv_hosp_predictors.parquet")
  )
  arrow::write_parquet(
    results$table6,
    file.path(output_dir, "table6_mv_hosp_weights.parquet")
  )
}

if (sys.nframe() == 0) {
  results <- main(save_outputs = TRUE, verbose = TRUE)
  message("Analysis complete.")
}
