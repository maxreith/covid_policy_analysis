# analysis_411_mv_covid.R
# Section 4.1.1: MV COVID Incidence Growth Rate Analysis
#
# This script runs synthetic control analysis for Mecklenburg-Vorpommern
# using 14-day COVID incidence growth rate as the dependent variable.

if (!exists("load_synthdata")) {
  source_file <- sys.frame(1)$ofile
  if (!is.null(source_file)) {
    source(file.path(dirname(source_file), "synth_helpers.R"))
  }
}

main <- function(save_outputs = TRUE, verbose = FALSE, use_parquet = TRUE) {
  config <- get_analysis_config()

  if (verbose) message("Loading synthdata...")
  synthdata <- load_synthdata(use_parquet = use_parquet)

  pool <- get_state_pool(synthdata)
  treated_unit <- pool |>
    filter(Name == config$treated_name) |>
    pull(UnitNumeric)

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

  results$table1 <- format_predictor_table(results$synth_tables)
  results$table2 <- format_weights_table(results$synth_tables)

  if (save_outputs) {
    save_analysis_outputs(results, synthdata, config)
  }

  results
}

get_analysis_config <- function() {
  list(
    name = "section_411",
    treated_name = "Mecklenburg-Vorpommern",
    var_dependent = "14 days covid incidence growth rate",
    predictor_days = c(65L, 79L, 93L),
    figure_prefix = "MV Growth Rate"
  )
}

save_analysis_outputs <- function(results, synthdata, config) {
  root <- find_project_root()
  output_dir <- file.path(root, "results")
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  arrow::write_parquet(
    results$table1,
    file.path(output_dir, "table1_mv_covid_predictors.parquet")
  )
  arrow::write_parquet(
    results$table2,
    file.path(output_dir, "table2_mv_covid_weights.parquet")
  )
}

if (sys.nframe() == 0) {
  results <- main(save_outputs = TRUE, verbose = TRUE)
  message("Analysis complete.")
}
