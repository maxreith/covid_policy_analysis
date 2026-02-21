# analysis_43_mv_cities.R
# Section 4.3: MV Cities Municipality-Level Analysis
#
# This script runs synthetic control analysis for aggregated MV cities (unit 419)
# using 14-day COVID incidence growth rate as the dependent variable.
# Donor pool: cities in states 1, 3, 12, 15 (Schleswig-Holstein, Hamburg, Berlin, Saxony).
# Note: This analysis does NOT run robustness checks (leave-one-out donor/predictor).

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

  pool <- get_mv_cities_pool(synthdata)
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
    run_robustness = FALSE,
    verbose = verbose
  )

  results$table9 <- format_predictor_table(results$synth_tables)
  results$table10 <- format_weights_table(results$synth_tables)

  if (save_outputs) {
    save_analysis_outputs(results, synthdata, config)
  }

  results
}

get_analysis_config <- function() {
  list(
    name = "section_43",
    treated_name = "SK MV aggregated",
    var_dependent = "14 days covid incidence growth rate",
    predictor_days = c(65L, 72L, 93L),
    figure_prefix = "MV Cities Growth Rate"
  )
}

save_analysis_outputs <- function(results, synthdata, config) {
  root <- find_project_root()
  output_dir <- file.path(root, "results")
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  arrow::write_parquet(
    results$table9,
    file.path(output_dir, "table9_mv_cities_predictors.parquet")
  )
  arrow::write_parquet(
    results$table10,
    file.path(output_dir, "table10_mv_cities_weights.parquet")
  )
}

if (sys.nframe() == 0) {
  results <- main(save_outputs = TRUE, verbose = TRUE)
  message("Analysis complete.")
}
