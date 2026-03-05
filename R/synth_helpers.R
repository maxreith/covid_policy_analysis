# synth_helpers.R
# Shared helper functions for synthetic control analyses

library(Synth)
library(dplyr)
library(purrr)

.find_project_root_local <- function() {
  dir <- getwd()
  while (!dir.exists(file.path(dir, "R"))) {
    parent <- dirname(dir)
    if (parent == dir) stop("Could not find project root")
    dir <- parent
  }
  dir
}

if (!exists("find_project_root")) {
  source(file.path(.find_project_root_local(), "R/utils.R"))
}

source(file.path(find_project_root(), "R/config.R"))


#' Load synthdata from parquet or Excel file.
#'
#' Loads the processed synthdata and adds the UnitNumeric column which assigns
#' sequential integer IDs to each regional unit.
#'
#' @param use_parquet Logical. If TRUE (default), loads from parquet file.
#'   If FALSE, loads from original Excel file.
#' @return Data frame with 56 columns including UnitNumeric.
load_synthdata <- function(use_parquet = TRUE) {
  root <- find_project_root()

  if (use_parquet) {
    path <- file.path(root, "data/processed/synthdata.parquet")
    synthdata <- arrow::read_parquet(path)
    synthdata <- as.data.frame(synthdata)
  } else {
    path <- file.path(root, "original_project/Data/processed data/synthdata.xlsx")
    col_types <- c(
      "numeric", "text", "text", "text", "numeric", "date",
      rep("numeric", 49), "text"
    )
    synthdata <- readxl::read_excel(path, col_types = col_types)
  }

  n_days <- TIME$n_days
  n_units <- nrow(synthdata) / n_days

  unit_numeric <- rep(seq_len(n_units), each = n_days)
  synthdata2 <- data.frame(UnitNumeric = unit_numeric)
  synthdata2[, 2:56] <- synthdata[, 2:56]
  colnames(synthdata2)[2:56] <- colnames(synthdata)[2:56]
  synthdata2$Date <- as.Date(synthdata2$Date)

  synthdata2
}


#' Get optimization and plot periods as DateNumeric vectors.
#'
#' Converts date ranges into DateNumeric values for use with the Synth package.
#'
#' @param synthdata Data frame containing DateNumeric and Date columns.
#' @param optimization_start Start date for optimization period (default from config).
#' @param optimization_end End date for optimization period (default from config).
#' @param plot_start Start date for plotting period (default from config).
#' @param plot_end End date for plotting period (default from config).
#' @return Named list with 'optimization_period' and 'plot_period' integer vectors.
get_date_periods <- function(
    synthdata,
    optimization_start = SYNTH$optimization_start,
    optimization_end = SYNTH$optimization_end,
    plot_start = SYNTH$plot_start,
    plot_end = SYNTH$plot_end) {
  optimization_dates <- seq(from = optimization_start, to = optimization_end, by = "day")
  optimization_period <- synthdata |>
    filter(UnitNumeric == 1, Date %in% optimization_dates) |>
    pull(DateNumeric)

  plot_dates <- seq(from = plot_start, to = plot_end, by = "day")
  plot_period <- synthdata |>
    filter(UnitNumeric == 1, Date %in% plot_dates) |>
    pull(DateNumeric)

  list(
    optimization_period = optimization_period,
    plot_period = plot_period
  )
}


#' Get time predictors prior period for synthetic control.
#'
#' Returns the DateNumeric values for a 14-day period starting March 18, 2022,
#' which is used as the time.predictors.prior argument in dataprep().
#'
#' @param synthdata Data frame containing AdmUnitId, Date, and DateNumeric columns.
#' @return Integer vector of DateNumeric values.
get_time_predictors_prior <- function(synthdata) {
  synthdata |>
    filter(
      AdmUnitId == "0",
      Date %in% seq(as.Date("2022-03-18"), by = "day", length.out = 14)
    ) |>
    pull(DateNumeric)
}


#' Run synthetic control estimation for a single treated unit.
#'
#' Wrapper around Synth::dataprep() and Synth::synth() that handles the
#' standard setup for this project's analyses.
#'
#' @param synthdata Data frame with panel data.
#' @param treated_unit Integer. UnitNumeric value of the treated unit.
#' @param controls Integer vector. UnitNumeric values of control units.
#' @param var_dependent Character. Name of the dependent variable column.
#' @param var_special_predictors List of special predictor specifications.
#' @param optimization_period Integer vector. DateNumeric values for optimization.
#' @param plot_period Integer vector. DateNumeric values for plotting.
#' @param method Character. Optimization method (default "BFGS").
#' @return Named list with 'dataprep_out', 'synth_out', and 'synth_tables'.
run_synth <- function(
    synthdata,
    treated_unit,
    controls,
    var_dependent,
    var_special_predictors,
    optimization_period,
    plot_period,
    method = "BFGS") {
  if (length(controls) == 0) {
    stop("controls must contain at least one unit")
  }

  time_predictors_prior <- get_time_predictors_prior(synthdata)

  dataprep_out <- Synth::dataprep(
    foo = synthdata,
    special.predictors = var_special_predictors,
    time.predictors.prior = time_predictors_prior,
    dependent = var_dependent,
    unit.variable = "UnitNumeric",
    unit.names.variable = "Name",
    time.variable = "DateNumeric",
    treatment.identifier = treated_unit,
    controls.identifier = controls,
    time.optimize.ssr = optimization_period,
    time.plot = plot_period
  )

  synth_out <- Synth::synth(data.prep.obj = dataprep_out, method = method)
  synth_tables <- Synth::synth.tab(dataprep.res = dataprep_out, synth.res = synth_out)

  list(
    dataprep_out = dataprep_out,
    synth_out = synth_out,
    synth_tables = synth_tables
  )
}


#' Run placebo tests across all units in the donor pool.
#'
#' For each unit in the pool (except the treated unit), runs a synthetic control
#' estimation treating that unit as treated and all others as controls.
#'
#' @param synthdata Data frame with panel data.
#' @param pool Data frame with UnitNumeric column identifying pool units.
#' @param treated_unit Integer. UnitNumeric of the actual treated unit.
#' @param var_dependent Character. Name of the dependent variable column.
#' @param var_special_predictors List of special predictor specifications.
#' @param optimization_period Integer vector. DateNumeric values for optimization.
#' @param plot_period Integer vector. DateNumeric values for plotting.
#' @param original_result List. Result from run_synth() for the original analysis.
#' @param mspe_restrict Numeric. MSPE restriction multiplier (unused, kept for compatibility).
#' @param verbose Logical. If TRUE, prints progress messages.
#' @return Named list of results, keyed by unit number or "original treatment unit".
run_placebo_tests <- function(
    synthdata,
    pool,
    treated_unit,
    var_dependent,
    var_special_predictors,
    optimization_period,
    plot_period,
    original_result,
    mspe_restrict = 5,
    verbose = FALSE) {
  placebo_list <- list()

  original_entry <- list(
    Y1 = pluck(original_result, "dataprep_out", "Y1plot"),
    Y0 = pluck(original_result, "dataprep_out", "Y0plot"),
    synth.out = pluck(original_result, "synth_out"),
    synth.tables = pluck(original_result, "synth_tables")
  )
  placebo_list[["original treatment unit"]] <- original_entry

  for (unit in pull(pool, UnitNumeric)) {
    if (unit == treated_unit) next

    controls <- pool |>
      filter(UnitNumeric != unit) |>
      pull(UnitNumeric)

    result <- run_synth(
      synthdata = synthdata,
      treated_unit = unit,
      controls = controls,
      var_dependent = var_dependent,
      var_special_predictors = var_special_predictors,
      optimization_period = optimization_period,
      plot_period = plot_period
    )

    entry <- list(
      Y1 = pluck(result, "dataprep_out", "Y1plot"),
      Y0 = pluck(result, "dataprep_out", "Y0plot"),
      synth.out = pluck(result, "synth_out"),
      synth.tables = pluck(result, "synth_tables")
    )
    placebo_list[[as.character(unit)]] <- entry

    if (verbose) message("Completed placebo for unit ", unit)
  }

  placebo_list
}


#' Run leave-one-out donor robustness checks.
#'
#' For each donor with non-zero weight, re-estimates the synthetic control
#' excluding that donor from the pool.
#'
#' @param synthdata Data frame with panel data.
#' @param pool Data frame with UnitNumeric column identifying pool units.
#' @param treated_unit Integer. UnitNumeric of the treated unit.
#' @param var_dependent Character. Name of the dependent variable column.
#' @param var_special_predictors List of special predictor specifications.
#' @param optimization_period Integer vector. DateNumeric values for optimization.
#' @param plot_period Integer vector. DateNumeric values for plotting.
#' @param original_result List. Result from run_synth() for the original analysis.
#' @param verbose Logical. If TRUE, prints progress messages.
#' @return Named list of results, keyed by dropped unit or "original treatment unit".
run_leave_one_out_donors <- function(
    synthdata,
    pool,
    treated_unit,
    var_dependent,
    var_special_predictors,
    optimization_period,
    plot_period,
    original_result,
    verbose = FALSE) {
  robustness_list <- list()

  original_entry <- list(
    Y1 = pluck(original_result, "dataprep_out", "Y1plot"),
    Y0 = pluck(original_result, "dataprep_out", "Y0plot"),
    synth.out = pluck(original_result, "synth_out"),
    synth.tables = pluck(original_result, "synth_tables")
  )
  robustness_list[["original treatment unit"]] <- original_entry

  original_weights <- round(pluck(original_result, "synth_out", "solution.w"), digits = 4)
  donor_units <- pool |>
    filter(UnitNumeric != treated_unit) |>
    pull(UnitNumeric)

  for (i in seq_along(donor_units)) {
    unit <- donor_units[i]

    if (original_weights[i, ] == 0) next

    controls <- donor_units[donor_units != unit]

    result <- run_synth(
      synthdata = synthdata,
      treated_unit = treated_unit,
      controls = controls,
      var_dependent = var_dependent,
      var_special_predictors = var_special_predictors,
      optimization_period = optimization_period,
      plot_period = plot_period
    )

    entry <- list(
      Y1 = pluck(result, "dataprep_out", "Y1plot"),
      Y0 = pluck(result, "dataprep_out", "Y0plot"),
      synth.out = pluck(result, "synth_out"),
      synth.tables = pluck(result, "synth_tables")
    )
    robustness_list[[paste(as.character(unit), " dropped")]] <- entry

    if (verbose) message("Completed leave-one-out for donor ", unit)
  }

  robustness_list
}


#' Run leave-one-out predictor robustness checks.
#'
#' For each predictor with non-zero weight, re-estimates the synthetic control
#' excluding that predictor.
#'
#' @param synthdata Data frame with panel data.
#' @param pool Data frame with UnitNumeric column identifying pool units.
#' @param treated_unit Integer. UnitNumeric of the treated unit.
#' @param var_dependent Character. Name of the dependent variable column.
#' @param var_special_predictors List of special predictor specifications.
#' @param optimization_period Integer vector. DateNumeric values for optimization.
#' @param plot_period Integer vector. DateNumeric values for plotting.
#' @param original_result List. Result from run_synth() for the original analysis.
#' @param verbose Logical. If TRUE, prints progress messages.
#' @return Named list of results, keyed by dropped predictor or "original treatment unit".
run_leave_one_out_predictors <- function(
    synthdata,
    pool,
    treated_unit,
    var_dependent,
    var_special_predictors,
    optimization_period,
    plot_period,
    original_result,
    verbose = FALSE) {
  robustness_list <- list()

  original_entry <- list(
    Y1 = pluck(original_result, "dataprep_out", "Y1plot"),
    Y0 = pluck(original_result, "dataprep_out", "Y0plot"),
    synth.out = pluck(original_result, "synth_out"),
    synth.tables = pluck(original_result, "synth_tables")
  )
  robustness_list[["original treatment unit"]] <- original_entry

  original_v_weights <- round(
    as.numeric(pluck(original_result, "synth_tables", "tab.v")[, 1]),
    digits = 4
  )
  controls <- pool |>
    filter(UnitNumeric != treated_unit) |>
    pull(UnitNumeric)

  for (predictor_num in seq_along(var_special_predictors)) {
    if (original_v_weights[predictor_num] == 0) next

    predictors_subset <- var_special_predictors[-predictor_num]

    result <- run_synth(
      synthdata = synthdata,
      treated_unit = treated_unit,
      controls = controls,
      var_dependent = var_dependent,
      var_special_predictors = predictors_subset,
      optimization_period = optimization_period,
      plot_period = plot_period
    )

    entry <- list(
      Y1 = pluck(result, "dataprep_out", "Y1plot"),
      Y0 = pluck(result, "dataprep_out", "Y0plot"),
      synth.out = pluck(result, "synth_out"),
      synth.tables = pluck(result, "synth_tables")
    )
    robustness_list[[paste(as.character(predictor_num), " dropped")]] <- entry

    if (verbose) message("Completed leave-one-out for predictor ", predictor_num)
  }

  robustness_list
}


#' Calculate post-treatment/pre-treatment MSPE ratios.
#'
#' For each unit in the placebo list, computes the ratio of mean squared
#' prediction error in the post-treatment period to the pre-treatment period.
#'
#' @param placebo_list Named list of placebo results from run_placebo_tests().
#' @param optimization_period Integer vector. DateNumeric values for pre-treatment period.
#' @param plot_period Integer vector. DateNumeric values for full period.
#' @return Numeric vector of Post-MSPE/Pre-MSPE ratios.
calculate_post_pre_mspe <- function(placebo_list, optimization_period, plot_period) {
  if (length(placebo_list) == 0) {
    stop("placebo_list must contain at least one entry")
  }

  n_opt <- length(optimization_period)
  n_plot <- length(plot_period)
  post_indices <- (n_opt + 1):n_plot
  pre_indices <- 1:n_opt

  post_pre <- numeric(length(placebo_list))

  for (i in seq_along(placebo_list)) {
    entry <- placebo_list[[i]]
    y1 <- pluck(entry, "Y1")
    y0 <- pluck(entry, "Y0")
    weights <- pluck(entry, "synth.out", "solution.w")

    post_gap <- y1[post_indices, ] - y0[post_indices, ] %*% weights
    pre_gap <- y1[pre_indices, ] - y0[pre_indices, ] %*% weights

    post_mspe <- sum(post_gap^2) / length(post_indices)
    pre_mspe <- sum(pre_gap^2) / length(pre_indices)

    post_pre[i] <- post_mspe / pre_mspe
  }

  post_pre
}


#' Format predictor balance table for export.
#'
#' Reorders and formats the predictor comparison table from synth.tab() output.
#'
#' @param synth_tables List. Output from Synth::synth.tab().
#' @return Data frame with predictor names, treated/synthetic values, and V-weights.
format_predictor_table <- function(synth_tables) {
  row_order <- SYNTH$predictor_table_row_order
  tab_pred <- pluck(synth_tables, "tab.pred")
  tab_v <- pluck(synth_tables, "tab.v")

  data.frame(
    predictor = rownames(tab_pred)[row_order],
    tab_pred[row_order, ],
    v_weight = unlist(tab_v)[row_order],
    row.names = NULL
  )
}


#' Format donor weights table for export.
#'
#' Extracts the donor unit weights from synth.tab() output.
#'
#' @param synth_tables List. Output from Synth::synth.tab().
#' @return Data frame with unit names and weights.
format_weights_table <- function(synth_tables) {
  data.frame(pluck(synth_tables, "tab.w"))
}


#' Get state-level donor pool for synthetic control.
#'
#' Returns German states excluding city-states (Hamburg, Bremen, Berlin) and
#' Germany aggregate.
#'
#' @param synthdata Data frame with panel data.
#' @return Data frame with UnitNumeric and Name columns for donor states.
get_state_pool <- function(synthdata) {
  state_units <- UNITS$donor_pool_states
  synthdata |>
    filter(UnitNumeric %in% state_units, DateNumeric == 1) |>
    select(UnitNumeric, Name)
}


#' Get Hamburg hospitalization donor pool.
#'
#' Returns German states excluding Germany aggregate and MV (which may be
#' treated in some analyses).
#'
#' @param synthdata Data frame with panel data.
#' @return Data frame with UnitNumeric and Name columns for donor states.
get_hamburg_hosp_pool <- function(synthdata) {
  state_units <- UNITS$donor_pool_hamburg_hosp
  synthdata |>
    filter(UnitNumeric %in% state_units, DateNumeric == 1) |>
    select(UnitNumeric, Name)
}


#' Get Hamburg COVID incidence donor pool.
#'
#' Returns city-states plus the 15 largest cities by population, excluding
#' Hamburg itself.
#'
#' @param synthdata Data frame with panel data.
#' @return Data frame with UnitNumeric and Name columns for donor cities.
get_hamburg_covid_pool <- function(synthdata) {
  city_data <- synthdata |>
    filter(DateNumeric == 1, `County type` == "SK")

  top_n <- UNITS$hamburg_covid_top_cities
  top_n_pop <- sort(pull(city_data, Population), decreasing = TRUE)[seq_len(top_n)]
  donor_cities <- city_data |>
    filter(Population %in% top_n_pop) |>
    pull(UnitNumeric)
  donor_cities <- donor_cities[!donor_cities %in% UNITS$city_states]

  pool_units <- c(UNITS$city_states, donor_cities)

  synthdata |>
    filter(UnitNumeric %in% pool_units, DateNumeric == 1) |>
    select(UnitNumeric, Name)
}


#' Get MV cities donor pool for municipality-level analysis.
#'
#' Returns city-districts (SK) from selected states plus the aggregated MV
#' cities unit (419).
#'
#' @param synthdata Data frame with panel data.
#' @return Data frame with UnitNumeric and Name columns for donor cities.
get_mv_cities_pool <- function(synthdata) {
  n_days <- TIME$n_days

  cities_in_states <- synthdata |>
    filter(
      `County type` == "SK",
      StateId %in% UNITS$mv_cities_state_ids,
      DateNumeric == 1
    ) |>
    select(UnitNumeric, Name)

  unit_419_row <- synthdata[UNITS$mv_cities_aggregated * n_days, ] |>
    select(UnitNumeric, Name)

  rbind(cities_in_states, unit_419_row)
}


#' Build special predictors list for synthetic control.
#'
#' Creates the special.predictors argument format required by Synth::dataprep().
#'
#' @param dependent_var Character. Name of the dependent variable column.
#' @param predictor_days Integer vector of length 3. Days for lagged dependent variable.
#' @return List of predictor specifications for dataprep().
build_predictors <- function(dependent_var, predictor_days) {
  treatment_day <- TIME$treatment_day
  list(
    list("Third dose vaccinations", treatment_day, "mean"),
    list("Unemployment rate in relation to employed labor force", treatment_day, "mean"),
    list("Population Density", treatment_day, "mean"),
    list(dependent_var, predictor_days[1], "mean"),
    list(dependent_var, predictor_days[2], "mean"),
    list(dependent_var, predictor_days[3], "mean")
  )
}


#' Run complete synthetic control analysis with all robustness checks.
#'
#' Orchestrates the full analysis workflow: main estimation, placebo tests,
#' and leave-one-out robustness checks.
#'
#' @param synthdata Data frame with panel data.
#' @param pool Data frame with UnitNumeric column identifying pool units.
#' @param treated_unit Integer. UnitNumeric of the treated unit.
#' @param var_dependent Character. Name of the dependent variable column.
#' @param var_special_predictors List of special predictor specifications.
#' @param run_robustness Logical. If TRUE (default), runs leave-one-out tests.
#' @param verbose Logical. If TRUE, prints progress messages.
#' @return Named list with dataprep_out, synth_out, synth_tables, placebo_list,
#'   post_pre, robustness_donor, robustness_predictor, and input parameters.
run_complete_analysis <- function(
    synthdata,
    pool,
    treated_unit,
    var_dependent,
    var_special_predictors,
    run_robustness = TRUE,
    verbose = FALSE) {
  Sys.setlocale("LC_TIME", "C")

  periods <- get_date_periods(synthdata)
  optimization_period <- pluck(periods, "optimization_period")
  plot_period <- pluck(periods, "plot_period")

  controls <- pool |>
    filter(UnitNumeric != treated_unit) |>
    pull(UnitNumeric)

  if (verbose) message("Running main synthetic control estimation...")
  main_result <- run_synth(
    synthdata = synthdata,
    treated_unit = treated_unit,
    controls = controls,
    var_dependent = var_dependent,
    var_special_predictors = var_special_predictors,
    optimization_period = optimization_period,
    plot_period = plot_period
  )

  if (verbose) message("Running placebo tests...")
  placebo_list <- run_placebo_tests(
    synthdata = synthdata,
    pool = pool,
    treated_unit = treated_unit,
    var_dependent = var_dependent,
    var_special_predictors = var_special_predictors,
    optimization_period = optimization_period,
    plot_period = plot_period,
    original_result = main_result,
    verbose = verbose
  )

  post_pre <- calculate_post_pre_mspe(placebo_list, optimization_period, plot_period)

  robustness_donor <- NULL
  robustness_predictor <- NULL

  if (run_robustness) {
    if (verbose) message("Running leave-one-out donor robustness...")
    robustness_donor <- run_leave_one_out_donors(
      synthdata = synthdata,
      pool = pool,
      treated_unit = treated_unit,
      var_dependent = var_dependent,
      var_special_predictors = var_special_predictors,
      optimization_period = optimization_period,
      plot_period = plot_period,
      original_result = main_result,
      verbose = verbose
    )

    if (verbose) message("Running leave-one-out predictor robustness...")
    robustness_predictor <- run_leave_one_out_predictors(
      synthdata = synthdata,
      pool = pool,
      treated_unit = treated_unit,
      var_dependent = var_dependent,
      var_special_predictors = var_special_predictors,
      optimization_period = optimization_period,
      plot_period = plot_period,
      original_result = main_result,
      verbose = verbose
    )
  }

  list(
    dataprep_out = pluck(main_result, "dataprep_out"),
    synth_out = pluck(main_result, "synth_out"),
    synth_tables = pluck(main_result, "synth_tables"),
    placebo_list = placebo_list,
    post_pre = post_pre,
    robustness_donor = robustness_donor,
    robustness_predictor = robustness_predictor,
    pool = pool,
    treated_unit = treated_unit,
    var_dependent = var_dependent,
    var_special_predictors = var_special_predictors
  )
}
