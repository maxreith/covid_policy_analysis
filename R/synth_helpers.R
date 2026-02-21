# synth_helpers.R
# Shared helper functions for synthetic control analyses

library(Synth)
library(dplyr)
library(purrr)

find_project_root <- function() {
  dir <- getwd()
  while (!dir.exists(file.path(dir, "R"))) {
    parent <- dirname(dir)
    if (parent == dir) stop("Could not find project root")
    dir <- parent
  }
  dir
}

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

  n_days <- 245L
  n_units <- nrow(synthdata) / n_days

  unit_numeric <- rep(seq_len(n_units), each = n_days)
  synthdata2 <- data.frame(UnitNumeric = unit_numeric)
  synthdata2[, 2:56] <- synthdata[, 2:56]
  colnames(synthdata2)[2:56] <- colnames(synthdata)[2:56]
  synthdata2$Date <- as.Date(synthdata2$Date)

  synthdata2
}

get_date_periods <- function(
    synthdata,
    optimization_start = as.Date("2022-02-21"),
    optimization_end = as.Date("2022-04-03"),
    plot_start = as.Date("2022-02-21"),
    plot_end = as.Date("2022-05-15")) {
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

get_time_predictors_prior <- function(synthdata) {
  synthdata |>
    filter(
      AdmUnitId == "0",
      Date %in% seq(as.Date("2022-03-18"), by = "day", length.out = 14)
    ) |>
    pull(DateNumeric)
}

run_synth <- function(
    synthdata,
    treated_unit,
    controls,
    var_dependent,
    var_special_predictors,
    optimization_period,
    plot_period,
    method = "BFGS") {
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

    if (verbose) print(unit)
  }

  placebo_list
}

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

    if (verbose) print(unit)
  }

  robustness_list
}

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

    if (verbose) print(predictor_num)
  }

  robustness_list
}

calculate_post_pre_mspe <- function(placebo_list, optimization_period, plot_period) {
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

format_predictor_table <- function(synth_tables) {
  row_order <- c(4, 5, 6, 2, 3, 1)
  tab_pred <- pluck(synth_tables, "tab.pred")
  tab_v <- pluck(synth_tables, "tab.v")

  data.frame(
    predictor = rownames(tab_pred)[row_order],
    tab_pred[row_order, ],
    v_weight = unlist(tab_v)[row_order],
    row.names = NULL
  )
}

format_weights_table <- function(synth_tables) {
  data.frame(pluck(synth_tables, "tab.w"))
}

get_state_pool <- function(synthdata) {
  state_units <- c(2L, 4L, 6L:11L, 13L:17L)
  synthdata |>
    filter(UnitNumeric %in% state_units, DateNumeric == 1) |>
    select(UnitNumeric, Name)
}

get_hamburg_hosp_pool <- function(synthdata) {
  state_units <- c(2L:13L, 15L:17L)
  synthdata |>
    filter(UnitNumeric %in% state_units, DateNumeric == 1) |>
    select(UnitNumeric, Name)
}

get_hamburg_covid_pool <- function(synthdata) {
  city_data <- synthdata |>
    filter(DateNumeric == 1, `County type` == "SK")

  top_15_pop <- sort(pull(city_data, Population), decreasing = TRUE)[1:15]
  donor_cities <- city_data |>
    filter(Population %in% top_15_pop) |>
    pull(UnitNumeric)
  donor_cities <- donor_cities[!donor_cities %in% c(3L, 12L)]

  pool_units <- c(3L, 12L, donor_cities)

  synthdata |>
    filter(UnitNumeric %in% pool_units, DateNumeric == 1) |>
    select(UnitNumeric, Name)
}

get_mv_cities_pool <- function(synthdata) {
  n_days <- 245L

  cities_in_states <- synthdata |>
    filter(
      `County type` == "SK",
      StateId %in% c("1", "3", "12", "15"),
      DateNumeric == 1
    ) |>
    select(UnitNumeric, Name)

  unit_419_row <- synthdata[419 * n_days, ] |>
    select(UnitNumeric, Name)

  rbind(cities_in_states, unit_419_row)
}

build_predictors <- function(dependent_var, predictor_days) {
  list(
    list("Third dose vaccinations", 93L, "mean"),
    list("Unemployment rate in relation to employed labor force", 93L, "mean"),
    list("Population Density", 93L, "mean"),
    list(dependent_var, predictor_days[1], "mean"),
    list(dependent_var, predictor_days[2], "mean"),
    list(dependent_var, predictor_days[3], "mean")
  )
}

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
