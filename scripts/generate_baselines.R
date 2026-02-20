# generate_baselines.R
# Generates baseline RDS files by running original analysis scripts
# and capturing key objects for testing refactored code.
#
# Usage: pixi run Rscript scripts/generate_baselines.R
#
# This script takes approximately 4 hours to run all analyses.
# Progress is printed to console.

library(Synth)
library(readxl)
library(writexl)

get_project_root <- function() {
  dir <- getwd()
  while (!dir.exists(file.path(dir, "R"))) {
    parent <- dirname(dir)
    if (parent == dir) stop("Could not find project root")
    dir <- parent
  }
  dir
}

project_root <- get_project_root()
baselines_dir <- file.path(project_root, "tests", "baselines")

if (!dir.exists(baselines_dir)) {
  dir.create(baselines_dir, recursive = TRUE)
}

load_synthdata <- function() {
  path <- file.path(
    project_root,
    "original_project/Data/processed data/synthdata.xlsx"
  )
  if (!file.exists(path)) {
    stop("synthdata.xlsx not found at ", path)
  }

  synthdata <- read_excel(
    path,
    col_types = c(
      "numeric", "text", "text", "text", "numeric", "date",
      rep("numeric", 49), "text"
    )
  )

  n_units <- nrow(synthdata) / 245
  y <- rep(seq_len(n_units), each = 245)
  synthdata2 <- as.data.frame(y)
  synthdata2[, 2:56] <- synthdata[, 2:56]
  colnames(synthdata2)[1] <- "UnitNumeric"
  synthdata2$Date <- as.Date(synthdata2$Date)

  synthdata2
}

calculate_post_pre_mspe <- function(placebo_list, optimization_period_len, plot_period_len) {
  post_pre <- numeric(length(placebo_list))
  for (i in seq_along(placebo_list)) {
    y1 <- placebo_list[[i]]$Y1
    y0 <- placebo_list[[i]]$Y0
    w <- placebo_list[[i]]$synth.out$solution.w

    post_start <- optimization_period_len + 1
    post_end <- plot_period_len
    pre_indices <- seq_len(optimization_period_len)
    post_indices <- post_start:post_end

    post_mspe <- sum((y1[post_indices, ] - y0[post_indices, ] %*% w)^2) /
      length(post_indices)
    pre_mspe <- sum((y1[pre_indices, ] - y0[pre_indices, ] %*% w)^2) /
      length(pre_indices)

    post_pre[i] <- post_mspe / pre_mspe
  }
  post_pre
}

create_placebo_entry <- function(dataprep_out, synth_out, synth_tables) {
  list(
    Y1 = dataprep_out$Y1plot,
    Y0 = dataprep_out$Y0plot,
    synth.out = synth_out,
    synth.tables = synth_tables
  )
}

run_synth_wrapper <- function(dataprep_out) {
  synth_out <- synth(data.prep.obj = dataprep_out, method = "BFGS")
  synth_tables <- synth.tab(dataprep.res = dataprep_out, synth.res = synth_out)
  list(synth_out = synth_out, synth_tables = synth_tables)
}

save_baseline <- function(name, data) {
  path <- file.path(baselines_dir, paste0(name, "_baseline.rds"))
  saveRDS(data, path)
  message("Saved baseline: ", path)
}

cat("Loading synthdata...\n")
synthdata <- load_synthdata()
Sys.setlocale("LC_TIME", "C")

optimization_period <- synthdata$DateNumeric[
  synthdata$UnitNumeric == 1 &
    synthdata$Date %in% seq(as.Date("2022-02-21"), as.Date("2022-04-03"), by = "day")
]
plot_period <- synthdata$DateNumeric[
  synthdata$UnitNumeric == 1 &
    synthdata$Date %in% seq(as.Date("2022-02-21"), as.Date("2022-05-15"), by = "day")
]

time_predictors_prior <- synthdata$DateNumeric[
  which(synthdata$AdmUnitId == "0" &
    synthdata$Date %in% seq(as.Date("2022-03-18"), by = "day", length.out = 14))
]

mspe_restrict <- 5

generate_section_411 <- function() {
  cat("\n=== Section 4.1.1: MV COVID Incidence ===\n")

  var_dependent <- "14 days covid incidence growth rate"
  var_special_predictors <- list(
    list("Third dose vaccinations", 93, "mean"),
    list("Unemployment rate in relation to employed labor force", 93, "mean"),
    list("Population Density", 93, "mean"),
    list("14 days covid incidence growth rate", 65, "mean"),
    list("14 days covid incidence growth rate", 79, "mean"),
    list("14 days covid incidence growth rate", 93, "mean")
  )

  pool <- synthdata[
    synthdata$UnitNumeric %in% c(2, 4, 6:11, 13:17) & synthdata$DateNumeric == 1,
    c("UnitNumeric", "Name")
  ]
  treated_unit <- pool$UnitNumeric[pool$Name == "Mecklenburg-Vorpommern"]

  cat("Running main synth...\n")
  dataprep_out_orig <- dataprep(
    foo = synthdata,
    special.predictors = var_special_predictors,
    time.predictors.prior = time_predictors_prior,
    dependent = var_dependent,
    unit.variable = "UnitNumeric",
    unit.names.variable = "Name",
    time.variable = "DateNumeric",
    treatment.identifier = treated_unit,
    controls.identifier = pool$UnitNumeric[pool$UnitNumeric != treated_unit],
    time.optimize.ssr = optimization_period,
    time.plot = plot_period
  )

  result <- run_synth_wrapper(dataprep_out_orig)
  synth_out_orig <- result$synth_out
  synth_tables_orig <- result$synth_tables

  placebo_list <- list()
  placebo_list[[1]] <- create_placebo_entry(
    dataprep_out_orig, synth_out_orig, synth_tables_orig
  )
  names(placebo_list)[1] <- "original treatment unit"

  cat("Running placebo tests...\n")
  for (unit in pool$UnitNumeric) {
    if (unit == treated_unit) next
    cat("  Placebo unit:", unit, "\n")

    dataprep_out <- dataprep(
      foo = synthdata,
      special.predictors = var_special_predictors,
      time.predictors.prior = time_predictors_prior,
      dependent = var_dependent,
      unit.variable = "UnitNumeric",
      unit.names.variable = "Name",
      time.variable = "DateNumeric",
      treatment.identifier = unit,
      controls.identifier = pool$UnitNumeric[pool$UnitNumeric != unit],
      time.optimize.ssr = optimization_period,
      time.plot = plot_period
    )

    result <- run_synth_wrapper(dataprep_out)
    placebo_list[[length(placebo_list) + 1]] <- create_placebo_entry(
      dataprep_out, result$synth_out, result$synth_tables
    )
    names(placebo_list)[length(placebo_list)] <- as.character(unit)
  }

  post_pre <- calculate_post_pre_mspe(
    placebo_list, length(optimization_period), length(plot_period)
  )

  robustness_donor <- list()
  robustness_donor[[1]] <- create_placebo_entry(
    dataprep_out_orig, synth_out_orig, synth_tables_orig
  )
  names(robustness_donor)[1] <- "original treatment unit"

  cat("Running leave-one-out donor robustness...\n")
  for (unit in pool$UnitNumeric) {
    if (unit == treated_unit) next
    weight_idx <- which(pool$UnitNumeric[pool$UnitNumeric != treated_unit] == unit)
    if (round(synth_out_orig$solution.w[weight_idx], digits = 4) == 0) next
    cat("  Dropping donor:", unit, "\n")

    dataprep_out <- dataprep(
      foo = synthdata,
      special.predictors = var_special_predictors,
      time.predictors.prior = time_predictors_prior,
      dependent = var_dependent,
      unit.variable = "UnitNumeric",
      unit.names.variable = "Name",
      time.variable = "DateNumeric",
      treatment.identifier = treated_unit,
      controls.identifier = pool$UnitNumeric[!pool$UnitNumeric %in% c(treated_unit, unit)],
      time.optimize.ssr = optimization_period,
      time.plot = plot_period
    )

    result <- run_synth_wrapper(dataprep_out)
    robustness_donor[[length(robustness_donor) + 1]] <- create_placebo_entry(
      dataprep_out, result$synth_out, result$synth_tables
    )
    names(robustness_donor)[length(robustness_donor)] <- paste(unit, "dropped")
  }

  robustness_predictor <- list()
  robustness_predictor[[1]] <- create_placebo_entry(
    dataprep_out_orig, synth_out_orig, synth_tables_orig
  )
  names(robustness_predictor)[1] <- "original treatment unit"

  cat("Running leave-one-out predictor robustness...\n")
  for (pred_num in seq_along(var_special_predictors)) {
    pred_weight <- as.numeric(synth_tables_orig$tab.v[pred_num, 1])
    if (round(pred_weight, digits = 4) == 0) next
    cat("  Dropping predictor:", pred_num, "\n")

    dataprep_out <- dataprep(
      foo = synthdata,
      special.predictors = var_special_predictors[-pred_num],
      time.predictors.prior = time_predictors_prior,
      dependent = var_dependent,
      unit.variable = "UnitNumeric",
      unit.names.variable = "Name",
      time.variable = "DateNumeric",
      treatment.identifier = treated_unit,
      controls.identifier = pool$UnitNumeric[pool$UnitNumeric != treated_unit],
      time.optimize.ssr = optimization_period,
      time.plot = plot_period
    )

    result <- run_synth_wrapper(dataprep_out)
    robustness_predictor[[length(robustness_predictor) + 1]] <- create_placebo_entry(
      dataprep_out, result$synth_out, result$synth_tables
    )
    names(robustness_predictor)[length(robustness_predictor)] <- paste(pred_num, "dropped")
  }

  table1 <- data.frame(
    predictor = rownames(synth_tables_orig$tab.pred)[c(4:6, 2, 3, 1)],
    synth_tables_orig$tab.pred[c(4:6, 2, 3, 1), ],
    v_weight = unlist(synth_tables_orig$tab.v)[c(4:6, 2, 3, 1)]
  )
  table2 <- data.frame(synth_tables_orig$tab.w)

  baseline <- list(
    dataprep_out = dataprep_out_orig,
    synth_out = synth_out_orig,
    synth_tables = synth_tables_orig,
    placebo_list = placebo_list,
    post_pre = post_pre,
    robustness_donor = robustness_donor,
    robustness_predictor = robustness_predictor,
    table1 = table1,
    table2 = table2,
    pool = pool,
    treated_unit = treated_unit,
    var_dependent = var_dependent,
    var_special_predictors = var_special_predictors
  )

  save_baseline("section_411", baseline)
  baseline
}

generate_section_412 <- function() {
  cat("\n=== Section 4.1.2: Hamburg COVID Incidence ===\n")

  var_dependent <- "14 days covid incidence growth rate"
  var_special_predictors <- list(
    list("Third dose vaccinations", 93, "mean"),
    list("Unemployment rate in relation to employed labor force", 93, "mean"),
    list("Population Density", 93, "mean"),
    list("14 days covid incidence growth rate", 65, "mean"),
    list("14 days covid incidence growth rate", 79, "mean"),
    list("14 days covid incidence growth rate", 93, "mean")
  )

  fifteenmax <- sort(
    synthdata$Population[synthdata$DateNumeric == 1 & synthdata$`County type` == "SK"],
    decreasing = TRUE
  )[1:15]
  donorcities <- synthdata$UnitNumeric[synthdata$Population %in% fifteenmax][
    seq(1, 15 * 245, by = 245)
  ][-c(1:2)]

  pool <- synthdata[
    synthdata$UnitNumeric %in% c(3, 12, donorcities) & synthdata$DateNumeric == 1,
    c("UnitNumeric", "Name")
  ]
  treated_unit <- pool$UnitNumeric[pool$Name == "Hamburg"]

  cat("Running main synth...\n")
  dataprep_out_orig <- dataprep(
    foo = synthdata,
    special.predictors = var_special_predictors,
    time.predictors.prior = time_predictors_prior,
    dependent = var_dependent,
    unit.variable = "UnitNumeric",
    unit.names.variable = "Name",
    time.variable = "DateNumeric",
    treatment.identifier = treated_unit,
    controls.identifier = pool$UnitNumeric[pool$UnitNumeric != treated_unit],
    time.optimize.ssr = optimization_period,
    time.plot = plot_period
  )

  result <- run_synth_wrapper(dataprep_out_orig)
  synth_out_orig <- result$synth_out
  synth_tables_orig <- result$synth_tables

  placebo_list <- list()
  placebo_list[[1]] <- create_placebo_entry(
    dataprep_out_orig, synth_out_orig, synth_tables_orig
  )
  names(placebo_list)[1] <- "original treatment unit"

  cat("Running placebo tests...\n")
  for (unit in pool$UnitNumeric) {
    if (unit == treated_unit) next
    cat("  Placebo unit:", unit, "\n")

    dataprep_out <- dataprep(
      foo = synthdata,
      special.predictors = var_special_predictors,
      time.predictors.prior = time_predictors_prior,
      dependent = var_dependent,
      unit.variable = "UnitNumeric",
      unit.names.variable = "Name",
      time.variable = "DateNumeric",
      treatment.identifier = unit,
      controls.identifier = pool$UnitNumeric[pool$UnitNumeric != unit],
      time.optimize.ssr = optimization_period,
      time.plot = plot_period
    )

    result <- run_synth_wrapper(dataprep_out)
    placebo_list[[length(placebo_list) + 1]] <- create_placebo_entry(
      dataprep_out, result$synth_out, result$synth_tables
    )
    names(placebo_list)[length(placebo_list)] <- as.character(unit)
  }

  post_pre <- calculate_post_pre_mspe(
    placebo_list, length(optimization_period), length(plot_period)
  )

  robustness_donor <- list()
  robustness_donor[[1]] <- create_placebo_entry(
    dataprep_out_orig, synth_out_orig, synth_tables_orig
  )
  names(robustness_donor)[1] <- "original treatment unit"

  cat("Running leave-one-out donor robustness...\n")
  for (unit in pool$UnitNumeric) {
    if (unit == treated_unit) next
    weight_idx <- which(pool$UnitNumeric[pool$UnitNumeric != treated_unit] == unit)
    if (round(synth_out_orig$solution.w[weight_idx], digits = 4) == 0) next
    cat("  Dropping donor:", unit, "\n")

    dataprep_out <- dataprep(
      foo = synthdata,
      special.predictors = var_special_predictors,
      time.predictors.prior = time_predictors_prior,
      dependent = var_dependent,
      unit.variable = "UnitNumeric",
      unit.names.variable = "Name",
      time.variable = "DateNumeric",
      treatment.identifier = treated_unit,
      controls.identifier = pool$UnitNumeric[!pool$UnitNumeric %in% c(treated_unit, unit)],
      time.optimize.ssr = optimization_period,
      time.plot = plot_period
    )

    result <- run_synth_wrapper(dataprep_out)
    robustness_donor[[length(robustness_donor) + 1]] <- create_placebo_entry(
      dataprep_out, result$synth_out, result$synth_tables
    )
    names(robustness_donor)[length(robustness_donor)] <- paste(unit, "dropped")
  }

  robustness_predictor <- list()
  robustness_predictor[[1]] <- create_placebo_entry(
    dataprep_out_orig, synth_out_orig, synth_tables_orig
  )
  names(robustness_predictor)[1] <- "original treatment unit"

  cat("Running leave-one-out predictor robustness...\n")
  for (pred_num in seq_along(var_special_predictors)) {
    pred_weight <- as.numeric(synth_tables_orig$tab.v[pred_num, 1])
    if (round(pred_weight, digits = 4) == 0) next
    cat("  Dropping predictor:", pred_num, "\n")

    dataprep_out <- dataprep(
      foo = synthdata,
      special.predictors = var_special_predictors[-pred_num],
      time.predictors.prior = time_predictors_prior,
      dependent = var_dependent,
      unit.variable = "UnitNumeric",
      unit.names.variable = "Name",
      time.variable = "DateNumeric",
      treatment.identifier = treated_unit,
      controls.identifier = pool$UnitNumeric[pool$UnitNumeric != treated_unit],
      time.optimize.ssr = optimization_period,
      time.plot = plot_period
    )

    result <- run_synth_wrapper(dataprep_out)
    robustness_predictor[[length(robustness_predictor) + 1]] <- create_placebo_entry(
      dataprep_out, result$synth_out, result$synth_tables
    )
    names(robustness_predictor)[length(robustness_predictor)] <- paste(pred_num, "dropped")
  }

  table3 <- data.frame(
    predictor = rownames(synth_tables_orig$tab.pred)[c(4:6, 2, 3, 1)],
    synth_tables_orig$tab.pred[c(4:6, 2, 3, 1), ],
    v_weight = unlist(synth_tables_orig$tab.v)[c(4:6, 2, 3, 1)]
  )
  table4 <- data.frame(synth_tables_orig$tab.w)

  baseline <- list(
    dataprep_out = dataprep_out_orig,
    synth_out = synth_out_orig,
    synth_tables = synth_tables_orig,
    placebo_list = placebo_list,
    post_pre = post_pre,
    robustness_donor = robustness_donor,
    robustness_predictor = robustness_predictor,
    table3 = table3,
    table4 = table4,
    pool = pool,
    treated_unit = treated_unit,
    var_dependent = var_dependent,
    var_special_predictors = var_special_predictors
  )

  save_baseline("section_412", baseline)
  baseline
}

generate_section_421 <- function() {
  cat("\n=== Section 4.2.1: MV Hospitalization ===\n")

  var_dependent <- "14 days hospitalization incidence growth rate"
  var_special_predictors <- list(
    list("Third dose vaccinations", 93, "mean"),
    list("Unemployment rate in relation to employed labor force", 93, "mean"),
    list("Population Density", 93, "mean"),
    list("14 days hospitalization incidence growth rate", 65, "mean"),
    list("14 days hospitalization incidence growth rate", 72, "mean"),
    list("14 days hospitalization incidence growth rate", 93, "mean")
  )

  pool <- synthdata[
    synthdata$UnitNumeric %in% c(2, 4, 6:11, 13:17) & synthdata$DateNumeric == 1,
    c("UnitNumeric", "Name")
  ]
  treated_unit <- pool$UnitNumeric[pool$Name == "Mecklenburg-Vorpommern"]

  cat("Running main synth...\n")
  dataprep_out_orig <- dataprep(
    foo = synthdata,
    special.predictors = var_special_predictors,
    time.predictors.prior = time_predictors_prior,
    dependent = var_dependent,
    unit.variable = "UnitNumeric",
    unit.names.variable = "Name",
    time.variable = "DateNumeric",
    treatment.identifier = treated_unit,
    controls.identifier = pool$UnitNumeric[pool$UnitNumeric != treated_unit],
    time.optimize.ssr = optimization_period,
    time.plot = plot_period
  )

  result <- run_synth_wrapper(dataprep_out_orig)
  synth_out_orig <- result$synth_out
  synth_tables_orig <- result$synth_tables

  placebo_list <- list()
  placebo_list[[1]] <- create_placebo_entry(
    dataprep_out_orig, synth_out_orig, synth_tables_orig
  )
  names(placebo_list)[1] <- "original treatment unit"

  cat("Running placebo tests...\n")
  for (unit in pool$UnitNumeric) {
    if (unit == treated_unit) next
    cat("  Placebo unit:", unit, "\n")

    dataprep_out <- dataprep(
      foo = synthdata,
      special.predictors = var_special_predictors,
      time.predictors.prior = time_predictors_prior,
      dependent = var_dependent,
      unit.variable = "UnitNumeric",
      unit.names.variable = "Name",
      time.variable = "DateNumeric",
      treatment.identifier = unit,
      controls.identifier = pool$UnitNumeric[pool$UnitNumeric != unit],
      time.optimize.ssr = optimization_period,
      time.plot = plot_period
    )

    result <- run_synth_wrapper(dataprep_out)
    placebo_list[[length(placebo_list) + 1]] <- create_placebo_entry(
      dataprep_out, result$synth_out, result$synth_tables
    )
    names(placebo_list)[length(placebo_list)] <- as.character(unit)
  }

  post_pre <- calculate_post_pre_mspe(
    placebo_list, length(optimization_period), length(plot_period)
  )

  robustness_donor <- list()
  robustness_donor[[1]] <- create_placebo_entry(
    dataprep_out_orig, synth_out_orig, synth_tables_orig
  )
  names(robustness_donor)[1] <- "original treatment unit"

  cat("Running leave-one-out donor robustness...\n")
  for (unit in pool$UnitNumeric) {
    if (unit == treated_unit) next
    weight_idx <- which(pool$UnitNumeric[pool$UnitNumeric != treated_unit] == unit)
    if (round(synth_out_orig$solution.w[weight_idx], digits = 4) == 0) next
    cat("  Dropping donor:", unit, "\n")

    dataprep_out <- dataprep(
      foo = synthdata,
      special.predictors = var_special_predictors,
      time.predictors.prior = time_predictors_prior,
      dependent = var_dependent,
      unit.variable = "UnitNumeric",
      unit.names.variable = "Name",
      time.variable = "DateNumeric",
      treatment.identifier = treated_unit,
      controls.identifier = pool$UnitNumeric[!pool$UnitNumeric %in% c(treated_unit, unit)],
      time.optimize.ssr = optimization_period,
      time.plot = plot_period
    )

    result <- run_synth_wrapper(dataprep_out)
    robustness_donor[[length(robustness_donor) + 1]] <- create_placebo_entry(
      dataprep_out, result$synth_out, result$synth_tables
    )
    names(robustness_donor)[length(robustness_donor)] <- paste(unit, "dropped")
  }

  robustness_predictor <- list()
  robustness_predictor[[1]] <- create_placebo_entry(
    dataprep_out_orig, synth_out_orig, synth_tables_orig
  )
  names(robustness_predictor)[1] <- "original treatment unit"

  cat("Running leave-one-out predictor robustness...\n")
  for (pred_num in seq_along(var_special_predictors)) {
    pred_weight <- as.numeric(synth_tables_orig$tab.v[pred_num, 1])
    if (round(pred_weight, digits = 4) == 0) next
    cat("  Dropping predictor:", pred_num, "\n")

    dataprep_out <- dataprep(
      foo = synthdata,
      special.predictors = var_special_predictors[-pred_num],
      time.predictors.prior = time_predictors_prior,
      dependent = var_dependent,
      unit.variable = "UnitNumeric",
      unit.names.variable = "Name",
      time.variable = "DateNumeric",
      treatment.identifier = treated_unit,
      controls.identifier = pool$UnitNumeric[pool$UnitNumeric != treated_unit],
      time.optimize.ssr = optimization_period,
      time.plot = plot_period
    )

    result <- run_synth_wrapper(dataprep_out)
    robustness_predictor[[length(robustness_predictor) + 1]] <- create_placebo_entry(
      dataprep_out, result$synth_out, result$synth_tables
    )
    names(robustness_predictor)[length(robustness_predictor)] <- paste(pred_num, "dropped")
  }

  table5 <- data.frame(
    predictor = rownames(synth_tables_orig$tab.pred)[c(4:6, 2, 3, 1)],
    synth_tables_orig$tab.pred[c(4:6, 2, 3, 1), ],
    v_weight = unlist(synth_tables_orig$tab.v)[c(4:6, 2, 3, 1)]
  )
  table6 <- data.frame(synth_tables_orig$tab.w)

  baseline <- list(
    dataprep_out = dataprep_out_orig,
    synth_out = synth_out_orig,
    synth_tables = synth_tables_orig,
    placebo_list = placebo_list,
    post_pre = post_pre,
    robustness_donor = robustness_donor,
    robustness_predictor = robustness_predictor,
    table5 = table5,
    table6 = table6,
    pool = pool,
    treated_unit = treated_unit,
    var_dependent = var_dependent,
    var_special_predictors = var_special_predictors
  )

  save_baseline("section_421", baseline)
  baseline
}

generate_section_422 <- function() {
  cat("\n=== Section 4.2.2: Hamburg Hospitalization ===\n")

  var_dependent <- "14 days hospitalization incidence"
  var_special_predictors <- list(
    list("Third dose vaccinations", 93, "mean"),
    list("Unemployment rate in relation to employed labor force", 93, "mean"),
    list("Population Density", 93, "mean"),
    list("14 days hospitalization incidence", 58, "mean"),
    list("14 days hospitalization incidence", 79, "mean"),
    list("14 days hospitalization incidence", 93, "mean")
  )

  pool <- synthdata[
    synthdata$UnitNumeric %in% c(2:13, 15:17) & synthdata$DateNumeric == 1,
    c("UnitNumeric", "Name")
  ]
  treated_unit <- pool$UnitNumeric[pool$Name == "Hamburg"]

  cat("Running main synth...\n")
  dataprep_out_orig <- dataprep(
    foo = synthdata,
    special.predictors = var_special_predictors,
    time.predictors.prior = time_predictors_prior,
    dependent = var_dependent,
    unit.variable = "UnitNumeric",
    unit.names.variable = "Name",
    time.variable = "DateNumeric",
    treatment.identifier = treated_unit,
    controls.identifier = pool$UnitNumeric[pool$UnitNumeric != treated_unit],
    time.optimize.ssr = optimization_period,
    time.plot = plot_period
  )

  result <- run_synth_wrapper(dataprep_out_orig)
  synth_out_orig <- result$synth_out
  synth_tables_orig <- result$synth_tables

  placebo_list <- list()
  placebo_list[[1]] <- create_placebo_entry(
    dataprep_out_orig, synth_out_orig, synth_tables_orig
  )
  names(placebo_list)[1] <- "original treatment unit"

  cat("Running placebo tests...\n")
  for (unit in pool$UnitNumeric) {
    if (unit == treated_unit) next
    cat("  Placebo unit:", unit, "\n")

    dataprep_out <- dataprep(
      foo = synthdata,
      special.predictors = var_special_predictors,
      time.predictors.prior = time_predictors_prior,
      dependent = var_dependent,
      unit.variable = "UnitNumeric",
      unit.names.variable = "Name",
      time.variable = "DateNumeric",
      treatment.identifier = unit,
      controls.identifier = pool$UnitNumeric[pool$UnitNumeric != unit],
      time.optimize.ssr = optimization_period,
      time.plot = plot_period
    )

    result <- run_synth_wrapper(dataprep_out)
    placebo_list[[length(placebo_list) + 1]] <- create_placebo_entry(
      dataprep_out, result$synth_out, result$synth_tables
    )
    names(placebo_list)[length(placebo_list)] <- as.character(unit)
  }

  post_pre <- calculate_post_pre_mspe(
    placebo_list, length(optimization_period), length(plot_period)
  )

  robustness_donor <- list()
  robustness_donor[[1]] <- create_placebo_entry(
    dataprep_out_orig, synth_out_orig, synth_tables_orig
  )
  names(robustness_donor)[1] <- "original treatment unit"

  cat("Running leave-one-out donor robustness...\n")
  for (unit in pool$UnitNumeric) {
    if (unit == treated_unit) next
    weight_idx <- which(pool$UnitNumeric[pool$UnitNumeric != treated_unit] == unit)
    if (round(synth_out_orig$solution.w[weight_idx], digits = 4) == 0) next
    cat("  Dropping donor:", unit, "\n")

    dataprep_out <- dataprep(
      foo = synthdata,
      special.predictors = var_special_predictors,
      time.predictors.prior = time_predictors_prior,
      dependent = var_dependent,
      unit.variable = "UnitNumeric",
      unit.names.variable = "Name",
      time.variable = "DateNumeric",
      treatment.identifier = treated_unit,
      controls.identifier = pool$UnitNumeric[!pool$UnitNumeric %in% c(treated_unit, unit)],
      time.optimize.ssr = optimization_period,
      time.plot = plot_period
    )

    result <- run_synth_wrapper(dataprep_out)
    robustness_donor[[length(robustness_donor) + 1]] <- create_placebo_entry(
      dataprep_out, result$synth_out, result$synth_tables
    )
    names(robustness_donor)[length(robustness_donor)] <- paste(unit, "dropped")
  }

  robustness_predictor <- list()
  robustness_predictor[[1]] <- create_placebo_entry(
    dataprep_out_orig, synth_out_orig, synth_tables_orig
  )
  names(robustness_predictor)[1] <- "original treatment unit"

  cat("Running leave-one-out predictor robustness...\n")
  for (pred_num in seq_along(var_special_predictors)) {
    pred_weight <- as.numeric(synth_tables_orig$tab.v[pred_num, 1])
    if (round(pred_weight, digits = 4) == 0) next
    cat("  Dropping predictor:", pred_num, "\n")

    dataprep_out <- dataprep(
      foo = synthdata,
      special.predictors = var_special_predictors[-pred_num],
      time.predictors.prior = time_predictors_prior,
      dependent = var_dependent,
      unit.variable = "UnitNumeric",
      unit.names.variable = "Name",
      time.variable = "DateNumeric",
      treatment.identifier = treated_unit,
      controls.identifier = pool$UnitNumeric[pool$UnitNumeric != treated_unit],
      time.optimize.ssr = optimization_period,
      time.plot = plot_period
    )

    result <- run_synth_wrapper(dataprep_out)
    robustness_predictor[[length(robustness_predictor) + 1]] <- create_placebo_entry(
      dataprep_out, result$synth_out, result$synth_tables
    )
    names(robustness_predictor)[length(robustness_predictor)] <- paste(pred_num, "dropped")
  }

  table7 <- data.frame(
    predictor = rownames(synth_tables_orig$tab.pred)[c(4:6, 2, 3, 1)],
    synth_tables_orig$tab.pred[c(4:6, 2, 3, 1), ],
    v_weight = unlist(synth_tables_orig$tab.v)[c(4:6, 2, 3, 1)]
  )
  table8 <- data.frame(synth_tables_orig$tab.w)

  baseline <- list(
    dataprep_out = dataprep_out_orig,
    synth_out = synth_out_orig,
    synth_tables = synth_tables_orig,
    placebo_list = placebo_list,
    post_pre = post_pre,
    robustness_donor = robustness_donor,
    robustness_predictor = robustness_predictor,
    table7 = table7,
    table8 = table8,
    pool = pool,
    treated_unit = treated_unit,
    var_dependent = var_dependent,
    var_special_predictors = var_special_predictors
  )

  save_baseline("section_422", baseline)
  baseline
}

generate_section_43 <- function() {
  cat("\n=== Section 4.3: MV Cities (Municipality Level) ===\n")

  var_dependent <- "14 days covid incidence growth rate"
  var_special_predictors <- list(
    list("Third dose vaccinations", 93, "mean"),
    list("Unemployment rate in relation to employed labor force", 93, "mean"),
    list("Population Density", 93, "mean"),
    list("14 days covid incidence growth rate", 65, "mean"),
    list("14 days covid incidence growth rate", 72, "mean"),
    list("14 days covid incidence growth rate", 93, "mean")
  )

  sk_units <- synthdata$UnitNumeric[
    synthdata$`County type` == "SK" &
      synthdata$StateId %in% c("1", "3", "12", "15") &
      synthdata$DateNumeric == 1
  ]
  mv_agg_row <- 419 * 245

  pool <- synthdata[
    c(which(synthdata$UnitNumeric %in% sk_units & synthdata$DateNumeric == 1), mv_agg_row),
    c("UnitNumeric", "Name")
  ]
  treated_unit <- pool$UnitNumeric[pool$Name == "SK MV aggregated"]

  cat("Running main synth...\n")
  dataprep_out_orig <- dataprep(
    foo = synthdata,
    special.predictors = var_special_predictors,
    time.predictors.prior = time_predictors_prior,
    dependent = var_dependent,
    unit.variable = "UnitNumeric",
    unit.names.variable = "Name",
    time.variable = "DateNumeric",
    treatment.identifier = treated_unit,
    controls.identifier = pool$UnitNumeric[pool$UnitNumeric != treated_unit],
    time.optimize.ssr = optimization_period,
    time.plot = plot_period
  )

  result <- run_synth_wrapper(dataprep_out_orig)
  synth_out_orig <- result$synth_out
  synth_tables_orig <- result$synth_tables

  placebo_list <- list()
  placebo_list[[1]] <- create_placebo_entry(
    dataprep_out_orig, synth_out_orig, synth_tables_orig
  )
  names(placebo_list)[1] <- "original treatment unit"

  cat("Running placebo tests...\n")
  for (unit in pool$UnitNumeric) {
    if (unit == treated_unit) next
    cat("  Placebo unit:", unit, "\n")

    dataprep_out <- dataprep(
      foo = synthdata,
      special.predictors = var_special_predictors,
      time.predictors.prior = time_predictors_prior,
      dependent = var_dependent,
      unit.variable = "UnitNumeric",
      unit.names.variable = "Name",
      time.variable = "DateNumeric",
      treatment.identifier = unit,
      controls.identifier = pool$UnitNumeric[pool$UnitNumeric != unit],
      time.optimize.ssr = optimization_period,
      time.plot = plot_period
    )

    result <- run_synth_wrapper(dataprep_out)
    placebo_list[[length(placebo_list) + 1]] <- create_placebo_entry(
      dataprep_out, result$synth_out, result$synth_tables
    )
    names(placebo_list)[length(placebo_list)] <- as.character(unit)
  }

  post_pre <- calculate_post_pre_mspe(
    placebo_list, length(optimization_period), length(plot_period)
  )

  table9 <- data.frame(
    predictor = rownames(synth_tables_orig$tab.pred)[c(4:6, 2, 3, 1)],
    synth_tables_orig$tab.pred[c(4:6, 2, 3, 1), ],
    v_weight = unlist(synth_tables_orig$tab.v)[c(4:6, 2, 3, 1)]
  )
  table10 <- data.frame(synth_tables_orig$tab.w)

  baseline <- list(
    dataprep_out = dataprep_out_orig,
    synth_out = synth_out_orig,
    synth_tables = synth_tables_orig,
    placebo_list = placebo_list,
    post_pre = post_pre,
    table9 = table9,
    table10 = table10,
    pool = pool,
    treated_unit = treated_unit,
    var_dependent = var_dependent,
    var_special_predictors = var_special_predictors
  )

  save_baseline("section_43", baseline)
  baseline
}

cat("Starting baseline generation...\n")
cat("This will take approximately 4 hours.\n\n")

start_time <- Sys.time()

generate_section_411()
generate_section_412()
generate_section_421()
generate_section_422()
generate_section_43()

end_time <- Sys.time()
elapsed <- difftime(end_time, start_time, units = "mins")

cat("\n=== Complete ===\n")
cat("Total time:", round(elapsed, 1), "minutes\n")
cat("Baselines saved to:", baselines_dir, "\n")
