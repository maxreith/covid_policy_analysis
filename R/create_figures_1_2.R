# create_figures_1_2.R
# Refactored script for creating Figures 1 and 2: COVID and Hospitalization Incidence

if (!exists("find_project_root")) {
  source_file <- sys.frame(1)$ofile
  if (!is.null(source_file)) {
    source(file.path(dirname(source_file), "synth_helpers.R"))
    source(file.path(dirname(source_file), "config.R"))
  }
}

main <- function(synthdata = NULL, output_dir = NULL) {
  Sys.setlocale("LC_TIME", "C")

  if (is.null(synthdata)) {
    synthdata <- load_synthdata()
  }
  if (is.null(output_dir)) {
    output_dir <- find_project_root()
  }

  date_range <- get_date_sequence()

  figure1_data <- extract_figure_data(
    synthdata = synthdata,
    variable = FIGURES$covid_var,
    date_range = date_range,
    hamburg_unit = FIGURES$hamburg_unit,
    mv_unit = FIGURES$mv_unit,
    donor_states = FIGURES$donor_states
  )
  figure1_data$ylim_upper <- quantile(
    synthdata[[FIGURES$covid_var]], 0.98, na.rm = TRUE
  )

  figure2_data <- extract_figure_data(
    synthdata = synthdata,
    variable = FIGURES$hosp_var,
    date_range = date_range,
    hamburg_unit = FIGURES$hamburg_unit,
    mv_unit = FIGURES$mv_unit,
    donor_states = FIGURES$donor_states
  )

  fig1_path <- file.path(output_dir, FIGURES$output_fig1)
  create_covid_figure(figure1_data, fig1_path)

  fig2_path <- file.path(output_dir, FIGURES$output_fig2)
  create_hosp_figure(figure2_data, fig2_path)

  list(
    figure1 = figure1_data,
    figure2 = figure2_data,
    date_range = date_range,
    n_days_in_range = length(date_range),
    treatment_date = TIME$treatment_date,
    donor_states = FIGURES$donor_states
  )
}

get_date_sequence <- function() {
  seq(from = FIGURES$date_start, to = FIGURES$date_end, by = "day")
}

extract_figure_data <- function(synthdata, variable, date_range, hamburg_unit,
                                 mv_unit, donor_states) {
  date_mask <- synthdata$Date %in% date_range

  dates <- synthdata$Date[synthdata$UnitNumeric == 1 & date_mask]
  hamburg <- synthdata[[variable]][synthdata$UnitNumeric == hamburg_unit & date_mask]
  mv <- synthdata[[variable]][synthdata$UnitNumeric == mv_unit & date_mask]

  donors <- lapply(donor_states, function(state_id) {
    synthdata[[variable]][synthdata$UnitNumeric == state_id & date_mask]
  })
  names(donors) <- as.character(donor_states)

  list(
    dates = dates,
    hamburg = hamburg,
    mv = mv,
    donors = donors
  )
}

create_covid_figure <- function(data, output_path) {
  dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)

  png(
    filename = output_path,
    units = "px",
    width = FIGURE$width,
    height = FIGURE$height,
    pointsize = FIGURE$pointsize,
    res = FIGURE$res
  )

  plot(
    data$dates,
    data$hamburg,
    type = "l",
    lwd = FIGURES$line_width$main,
    col = FIGURES$colors$hamburg,
    ylim = c(0, data$ylim_upper),
    main = "Incidence of COVID-19 Cases in German States",
    ylab = "Seven Days Incidence",
    xlab = ""
  )

  lines(
    data$dates,
    data$mv,
    type = "l",
    lwd = FIGURES$line_width$main,
    col = FIGURES$colors$mv
  )

  for (state_data in data$donors) {
    lines(
      data$dates,
      state_data,
      type = "l",
      col = FIGURES$colors$donors,
      lty = 2,
      lwd = FIGURES$line_width$donors
    )
  }

  legend(
    x = "topright",
    legend = c("Hamburg", "Mecklenburg-Vorpommern", "Other States"),
    fill = c(FIGURES$colors$hamburg, FIGURES$colors$mv, FIGURES$colors$donors),
    cex = 1
  )

  abline(v = TIME$treatment_date, lty = 1, col = FIGURES$colors$donors)
  dev.off()

  invisible(output_path)
}

create_hosp_figure <- function(data, output_path) {
  dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)

  png(
    filename = output_path,
    units = "px",
    width = FIGURE$width,
    height = FIGURE$height,
    pointsize = FIGURE$pointsize,
    res = FIGURE$res
  )

  # Note: Original code has "#Hamburg" comment but plots MV first (color 3)
  # This is preserved for compatibility
  plot(
    data$dates,
    data$mv,
    type = "l",
    lwd = 3,
    col = FIGURES$colors$mv,
    main = "Incidence of Hospitalized COVID-19 Cases in German States",
    ylab = "Seven Days Hospitalization Incidence",
    xlab = ""
  )

  lines(
    data$dates,
    data$hamburg,
    type = "l",
    lwd = 3,
    col = FIGURES$colors$hamburg
  )

  for (state_data in data$donors) {
    lines(
      data$dates,
      state_data,
      type = "l",
      col = FIGURES$colors$donors,
      lty = 2
    )
  }

  legend(
    x = "topright",
    legend = c("Hamburg", "Mecklenburg-Vorpommern", "Other States"),
    fill = c(FIGURES$colors$hamburg, FIGURES$colors$mv, FIGURES$colors$donors),
    cex = 1
  )

  abline(v = TIME$treatment_date, lty = 1, col = FIGURES$colors$donors)
  dev.off()

  invisible(output_path)
}

if (sys.nframe() == 0) {
  main()
}
