# generate_figures_baseline.R
# Generates baseline data for Figures 1 and 2 tests
# This extracts the exact data values plotted in the original figures

source("R/test_helpers.R")

generate_figures_baseline <- function() {
  synthdata <- load_original_synthdata()

  n_days <- 245L
  n_units <- nrow(synthdata) / n_days

  unit_numeric <- integer(nrow(synthdata))
  for (i in seq_len(n_units)) {
    unit_numeric[((1 + n_days * (i - 1)):(n_days * i))] <- i
  }
  synthdata$UnitNumeric <- unit_numeric
  synthdata$Date <- as.Date(synthdata$Date)

  date_range <- seq(from = as.Date("2022-02-01"), to = as.Date("2022-06-01"), by = "day")
  treatment_date <- as.Date("2022-04-03")
  donor_states <- c(2L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 15L, 16L, 17L)

  dates_vec <- synthdata$Date[synthdata$UnitNumeric == 1 & synthdata$Date %in% date_range]

  hamburg_covid <- synthdata$`covid incidence`[
    synthdata$UnitNumeric == 3 & synthdata$Date %in% date_range
  ]
  mv_covid <- synthdata$`covid incidence`[
    synthdata$UnitNumeric == 14 & synthdata$Date %in% date_range
  ]

  donors_covid <- list()
  for (state_id in donor_states) {
    donors_covid[[as.character(state_id)]] <- synthdata$`covid incidence`[
      synthdata$UnitNumeric == state_id & synthdata$Date %in% date_range
    ]
  }

  ylim_upper_fig1 <- quantile(synthdata$`covid incidence`, 0.98, na.rm = TRUE)

  hamburg_hosp <- synthdata$`Hospitalization incidence 00+`[
    synthdata$UnitNumeric == 3 & synthdata$Date %in% date_range
  ]
  mv_hosp <- synthdata$`Hospitalization incidence 00+`[
    synthdata$UnitNumeric == 14 & synthdata$Date %in% date_range
  ]

  donors_hosp <- list()
  for (state_id in donor_states) {
    donors_hosp[[as.character(state_id)]] <- synthdata$`Hospitalization incidence 00+`[
      synthdata$UnitNumeric == state_id & synthdata$Date %in% date_range
    ]
  }

  baseline <- list(
    figure1 = list(
      dates = dates_vec,
      hamburg = hamburg_covid,
      mv = mv_covid,
      donors = donors_covid,
      ylim_upper = ylim_upper_fig1
    ),
    figure2 = list(
      dates = dates_vec,
      hamburg = hamburg_hosp,
      mv = mv_hosp,
      donors = donors_hosp
    ),
    treatment_date = treatment_date,
    donor_states = donor_states,
    date_range = date_range,
    n_days_in_range = length(date_range)
  )

  baseline
}

if (interactive() || !exists("SOURCED_ONLY")) {
  baseline <- generate_figures_baseline()

  baselines_dir <- file.path(find_project_root(), "tests", "baselines")
  if (!dir.exists(baselines_dir)) {
    dir.create(baselines_dir, recursive = TRUE)
  }

  path <- file.path(baselines_dir, "figures_baseline.rds")
  saveRDS(baseline, path)
  message("Saved figures baseline to: ", path)

  message("\nBaseline summary:")
  message("  Date range: ", min(baseline$date_range), " to ", max(baseline$date_range))
  message("  Days in range: ", baseline$n_days_in_range)
  message("  Donor states: ", paste(baseline$donor_states, collapse = ", "))
  message("  Figure 1 y-limit (98th percentile): ", round(baseline$figure1$ylim_upper, 2))
  message("  Hamburg COVID incidence range: ",
          round(min(baseline$figure1$hamburg, na.rm = TRUE), 2), " - ",
          round(max(baseline$figure1$hamburg, na.rm = TRUE), 2))
  message("  MV COVID incidence range: ",
          round(min(baseline$figure1$mv, na.rm = TRUE), 2), " - ",
          round(max(baseline$figure1$mv, na.rm = TRUE), 2))
}
