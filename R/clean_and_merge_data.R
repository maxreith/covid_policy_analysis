# clean_and_merge_data.R
# Refactored data import pipeline for COVID-19 policy analysis
# Produces synthdata.parquet from 7 raw data sources

library(dplyr)
library(readr)
library(readxl)
library(arrow)
library(stringr)
library(tidyr)
library(slider)


find_project_root <- function() {
  dir <- getwd()
  while (!dir.exists(file.path(dir, "R"))) {
    parent <- dirname(dir)
    if (parent == dir) stop("Could not find project root")
    dir <- parent
  }
  dir
}


if (!exists("TIME") || !exists("UNITS")) {
  source(file.path(find_project_root(), "R/config.R"))
}


main <- function() {
  raw <- load_all_raw_data()

  synthdata <- build_panel_skeleton(raw$covid, raw$admunit)
  synthdata <- aggregate_berlin(synthdata, raw$admunit)
  synthdata <- pad_admunit_ids(synthdata)
  synthdata <- add_names(synthdata, raw$admunit)
  synthdata <- join_population_data(synthdata, raw$population)
  synthdata <- join_unemployment_data(synthdata, raw$unemployment)
  synthdata <- join_vaccination_states(synthdata, raw$vac_states)
  synthdata <- join_vaccination_counties(synthdata, raw$vac_counties)
  synthdata <- join_hospitalization_data(synthdata, raw$hospitalization)
  synthdata <- compute_hospitalization_growth_rate_7d(synthdata)
  synthdata <- add_county_type(synthdata)
  synthdata <- create_mv_aggregates(synthdata)
  synthdata <- compute_covid_incidence_7d(synthdata)
  synthdata <- convert_vaccinations_to_rates(synthdata)
  synthdata <- compute_covid_growth_rate_7d(synthdata)
  synthdata <- compute_covid_incidence_14d(synthdata)
  synthdata <- compute_covid_growth_rate_14d(synthdata)
  synthdata <- compute_hospitalization_incidence_14d(synthdata)
  synthdata <- compute_hospitalization_growth_rate_14d(synthdata)
  synthdata <- reorder_columns(synthdata)
  synthdata <- rename_to_snake_case(synthdata)

  validate_and_write(synthdata)
}


load_all_raw_data <- function() {
  list(
    covid = load_covid_data(),
    admunit = load_admunit_data(),
    population = load_population_data(),
    unemployment = load_unemployment_data(),
    vac_states = load_vaccination_states(),
    vac_counties = load_vaccination_counties(),
    hospitalization = load_hospitalization_data()
  )
}


load_covid_data <- function() {
  path <- file.path(find_project_root(), "original_project/Data/raw data/RKI_History.csv")
  read_csv(
    path,
    col_types = cols(
      AdmUnitId = col_character(),
      BundeslandId = col_character(),
      .default = col_guess()
    ),
    show_col_types = FALSE
  ) |>
    mutate(Datum = as.Date(Datum)) |>
    filter(format(Datum, "%Y") == "2022")
}


load_admunit_data <- function() {
  path <- file.path(find_project_root(), "original_project/Data/raw data/RKI_AdmUnit.csv")
  df <- read_csv(
    path,
    col_types = cols(AdmUnitId = col_character()),
    show_col_types = FALSE
  )
  df
}


get_admunit_with_berlin_fix <- function(admunit_data) {
  df <- admunit_data
  row_429 <- which(df$AdmUnitId == "11001")
  if (length(row_429) > 0) {
    df$AdmUnitId[row_429[1]] <- "11000"
    df$Name[row_429[1]] <- "Berlin"
  }
  df
}


load_population_data <- function() {
  path <- file.path(
    find_project_root(),
    "original_project/Data/raw data/Kreisfreie Staedte und Landkreise Flache, Bevoelkerung.xlsx"
  )

  df <- read_excel(path, sheet = EXCEL_SHEETS$population, range = POPULATION$excel_range)
  df <- df[-POPULATION$rows_to_drop, ]
  df <- df[!is.na(df[[3]]), ]
  df[[1]][nrow(df)] <- "0"

  colnames(df) <- c(
    "AdmUnitId", "Name", "StateCode", "AreaType",
    "area_sq_km", "population", "population_male",
    "population_female", "population_density"
  )
  df
}


load_unemployment_data <- function() {
  path <- file.path(
    find_project_root(),
    "original_project/Data/raw data/Arbeitslose_Kreise.xlsx"
  )
  df <- read_excel(
    path,
    sheet = EXCEL_SHEETS$unemployment,
    col_types = c("text", "text", rep("numeric", 13))
  )
  colnames(df)[1] <- "AdmUnitId"

  last_row <- which(df$AdmUnitId == "16077")[1]
  df <- df[1:last_row, ]

  cols_to_drop <- c("Arbeitslose schwerbehindert", "...2")
  df <- df[, !colnames(df) %in% cols_to_drop]

  colnames(df) <- c(
    "AdmUnitId",
    "Unemployed", "Unemployed Foreigners", "Unemployed Age 15-20",
    "Unemployed Age 15-25", "Unemployed Age 55-65", "Long-term Unemployed",
    "Unemployment rate in relation to employed labor force",
    "Unemployment rate in relation to total labor force",
    "Unemployment rate of men in relation to total male labor force",
    "Unemployment rate of women in relation to total female labor force",
    "Unemployment rate of foreigners in relation to total foreign labor force",
    "Unemployment rate of people aged 15-25 in relation to total labor force aged 15-25"
  )
  df
}


load_vaccination_states <- function() {
  path <- file.path(
    find_project_root(),
    "original_project/Data/raw data/Impfungen Laender.csv"
  )
  read_csv(
    path,
    col_types = cols(Impfdatum = col_date(format = "%Y-%m-%d")),
    show_col_types = FALSE
  )
}


load_vaccination_counties <- function() {
  path <- file.path(
    find_project_root(),
    "original_project/Data/raw data/Impfungen Landkreise.csv"
  )
  read_csv(
    path,
    col_types = cols(Impfdatum = col_character()),
    show_col_types = FALSE
  )
}


load_hospitalization_data <- function() {
  path <- file.path(
    find_project_root(),
    "original_project/Data/raw data/Hospitalisierungen.csv"
  )
  df <- read_csv(path, show_col_types = FALSE)
  df$Datum <- as.Date(df$Datum)
  df$Bundesland_Id[df$Bundesland_Id == "00"] <- "0"
  df
}


build_panel_skeleton <- function(covid_data, admunit_data) {
  region_ids <- admunit_data$AdmUnitId
  n_days <- TIME$n_days

  unit_counter <- 0
  result_list <- list()

  for (region_id in region_ids) {
    region_rows <- covid_data[covid_data$AdmUnitId == region_id, ]
    if (nrow(region_rows) == 0) next

    region_rows <- region_rows[order(region_rows$Datum), ]
    unit_counter <- unit_counter + 1

    region_rows$UnitNumeric <- unit_counter
    region_rows$DateNumeric <- seq_len(nrow(region_rows))
    result_list[[unit_counter]] <- region_rows
  }

  df <- bind_rows(result_list)

  df <- df |>
    select(
      UnitNumeric,
      AdmUnitId,
      StateId = BundeslandId,
      DateNumeric,
      Date = Datum,
      AnzFallNeu,
      AnzFallVortag,
      AnzFallErkrankung,
      AnzFallMeldung,
      KumFall
    ) |>
    mutate(Name = NA_character_)

  df <- df |>
    select(
      UnitNumeric, AdmUnitId, StateId, Name, DateNumeric, Date,
      AnzFallNeu, AnzFallVortag, AnzFallErkrankung, AnzFallMeldung, KumFall
    )

  df
}


aggregate_berlin <- function(synthdata, admunit_data) {
  berlin_district_ids <- sprintf("1100%d", UNITS$berlin_district_nums[1:9])
  berlin_district_ids <- c(berlin_district_ids, "11010", "11011", "11012")
  dates <- unique(synthdata$Date[synthdata$AdmUnitId == "0"])
  n_days <- length(dates)

  numeric_cols <- c("AnzFallNeu", "AnzFallVortag", "AnzFallErkrankung",
                    "AnzFallMeldung", "KumFall")

  aggregated_rows <- list()
  for (i in seq_along(dates)) {
    d <- dates[i]
    berlin_rows <- synthdata[
      synthdata$AdmUnitId %in% berlin_district_ids & synthdata$Date == d,
    ]

    if (nrow(berlin_rows) == 0) next

    agg_row <- data.frame(
      UnitNumeric = NA_integer_,
      AdmUnitId = "11000",
      StateId = berlin_rows$StateId[1],
      Name = NA_character_,
      DateNumeric = i,
      Date = d,
      AnzFallNeu = sum(berlin_rows$AnzFallNeu, na.rm = TRUE),
      AnzFallVortag = sum(berlin_rows$AnzFallVortag, na.rm = TRUE),
      AnzFallErkrankung = sum(berlin_rows$AnzFallErkrankung, na.rm = TRUE),
      AnzFallMeldung = sum(berlin_rows$AnzFallMeldung, na.rm = TRUE),
      KumFall = sum(berlin_rows$KumFall, na.rm = TRUE)
    )
    aggregated_rows[[i]] <- agg_row
  }
  berlin_agg <- bind_rows(aggregated_rows)

  first_berlin_unit <- synthdata$UnitNumeric[synthdata$AdmUnitId == "11001"][1]
  first_berlin_rows <- which(synthdata$AdmUnitId == "11001")
  synthdata[first_berlin_rows, numeric_cols] <- berlin_agg[, numeric_cols]
  synthdata[first_berlin_rows, "AdmUnitId"] <- "11000"
  synthdata[first_berlin_rows, "Date"] <- berlin_agg$Date
  synthdata[first_berlin_rows, "DateNumeric"] <- berlin_agg$DateNumeric

  other_berlin_ids <- berlin_district_ids[berlin_district_ids != "11001"]
  synthdata <- synthdata[!synthdata$AdmUnitId %in% other_berlin_ids, ]

  synthdata <- synthdata[order(synthdata$UnitNumeric, synthdata$DateNumeric), ]

  unit_ids <- unique(synthdata$UnitNumeric)
  unit_mapping <- setNames(seq_along(unit_ids), unit_ids)
  synthdata$UnitNumeric <- unit_mapping[as.character(synthdata$UnitNumeric)]

  synthdata
}


pad_admunit_ids <- function(synthdata) {
  state_ids_1digit <- as.character(1:9)
  needs_padding_state <- synthdata$AdmUnitId %in% state_ids_1digit
  synthdata$AdmUnitId[needs_padding_state] <- paste0("0", synthdata$AdmUnitId[needs_padding_state])

  needs_padding_county <- nchar(synthdata$AdmUnitId) == 4 &
                          !synthdata$AdmUnitId %in% c("1001", as.character(10:16))
  needs_padding_county <- needs_padding_county |
                          (nchar(synthdata$AdmUnitId) == 4 &
                           as.numeric(synthdata$AdmUnitId) >= 1001)
  needs_padding_county[is.na(needs_padding_county)] <- FALSE
  synthdata$AdmUnitId[needs_padding_county] <- paste0("0", synthdata$AdmUnitId[needs_padding_county])

  synthdata
}


add_names <- function(synthdata, admunit_data) {
  admunit_fixed <- get_admunit_with_berlin_fix(admunit_data)

  first_county_row <- which(synthdata$AdmUnitId == "01001")[1]
  is_state_row <- seq_len(nrow(synthdata)) < first_county_row

  name_lookup <- admunit_fixed |>
    select(AdmUnitId, Name) |>
    distinct()

  state_rows <- synthdata[is_state_row, ]
  state_rows <- state_rows |>
    left_join(
      name_lookup |> rename(LookedUpName = Name),
      by = c("StateId" = "AdmUnitId")
    ) |>
    mutate(Name = LookedUpName) |>
    select(-LookedUpName)

  county_rows <- synthdata[!is_state_row, ]
  county_rows <- county_rows |>
    mutate(UnpaddedId = sub("^0+", "", AdmUnitId)) |>
    mutate(UnpaddedId = if_else(UnpaddedId == "", "0", UnpaddedId)) |>
    left_join(
      name_lookup |> rename(LookedUpName = Name),
      by = c("UnpaddedId" = "AdmUnitId")
    )

  still_missing <- is.na(county_rows$LookedUpName)
  if (any(still_missing)) {
    county_rows <- county_rows |>
      left_join(
        name_lookup |> rename(LookedUpName2 = Name),
        by = c("AdmUnitId" = "AdmUnitId")
      ) |>
      mutate(LookedUpName = if_else(is.na(LookedUpName), LookedUpName2, LookedUpName)) |>
      select(-LookedUpName2)
  }

  county_rows <- county_rows |>
    mutate(Name = LookedUpName) |>
    select(-UnpaddedId, -LookedUpName)

  synthdata <- bind_rows(state_rows, county_rows)
  synthdata <- synthdata[order(synthdata$UnitNumeric, synthdata$DateNumeric), ]

  synthdata
}


join_population_data <- function(synthdata, population_data) {
  pop_cols <- c("area_sq_km", "population", "population_male",
                "population_female", "population_density")

  pop_lookup <- population_data |>
    select(AdmUnitId, all_of(pop_cols)) |>
    mutate(
      AdmUnitId_padded = if_else(
        nchar(AdmUnitId) == 4,
        paste0("0", AdmUnitId),
        AdmUnitId
      )
    )

  synthdata_first_rows <- synthdata |>
    group_by(UnitNumeric) |>
    slice(1) |>
    ungroup() |>
    select(UnitNumeric, AdmUnitId)

  unit_pop <- synthdata_first_rows |>
    left_join(
      pop_lookup |> select(AdmUnitId_padded, all_of(pop_cols)),
      by = c("AdmUnitId" = "AdmUnitId_padded")
    )

  still_missing <- is.na(unit_pop$population)
  if (any(still_missing)) {
    missing_units <- unit_pop$UnitNumeric[still_missing]
    for (unit_num in missing_units) {
      adm_id <- synthdata_first_rows$AdmUnitId[synthdata_first_rows$UnitNumeric == unit_num]
      unpadded <- sub("^0", "", adm_id)
      match_idx <- which(pop_lookup$AdmUnitId == unpadded)
      if (length(match_idx) > 0) {
        for (col in pop_cols) {
          unit_pop[[col]][unit_pop$UnitNumeric == unit_num] <- pop_lookup[[col]][match_idx[1]]
        }
      }
    }
  }

  synthdata <- synthdata |>
    left_join(
      unit_pop |> select(UnitNumeric, all_of(pop_cols)),
      by = "UnitNumeric"
    )

  synthdata <- synthdata |>
    rename(
      `Area in Square Kilometers` = area_sq_km,
      `Population` = population,
      `Male Population` = population_male,
      `Female Population` = population_female,
      `Population Density` = population_density
    )

  synthdata
}


join_unemployment_data <- function(synthdata, unemployment_data) {
  unempl_cols <- colnames(unemployment_data)[-1]

  unempl_lookup <- unemployment_data |>
    mutate(
      AdmUnitId_padded = if_else(
        nchar(AdmUnitId) == 4,
        paste0("0", AdmUnitId),
        AdmUnitId
      )
    )

  synthdata_first_rows <- synthdata |>
    group_by(UnitNumeric) |>
    slice(1) |>
    ungroup() |>
    select(UnitNumeric, AdmUnitId)

  unit_unempl <- synthdata_first_rows |>
    left_join(
      unempl_lookup |> select(AdmUnitId_padded, all_of(unempl_cols)),
      by = c("AdmUnitId" = "AdmUnitId_padded")
    )

  still_missing <- is.na(unit_unempl[[unempl_cols[1]]])
  if (any(still_missing)) {
    missing_units <- unit_unempl$UnitNumeric[still_missing]
    for (unit_num in missing_units) {
      adm_id <- synthdata_first_rows$AdmUnitId[synthdata_first_rows$UnitNumeric == unit_num]
      match_idx <- which(unempl_lookup$AdmUnitId == adm_id)
      if (length(match_idx) > 0) {
        for (col in unempl_cols) {
          unit_unempl[[col]][unit_unempl$UnitNumeric == unit_num] <- unempl_lookup[[col]][match_idx[1]]
        }
      }
    }
  }

  synthdata <- synthdata |>
    left_join(
      unit_unempl |> select(UnitNumeric, all_of(unempl_cols)),
      by = "UnitNumeric"
    )

  synthdata
}


join_vaccination_states <- function(synthdata, vac_data) {
  vac_cols <- VACCINATION$dose_columns
  for (col in vac_cols) {
    synthdata[[col]] <- NA_real_
  }

  max_dose <- max(vac_data$Impfserie)
  state_units <- UNITS$state_unit_range[-1]

  for (dose in seq_len(max_dose)) {
    col_name <- vac_cols[dose]

    for (unit_num in state_units) {
      unit_rows <- which(synthdata$UnitNumeric == unit_num)
      if (length(unit_rows) == 0) next

      adm_id <- synthdata$AdmUnitId[unit_rows[1]]

      for (row_idx in unit_rows) {
        current_date <- synthdata$Date[row_idx]

        total_vac <- sum(
          vac_data$Anzahl[
            vac_data$Impfdatum <= current_date &
            vac_data$BundeslandId_Impfort == adm_id &
            vac_data$Impfserie == dose
          ],
          na.rm = TRUE
        )

        synthdata[row_idx, col_name] <- total_vac
      }
    }
  }

  synthdata
}


join_vaccination_counties <- function(synthdata, vac_data) {
  vac_cols <- VACCINATION$dose_columns

  max_dose <- max(vac_data$Impfschutz)
  vaccination_cutoff <- TIME$vaccination_cutoff_day
  municipality_units <- UNITS$municipality_unit_range

  for (dose in seq_len(max_dose)) {
    col_name <- vac_cols[dose]
    dose_mask <- vac_data$Impfschutz == dose

    for (unit_num in municipality_units) {
      unit_rows <- which(synthdata$UnitNumeric == unit_num)
      if (length(unit_rows) == 0) next

      adm_id <- synthdata$AdmUnitId[unit_rows[1]]

      first_row <- unit_rows[1]
      initial_vac <- sum(
        vac_data$Anzahl[
          vac_data$Impfdatum <= "2022-01-01" &
          vac_data$LandkreisId_Impfort == adm_id &
          dose_mask
        ],
        na.rm = TRUE
      )
      synthdata[first_row, col_name] <- initial_vac

      for (day_num in 2:vaccination_cutoff) {
        row_idx <- unit_rows[day_num]
        current_date <- as.character(synthdata$Date[row_idx])

        daily_vac <- sum(
          vac_data$Anzahl[
            vac_data$Impfdatum == current_date &
            vac_data$LandkreisId_Impfort == adm_id &
            dose_mask
          ],
          na.rm = TRUE
        )

        prev_row <- unit_rows[day_num - 1]
        synthdata[row_idx, col_name] <- synthdata[prev_row, col_name] + daily_vac
      }
    }

    message(sprintf("Completed dose %d of %d", dose, max_dose))
  }

  synthdata
}


join_hospitalization_data <- function(synthdata, hosp_data) {
  age_groups <- HOSPITALIZATION$age_groups
  hosp_count_cols <- paste0("Hospitalizations ", age_groups)
  hosp_inc_cols <- paste0("Hospitalization incidence ", age_groups)

  for (col in c(hosp_count_cols, hosp_inc_cols)) {
    synthdata[[col]] <- NA_real_
  }

  state_units <- UNITS$state_unit_range

  for (unit_num in state_units) {
    unit_rows <- which(synthdata$UnitNumeric == unit_num)
    if (length(unit_rows) == 0) next

    adm_id <- synthdata$AdmUnitId[unit_rows[1]]

    for (row_idx in unit_rows) {
      current_date <- synthdata$Date[row_idx]

      for (j in seq_along(age_groups)) {
        age_grp <- age_groups[j]

        matching <- which(
          hosp_data$Datum == current_date &
          hosp_data$Bundesland_Id == adm_id &
          hosp_data$Altersgruppe == age_grp
        )

        if (length(matching) > 0) {
          synthdata[row_idx, hosp_count_cols[j]] <- hosp_data$`7T_Hospitalisierung_Faelle`[matching[1]]
          synthdata[row_idx, hosp_inc_cols[j]] <- hosp_data$`7T_Hospitalisierung_Inzidenz`[matching[1]]
        }
      }
    }
  }

  synthdata
}


create_mv_aggregates <- function(synthdata) {
  n_days <- TIME$n_days

  mv_lk_names <- c("LK Vorpommern-Rügen", "LK Rostock")
  lk_units <- synthdata |>
    filter(StateId == "13", DateNumeric == 1, Name %in% mv_lk_names) |>
    pull(UnitNumeric)

  synthdata <- create_aggregate_unit(
    synthdata, lk_units, 418, "LK MV aggregated",
    use_single_date = TRUE
  )

  sk_units <- synthdata |>
    filter(StateId == "13", DateNumeric == 1, grepl("^SK ", Name)) |>
    pull(UnitNumeric)

  synthdata <- create_aggregate_unit(
    synthdata, sk_units, 419, "SK MV aggregated",
    use_single_date = FALSE
  )

  synthdata
}


create_aggregate_unit <- function(synthdata, source_units, new_unit_num, new_name,
                                  use_single_date = TRUE) {
  n_days <- TIME$n_days

  sum_cols <- c(
    "AnzFallNeu", "AnzFallVortag", "AnzFallErkrankung", "AnzFallMeldung", "KumFall",
    "Area in Square Kilometers", "Population", "Male Population", "Female Population",
    "Unemployed", "Unemployed Foreigners", "Unemployed Age 15-20",
    "Unemployed Age 15-25", "Unemployed Age 55-65", "Long-term Unemployed",
    VACCINATION$dose_columns
  )

  template_rows <- synthdata[synthdata$UnitNumeric == source_units[1], ]
  new_rows <- template_rows
  new_rows$UnitNumeric <- new_unit_num
  new_rows$Name <- new_name
  new_rows$AdmUnitId <- NA_character_
  new_rows$StateId <- NA_character_

  if ("County type" %in% colnames(new_rows)) {
    new_rows[["County type"]] <- NA_character_
  }

  if (use_single_date) {
    first_source_row <- synthdata[synthdata$UnitNumeric %in% source_units, ][1, ]
    new_rows$DateNumeric <- first_source_row$DateNumeric
    new_rows$Date <- first_source_row$Date
  }

  for (day_num in 1:n_days) {
    source_day_rows <- synthdata[
      synthdata$UnitNumeric %in% source_units & synthdata$DateNumeric == day_num,
    ]

    for (col in sum_cols) {
      if (col %in% colnames(synthdata)) {
        col_values <- source_day_rows[[col]]
        if (all(is.na(col_values))) {
          new_rows[day_num, col] <- NA_real_
        } else {
          new_rows[day_num, col] <- sum(col_values, na.rm = TRUE)
        }
      }
    }
  }

  new_rows[["Population Density"]] <-
    new_rows[["Population"]] / new_rows[["Area in Square Kilometers"]]

  unemp_rate_employed_col <- "Unemployment rate in relation to employed labor force"
  unemp_rate_total_col <- "Unemployment rate in relation to total labor force"

  other_unemp_rate_cols <- c(
    "Unemployment rate of men in relation to total male labor force",
    "Unemployment rate of women in relation to total female labor force",
    "Unemployment rate of foreigners in relation to total foreign labor force",
    "Unemployment rate of people aged 15-25 in relation to total labor force aged 15-25"
  )
  for (col in other_unemp_rate_cols) {
    if (col %in% colnames(new_rows)) {
      new_rows[[col]] <- NA_real_
    }
  }

  if (unemp_rate_employed_col %in% colnames(synthdata)) {
    day1_source <- synthdata[
      synthdata$UnitNumeric %in% source_units & synthdata$DateNumeric == 1,
    ]

    labor_force_employed <- sum(
      day1_source[["Unemployed"]] / day1_source[[unemp_rate_employed_col]],
      na.rm = TRUE
    )
    new_rows[[unemp_rate_employed_col]] <- new_rows[["Unemployed"]] / labor_force_employed

    labor_force_total <- sum(
      day1_source[["Unemployed"]] / day1_source[[unemp_rate_total_col]],
      na.rm = TRUE
    )
    new_rows[[unemp_rate_total_col]] <- new_rows[["Unemployed"]] / labor_force_total
  }

  rbind(synthdata, new_rows)
}


compute_covid_incidence_7d <- function(synthdata) {
  n_days <- TIME$n_days
  per_capita <- CONSTANTS$per_capita
  n_units <- max(synthdata$UnitNumeric)

  synthdata[["covid incidence"]] <- NA_real_

  for (unit_num in seq_len(n_units)) {
    unit_rows <- which(synthdata$UnitNumeric == unit_num)
    if (length(unit_rows) == 0) next

    pop <- synthdata[["Population"]][unit_rows[1]]
    cases <- synthdata[["AnzFallVortag"]][unit_rows]

    for (day_num in 6:(n_days - 1)) {
      day_range <- (day_num - 5):(day_num + 1)
      case_sum <- sum(cases[day_range], na.rm = TRUE)
      synthdata[["covid incidence"]][unit_rows[day_num]] <- case_sum / pop * per_capita
    }
  }

  synthdata
}


convert_vaccinations_to_rates <- function(synthdata) {
  vac_cols <- VACCINATION$dose_columns

  for (col in vac_cols) {
    if (col %in% colnames(synthdata)) {
      synthdata[[col]] <- synthdata[[col]] / synthdata[["Population"]]
    }
  }

  synthdata
}


compute_covid_growth_rate_7d <- function(synthdata) {
  synthdata[["incidence growth rate"]] <- NA_real_

  n_days <- TIME$n_days
  n_units <- max(synthdata$UnitNumeric)
  pct <- CONSTANTS$percent_multiplier

  for (unit_num in seq_len(n_units)) {
    unit_rows <- which(synthdata$UnitNumeric == unit_num)
    if (length(unit_rows) == 0) next

    incidence <- synthdata[["covid incidence"]][unit_rows]

    for (day_num in 14:n_days) {
      current_inc <- incidence[day_num]
      prev_inc <- incidence[day_num - 7]

      if (!is.na(current_inc) && !is.na(prev_inc) && prev_inc != 0) {
        growth_rate <- pct * (current_inc - prev_inc) / prev_inc
        synthdata[["incidence growth rate"]][unit_rows[day_num]] <- growth_rate
      }
    }
  }

  synthdata
}


compute_covid_incidence_14d <- function(synthdata) {
  synthdata[["14 days covid incidence"]] <- NA_real_

  n_days <- TIME$n_days
  n_units <- max(synthdata$UnitNumeric)
  per_capita <- CONSTANTS$per_capita

  for (unit_num in seq_len(n_units)) {
    unit_rows <- which(synthdata$UnitNumeric == unit_num)
    if (length(unit_rows) == 0) next

    pop <- synthdata[["Population"]][unit_rows[1]]
    cases <- synthdata[["AnzFallVortag"]][unit_rows]

    for (day_num in 13:(n_days - 1)) {
      day_rows_range <- (day_num - 12):(day_num + 1)
      case_sum <- sum(cases[day_rows_range], na.rm = TRUE)
      incidence <- case_sum / pop * per_capita
      synthdata[["14 days covid incidence"]][unit_rows[day_num]] <- incidence
    }
  }

  synthdata
}


compute_covid_growth_rate_14d <- function(synthdata) {
  synthdata[["14 days covid incidence growth rate"]] <- NA_real_

  n_days <- TIME$n_days
  n_units <- max(synthdata$UnitNumeric)
  pct <- CONSTANTS$percent_multiplier

  for (unit_num in seq_len(n_units)) {
    unit_rows <- which(synthdata$UnitNumeric == unit_num)
    if (length(unit_rows) == 0) next

    incidence <- synthdata[["14 days covid incidence"]][unit_rows]

    for (day_num in 27:n_days) {
      current_inc <- incidence[day_num]
      prev_inc <- incidence[day_num - 14]

      if (!is.na(current_inc) && !is.na(prev_inc) && prev_inc != 0) {
        growth_rate <- pct * (current_inc - prev_inc) / prev_inc
        synthdata[["14 days covid incidence growth rate"]][unit_rows[day_num]] <- growth_rate
      }
    }
  }

  synthdata
}


compute_hospitalization_growth_rate_7d <- function(synthdata) {
  synthdata[["hospitalization inc. growth rate"]] <- NA_real_

  n_days <- TIME$n_days
  pct <- CONSTANTS$percent_multiplier
  state_units <- UNITS$state_unit_range

  for (unit_num in state_units) {
    unit_rows <- which(synthdata$UnitNumeric == unit_num)
    if (length(unit_rows) == 0) next

    hosp_inc <- synthdata[["Hospitalization incidence 00+"]][unit_rows]

    for (day_num in 8:n_days) {
      current_inc <- hosp_inc[day_num]
      prev_inc <- hosp_inc[day_num - 7]

      if (!is.na(current_inc) && !is.na(prev_inc) && prev_inc != 0) {
        growth_rate <- pct * (current_inc - prev_inc) / prev_inc
        synthdata[["hospitalization inc. growth rate"]][unit_rows[day_num]] <- growth_rate
      }
    }
  }

  synthdata
}


compute_hospitalization_incidence_14d <- function(synthdata) {
  synthdata[["14 days hospitalization incidence"]] <- NA_real_

  n_days <- TIME$n_days
  per_capita <- CONSTANTS$per_capita
  state_units <- UNITS$state_unit_range

  for (unit_num in state_units) {
    unit_rows <- which(synthdata$UnitNumeric == unit_num)
    if (length(unit_rows) == 0) next

    hosp_7d <- synthdata[["Hospitalizations 00+"]][unit_rows]
    pop <- synthdata[["Population"]][unit_rows]

    for (day_num in 8:n_days) {
      hosp_current <- hosp_7d[day_num]
      hosp_prev <- hosp_7d[day_num - 7]

      if (!is.na(hosp_current) && !is.na(hosp_prev)) {
        hosp_14d <- (hosp_current + hosp_prev) / pop[day_num] * per_capita
        synthdata[["14 days hospitalization incidence"]][unit_rows[day_num]] <- hosp_14d
      }
    }
  }

  synthdata
}


compute_hospitalization_growth_rate_14d <- function(synthdata) {
  synthdata[["14 days hospitalization incidence growth rate"]] <- NA_real_

  n_days <- TIME$n_days
  pct <- CONSTANTS$percent_multiplier
  state_units <- UNITS$state_unit_range

  for (unit_num in state_units) {
    unit_rows <- which(synthdata$UnitNumeric == unit_num)
    if (length(unit_rows) == 0) next

    hosp_inc <- synthdata[["14 days hospitalization incidence"]][unit_rows]

    for (day_num in 22:n_days) {
      current_inc <- hosp_inc[day_num]
      prev_inc <- hosp_inc[day_num - 14]

      if (!is.na(current_inc) && !is.na(prev_inc) && prev_inc != 0) {
        growth_rate <- pct * (current_inc - prev_inc) / prev_inc
        synthdata[["14 days hospitalization incidence growth rate"]][unit_rows[day_num]] <- growth_rate
      }
    }
  }

  synthdata
}


add_county_type <- function(synthdata) {
  first_county_row <- which(synthdata$AdmUnitId == "01001")[1]
  if (is.na(first_county_row)) {
    synthdata[["County type"]] <- NA_character_
    return(synthdata)
  }

  synthdata <- synthdata |>
    mutate(
      `County type` = if_else(
        row_number() >= first_county_row & !is.na(Name),
        sapply(strsplit(Name, " "), `[`, 1),
        NA_character_
      )
    )

  synthdata
}


reorder_columns <- function(synthdata) {
  col_order <- c(
    "UnitNumeric", "AdmUnitId", "StateId", "Name", "DateNumeric", "Date",
    "AnzFallNeu", "AnzFallVortag", "AnzFallErkrankung", "AnzFallMeldung", "KumFall",
    "covid incidence", "incidence growth rate",
    "14 days covid incidence", "14 days covid incidence growth rate",
    "Area in Square Kilometers", "Population", "Male Population",
    "Female Population", "Population Density",
    "Unemployed", "Unemployed Foreigners", "Unemployed Age 15-20",
    "Unemployed Age 15-25", "Unemployed Age 55-65", "Long-term Unemployed",
    "Unemployment rate in relation to employed labor force",
    "Unemployment rate in relation to total labor force",
    "Unemployment rate of men in relation to total male labor force",
    "Unemployment rate of women in relation to total female labor force",
    "Unemployment rate of foreigners in relation to total foreign labor force",
    "Unemployment rate of people aged 15-25 in relation to total labor force aged 15-25",
    "First dose vaccinations", "Second dose vaccinations",
    "Third dose vaccinations", "Fourth dose vaccinations",
    "Fifth dose vaccinations", "Sixth dose vaccinations",
    "Hospitalizations 00+", "Hospitalizations 00-04", "Hospitalizations 05-14",
    "Hospitalizations 15-34", "Hospitalizations 35-59", "Hospitalizations 60-79",
    "Hospitalizations 80+",
    "Hospitalization incidence 00+", "Hospitalization incidence 00-04",
    "Hospitalization incidence 05-14", "Hospitalization incidence 15-34",
    "Hospitalization incidence 35-59", "Hospitalization incidence 60-79",
    "Hospitalization incidence 80+",
    "hospitalization inc. growth rate",
    "14 days hospitalization incidence",
    "14 days hospitalization incidence growth rate",
    "County type"
  )

  existing_cols <- col_order[col_order %in% colnames(synthdata)]
  missing_cols <- col_order[!col_order %in% colnames(synthdata)]

  if (length(missing_cols) > 0) {
    warning("Missing columns: ", paste(missing_cols, collapse = ", "))
    for (col in missing_cols) {
      synthdata[[col]] <- NA
    }
  }

  synthdata[, col_order]
}


rename_to_snake_case <- function(synthdata) {
  source(file.path(find_project_root(), "R/test_helpers.R"))
  mapping <- get_column_mapping()

  new_names <- colnames(synthdata)
  for (old_name in names(mapping)) {
    idx <- which(new_names == old_name)
    if (length(idx) > 0) {
      new_names[idx] <- mapping[[old_name]]
    }
  }

  colnames(synthdata) <- new_names
  synthdata
}


validate_and_write <- function(synthdata) {
  expected_rows <- UNITS$n_total * TIME$n_days
  expected_cols <- 56

  if (nrow(synthdata) != expected_rows) {
    stop(sprintf(
      "Row count mismatch: expected %d, got %d",
      expected_rows, nrow(synthdata)
    ))
  }

  if (ncol(synthdata) != expected_cols) {
    stop(sprintf(
      "Column count mismatch: expected %d, got %d",
      expected_cols, ncol(synthdata)
    ))
  }

  synthdata <- synthdata |>
    arrange(unit_numeric, date_numeric)

  output_dir <- file.path(find_project_root(), "data/processed")
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  output_path <- file.path(output_dir, "synthdata.parquet")
  write_parquet(synthdata, output_path)

  message(sprintf("Successfully wrote %s", output_path))
  message(sprintf("  Rows: %d, Columns: %d", nrow(synthdata), ncol(synthdata)))

  invisible(synthdata)
}


if (!identical(Sys.getenv("TESTTHAT"), "true") &&
    (sys.nframe() == 0 || !interactive())) {
  main()
}
