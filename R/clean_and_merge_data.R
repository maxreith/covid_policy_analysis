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


if (!exists("TIME") || !exists("UNITS") || !exists("SLIDING_WINDOW")) {
  source(file.path(find_project_root(), "R/config.R"))
}

if (!exists("get_column_mapping")) {
  source(file.path(find_project_root(), "R/test_helpers.R"))
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
  synthdata <- synthdata |> arrange(UnitNumeric, DateNumeric)
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
    filter(Datum >= TIME$start_date, Datum <= TIME$end_date)
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
  first_district <- UNITS$berlin_first_district_id
  berlin_id <- UNITS$berlin_admunit_id

  admunit_data |>
    mutate(
      is_first_berlin = AdmUnitId == first_district & row_number() == min(which(AdmUnitId == first_district)),
      AdmUnitId = if_else(is_first_berlin, berlin_id, AdmUnitId),
      Name = if_else(is_first_berlin, "Berlin", Name)
    ) |>
    select(-is_first_berlin)
}


load_population_data <- function() {
  path <- file.path(
    find_project_root(),
    "original_project/Data/raw data/Kreisfreie Staedte und Landkreise Flache, Bevoelkerung.xlsx"
  )

  df <- read_excel(path, sheet = EXCEL_SHEETS$population, range = POPULATION$excel_range)

  colnames(df) <- c(
    "AdmUnitId", "Name", "StateCode", "AreaType",
    "area_sq_km", "population", "population_male",
    "population_female", "population_density"
  )

  df <- df[-POPULATION$rows_to_drop, ]

  df <- df |>
    filter(!is.na(StateCode))

  df <- df |>
    mutate(
      AdmUnitId = if_else(row_number() == n(), UNITS$germany_admunit_id, AdmUnitId)
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

  last_row <- df |>
    mutate(row_idx = row_number()) |>
    filter(AdmUnitId == POPULATION$last_municipality_id) |>
    slice(1) |>
    pull(row_idx)
  df <- df |> slice(1:last_row)

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
  read_csv(path, show_col_types = FALSE) |>
    mutate(
      Datum = as.Date(Datum),
      Bundesland_Id = if_else(
        Bundesland_Id == UNITS$germany_admunit_id_alt,
        UNITS$germany_admunit_id,
        Bundesland_Id
      )
    )
}


build_panel_skeleton <- function(covid_data, admunit_data) {
  region_ids <- admunit_data |> pull(AdmUnitId)

  covid_data |>
    filter(AdmUnitId %in% region_ids) |>
    mutate(AdmUnitId = factor(AdmUnitId, levels = region_ids)) |>
    arrange(AdmUnitId, Datum) |>
    group_by(AdmUnitId) |>
    mutate(DateNumeric = row_number()) |>
    ungroup() |>
    mutate(AdmUnitId = droplevels(AdmUnitId)) |>
    mutate(UnitNumeric = as.integer(AdmUnitId)) |>
    mutate(AdmUnitId = as.character(AdmUnitId)) |>
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
    mutate(Name = NA_character_) |>
    select(
      UnitNumeric, AdmUnitId, StateId, Name, DateNumeric, Date,
      AnzFallNeu, AnzFallVortag, AnzFallErkrankung, AnzFallMeldung, KumFall
    )
}


aggregate_berlin <- function(synthdata, admunit_data) {
  berlin_id <- UNITS$berlin_admunit_id
  first_district <- UNITS$berlin_first_district_id
  berlin_district_ids <- sprintf("1100%d", UNITS$berlin_district_nums)

  dates <- synthdata |>
    filter(AdmUnitId == UNITS$germany_admunit_id) |>
    pull(Date) |>
    unique()

  numeric_cols <- c("AnzFallNeu", "AnzFallVortag", "AnzFallErkrankung",
                    "AnzFallMeldung", "KumFall")

  berlin_agg <- synthdata |>
    filter(AdmUnitId %in% berlin_district_ids) |>
    group_by(Date) |>
    summarise(
      StateId = first(StateId),
      AnzFallNeu = sum(AnzFallNeu, na.rm = TRUE),
      AnzFallVortag = sum(AnzFallVortag, na.rm = TRUE),
      AnzFallErkrankung = sum(AnzFallErkrankung, na.rm = TRUE),
      AnzFallMeldung = sum(AnzFallMeldung, na.rm = TRUE),
      KumFall = sum(KumFall, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(DateNumeric = row_number())

  berlin_updates <- berlin_agg |>
    select(Date, DateNumeric, all_of(numeric_cols)) |>
    mutate(AdmUnitId = berlin_id)

  berlin_row_indices <- synthdata |>
    mutate(row_idx = row_number()) |>
    filter(AdmUnitId == first_district) |>
    pull(row_idx)

  synthdata <- synthdata |>
    mutate(row_idx = row_number()) |>
    left_join(
      berlin_updates |>
        mutate(row_idx = berlin_row_indices),
      by = "row_idx",
      suffix = c("", "_new")
    ) |>
    mutate(
      across(
        all_of(numeric_cols),
        ~ coalesce(.data[[paste0(cur_column(), "_new")]], .x)
      ),
      AdmUnitId = coalesce(AdmUnitId_new, AdmUnitId),
      Date = coalesce(Date_new, Date),
      DateNumeric = coalesce(DateNumeric_new, DateNumeric)
    ) |>
    select(-ends_with("_new"), -row_idx)

  other_berlin_ids <- berlin_district_ids[berlin_district_ids != first_district]
  synthdata <- synthdata |>
    filter(!AdmUnitId %in% other_berlin_ids)

  synthdata <- synthdata |>
    arrange(UnitNumeric, DateNumeric)

  unit_ids <- synthdata |> distinct(UnitNumeric) |> pull(UnitNumeric)
  unit_mapping <- setNames(seq_along(unit_ids), unit_ids)
  synthdata <- synthdata |>
    mutate(UnitNumeric = unit_mapping[as.character(UnitNumeric)])

  synthdata
}


pad_admunit_ids <- function(synthdata) {
  state_ids_1digit <- UNITS$state_ids_1digit
  state_ids_2digit <- UNITS$state_ids_2digit
  min_county_4digit <- UNITS$min_county_id_4digit
  id_len_unpadded <- UNITS$admunit_id_length_unpadded

  synthdata <- synthdata |>
    mutate(
      AdmUnitId = if_else(
        AdmUnitId %in% state_ids_1digit,
        paste0("0", AdmUnitId),
        AdmUnitId
      )
    )

  synthdata <- synthdata |>
    mutate(
      needs_padding = nchar(AdmUnitId) == id_len_unpadded &
        !AdmUnitId %in% c(UNITS$first_county_admunit_id_unpadded, state_ids_2digit),
      needs_padding = needs_padding |
        (nchar(AdmUnitId) == id_len_unpadded & as.numeric(AdmUnitId) >= min_county_4digit),
      needs_padding = if_else(is.na(needs_padding), FALSE, needs_padding),
      AdmUnitId = if_else(needs_padding, paste0("0", AdmUnitId), AdmUnitId)
    ) |>
    select(-needs_padding)

  synthdata
}


add_names <- function(synthdata, admunit_data) {
  admunit_fixed <- get_admunit_with_berlin_fix(admunit_data)

  first_county_row <- synthdata |>
    mutate(row_idx = row_number()) |>
    filter(AdmUnitId == UNITS$first_county_admunit_id) |>
    slice(1) |>
    pull(row_idx)

  synthdata <- synthdata |>
    mutate(is_state_row = row_number() < first_county_row)

  name_lookup <- admunit_fixed |>
    select(AdmUnitId, Name) |>
    distinct()

  state_rows <- synthdata |>
    filter(is_state_row) |>
    select(-is_state_row) |>
    left_join(
      name_lookup |> rename(LookedUpName = Name),
      by = c("StateId" = "AdmUnitId")
    ) |>
    mutate(Name = LookedUpName) |>
    select(-LookedUpName)

  county_rows <- synthdata |>
    filter(!is_state_row) |>
    select(-is_state_row) |>
    mutate(
      UnpaddedId = sub("^0+", "", AdmUnitId),
      UnpaddedId = if_else(UnpaddedId == "", "0", UnpaddedId)
    ) |>
    left_join(
      name_lookup |> rename(LookedUpName = Name),
      by = c("UnpaddedId" = "AdmUnitId")
    )

  has_missing <- county_rows |>
    filter(is.na(LookedUpName)) |>
    nrow() > 0

  if (has_missing) {
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

  bind_rows(state_rows, county_rows) |>
    arrange(UnitNumeric, DateNumeric)
}


join_population_data <- function(synthdata, population_data) {
  pop_cols <- c("area_sq_km", "population", "population_male",
                "population_female", "population_density")
  id_len_unpadded <- UNITS$admunit_id_length_unpadded

  pop_lookup <- population_data |>
    select(AdmUnitId, all_of(pop_cols)) |>
    mutate(
      AdmUnitId_padded = if_else(
        nchar(AdmUnitId) == id_len_unpadded,
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

  missing_unit_pop <- unit_pop |> filter(is.na(population))
  if (nrow(missing_unit_pop) > 0) {
    missing_units <- missing_unit_pop |> pull(UnitNumeric)

    missing_lookup <- synthdata_first_rows |>
      filter(UnitNumeric %in% missing_units) |>
      mutate(unpadded_id = sub("^0", "", AdmUnitId)) |>
      left_join(
        pop_lookup |> select(AdmUnitId, all_of(pop_cols)),
        by = c("unpadded_id" = "AdmUnitId")
      ) |>
      select(UnitNumeric, all_of(pop_cols))

    unit_pop <- unit_pop |>
      rows_update(missing_lookup, by = "UnitNumeric")
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
  id_len_unpadded <- UNITS$admunit_id_length_unpadded

  unempl_lookup <- unemployment_data |>
    mutate(
      AdmUnitId_padded = if_else(
        nchar(AdmUnitId) == id_len_unpadded,
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

  missing_unit_unempl <- unit_unempl |> filter(is.na(.data[[unempl_cols[1]]]))
  if (nrow(missing_unit_unempl) > 0) {
    missing_units <- missing_unit_unempl |> pull(UnitNumeric)

    missing_lookup <- synthdata_first_rows |>
      filter(UnitNumeric %in% missing_units) |>
      left_join(
        unempl_lookup |> select(AdmUnitId, all_of(unempl_cols)),
        by = "AdmUnitId"
      ) |>
      select(UnitNumeric, all_of(unempl_cols))

    unit_unempl <- unit_unempl |>
      rows_update(missing_lookup, by = "UnitNumeric")
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
  max_dose <- vac_data |> summarise(max(Impfserie)) |> pull()
  state_units <- UNITS$state_unit_range[-1]

  vac_daily <- vac_data |>
    group_by(BundeslandId_Impfort, Impfserie, Impfdatum) |>
    summarise(daily_total = sum(Anzahl, na.rm = TRUE), .groups = "drop")

  vac_cumsum <- vac_daily |>
    group_by(BundeslandId_Impfort, Impfserie) |>
    arrange(Impfdatum) |>
    mutate(cumsum_total = cumsum(daily_total)) |>
    ungroup() |>
    select(BundeslandId_Impfort, Impfserie, Impfdatum, cumsum_total)

  vac_wide <- vac_cumsum |>
    pivot_wider(
      id_cols = c(BundeslandId_Impfort, Impfdatum),
      names_from = Impfserie,
      values_from = cumsum_total,
      names_prefix = "dose_"
    )

  dose_name_map <- setNames(
    paste0("dose_", seq_len(max_dose)),
    vac_cols[seq_len(max_dose)]
  )
  vac_wide <- vac_wide |>
    rename(!!!dose_name_map)

  earliest_date <- vac_wide |> summarise(min(Impfdatum)) |> pull()
  all_dates <- seq(earliest_date, TIME$end_date, by = "day")
  all_states <- vac_wide |> distinct(BundeslandId_Impfort) |> pull(BundeslandId_Impfort)
  active_vac_cols <- vac_cols[seq_len(max_dose)]

  complete_grid <- expand.grid(
    BundeslandId_Impfort = all_states,
    Impfdatum = all_dates,
    stringsAsFactors = FALSE
  )

  vac_complete <- complete_grid |>
    left_join(vac_wide, by = c("BundeslandId_Impfort", "Impfdatum")) |>
    arrange(BundeslandId_Impfort, Impfdatum) |>
    group_by(BundeslandId_Impfort) |>
    fill(all_of(active_vac_cols), .direction = "down") |>
    ungroup() |>
    filter(Impfdatum >= TIME$start_date)

  synthdata <- synthdata |>
    left_join(
      vac_complete,
      by = c("AdmUnitId" = "BundeslandId_Impfort", "Date" = "Impfdatum")
    ) |>
    mutate(
      across(
        any_of(active_vac_cols),
        ~ case_when(
          UnitNumeric %in% state_units & is.na(.x) ~ 0,
          !UnitNumeric %in% state_units ~ NA_real_,
          TRUE ~ .x
        )
      )
    )

  missing_vac_cols <- setdiff(vac_cols, names(synthdata))
  for (col in missing_vac_cols) {
    synthdata <- synthdata |> mutate(!!col := NA_real_)
  }

  synthdata
}


join_vaccination_counties <- function(synthdata, vac_data) {
  vac_cols <- VACCINATION$dose_columns
  max_dose <- vac_data |> summarise(max(Impfschutz)) |> pull()
  vaccination_cutoff <- TIME$vaccination_cutoff_day
  municipality_units <- UNITS$municipality_unit_range
  cutoff_date <- TIME$start_date + vaccination_cutoff - 1

  vac_data <- vac_data |>
    mutate(Impfdatum = as.Date(Impfdatum))

  initial_date <- TIME$start_date
  initial_vac <- vac_data |>
    filter(Impfdatum <= initial_date) |>
    group_by(LandkreisId_Impfort, Impfschutz) |>
    summarise(initial_total = sum(Anzahl, na.rm = TRUE), .groups = "drop")

  vac_daily <- vac_data |>
    filter(Impfdatum > initial_date, Impfdatum <= cutoff_date) |>
    group_by(LandkreisId_Impfort, Impfschutz, Impfdatum) |>
    summarise(daily_total = sum(Anzahl, na.rm = TRUE), .groups = "drop")

  all_dates <- seq(TIME$start_date, cutoff_date, by = "day")
  all_counties <- synthdata |>
    filter(UnitNumeric %in% municipality_units) |>
    distinct(AdmUnitId) |>
    pull(AdmUnitId)

  complete_grid <- expand.grid(
    LandkreisId_Impfort = all_counties,
    Impfschutz = seq_len(max_dose),
    Impfdatum = all_dates,
    stringsAsFactors = FALSE
  ) |>
    as_tibble()

  vac_filled <- complete_grid |>
    left_join(initial_vac, by = c("LandkreisId_Impfort", "Impfschutz")) |>
    left_join(vac_daily, by = c("LandkreisId_Impfort", "Impfschutz", "Impfdatum")) |>
    mutate(
      initial_total = if_else(is.na(initial_total), 0, initial_total),
      daily_total = if_else(is.na(daily_total), 0, daily_total)
    ) |>
    arrange(LandkreisId_Impfort, Impfschutz, Impfdatum) |>
    group_by(LandkreisId_Impfort, Impfschutz) |>
    mutate(
      cumsum_daily = cumsum(daily_total),
      cumsum_total = initial_total + cumsum_daily
    ) |>
    ungroup() |>
    select(LandkreisId_Impfort, Impfschutz, Impfdatum, cumsum_total)

  vac_wide <- vac_filled |>
    pivot_wider(
      id_cols = c(LandkreisId_Impfort, Impfdatum),
      names_from = Impfschutz,
      values_from = cumsum_total,
      names_prefix = "dose_"
    )

  dose_name_map <- setNames(
    paste0("dose_", seq_len(max_dose)),
    vac_cols[seq_len(max_dose)]
  )
  vac_wide <- vac_wide |>
    rename(!!!dose_name_map)

  synthdata <- synthdata |>
    left_join(
      vac_wide,
      by = c("AdmUnitId" = "LandkreisId_Impfort", "Date" = "Impfdatum"),
      suffix = c("", "_county")
    )

  for (dose in seq_len(max_dose)) {
    col_name <- vac_cols[dose]
    county_col <- paste0(col_name, "_county")
    if (county_col %in% names(synthdata)) {
      synthdata <- synthdata |>
        mutate(
          !!col_name := if_else(
            UnitNumeric %in% municipality_units,
            .data[[county_col]],
            .data[[col_name]]
          )
        ) |>
        select(-all_of(county_col))
    }
    message(sprintf("Completed dose %d of %d", dose, max_dose))
  }

  synthdata
}


join_hospitalization_data <- function(synthdata, hosp_data) {
  age_groups <- HOSPITALIZATION$age_groups
  hosp_count_cols <- paste0("Hospitalizations ", age_groups)
  hosp_inc_cols <- paste0("Hospitalization incidence ", age_groups)
  state_units <- UNITS$state_unit_range

  hosp_wide <- hosp_data |>
    select(Datum, Bundesland_Id, Altersgruppe,
           `7T_Hospitalisierung_Faelle`, `7T_Hospitalisierung_Inzidenz`) |>
    pivot_wider(
      id_cols = c(Datum, Bundesland_Id),
      names_from = Altersgruppe,
      values_from = c(`7T_Hospitalisierung_Faelle`, `7T_Hospitalisierung_Inzidenz`),
      names_glue = "{.value}_{Altersgruppe}"
    )

  rename_map <- setNames(
    c(paste0("7T_Hospitalisierung_Faelle_", age_groups),
      paste0("7T_Hospitalisierung_Inzidenz_", age_groups)),
    c(hosp_count_cols, hosp_inc_cols)
  )

  hosp_wide <- hosp_wide |>
    rename(!!!rename_map)

  synthdata <- synthdata |>
    left_join(
      hosp_wide,
      by = c("Date" = "Datum", "AdmUnitId" = "Bundesland_Id")
    ) |>
    mutate(
      across(
        all_of(c(hosp_count_cols, hosp_inc_cols)),
        ~ if_else(UnitNumeric %in% state_units, .x, NA_real_)
      )
    )

  synthdata
}


create_mv_aggregates <- function(synthdata) {
  n_days <- TIME$n_days

  mv_lk_names <- c("LK Vorpommern-Rügen", "LK Rostock")
  lk_units <- synthdata |>
    filter(StateId == UNITS$mecklenburg_vorpommern_state_id, DateNumeric == 1, Name %in% mv_lk_names) |>
    pull(UnitNumeric)

  synthdata <- create_aggregate_unit(
    synthdata, lk_units, UNITS$mv_counties_aggregated, "LK MV aggregated",
    use_single_date = TRUE
  )

  sk_units <- synthdata |>
    filter(StateId == UNITS$mecklenburg_vorpommern_state_id, DateNumeric == 1, grepl("^SK ", Name)) |>
    pull(UnitNumeric)

  synthdata <- create_aggregate_unit(
    synthdata, sk_units, UNITS$mv_cities_aggregated, "SK MV aggregated",
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

  existing_sum_cols <- intersect(sum_cols, colnames(synthdata))

  aggregated_sums <- synthdata |>
    filter(UnitNumeric %in% source_units) |>
    group_by(DateNumeric) |>
    summarise(
      across(
        all_of(existing_sum_cols),
        ~ if (all(is.na(.x))) NA_real_ else sum(.x, na.rm = TRUE)
      ),
      .groups = "drop"
    )

  template_rows <- synthdata |>
    filter(UnitNumeric == source_units[1]) |>
    select(-all_of(existing_sum_cols))

  new_rows <- template_rows |>
    left_join(aggregated_sums, by = "DateNumeric") |>
    mutate(
      UnitNumeric = new_unit_num,
      Name = new_name,
      AdmUnitId = NA_character_,
      StateId = NA_character_
    )

  if ("County type" %in% colnames(new_rows)) {
    new_rows <- new_rows |>
      mutate(`County type` = NA_character_)
  }

  if (use_single_date) {
    first_source_row <- synthdata |>
      filter(UnitNumeric %in% source_units) |>
      slice(1)
    first_date_numeric <- first_source_row |> pull(DateNumeric)
    first_date <- first_source_row |> pull(Date)
    new_rows <- new_rows |>
      mutate(
        DateNumeric = first_date_numeric,
        Date = first_date
      )
  }

  new_rows <- new_rows |>
    mutate(
      `Population Density` = Population / `Area in Square Kilometers`
    )

  unemp_rate_employed_col <- "Unemployment rate in relation to employed labor force"
  unemp_rate_total_col <- "Unemployment rate in relation to total labor force"

  other_unemp_rate_cols <- c(
    "Unemployment rate of men in relation to total male labor force",
    "Unemployment rate of women in relation to total female labor force",
    "Unemployment rate of foreigners in relation to total foreign labor force",
    "Unemployment rate of people aged 15-25 in relation to total labor force aged 15-25"
  )

  existing_other_unemp_cols <- intersect(other_unemp_rate_cols, colnames(new_rows))
  if (length(existing_other_unemp_cols) > 0) {
    new_rows <- new_rows |>
      mutate(across(all_of(existing_other_unemp_cols), ~ NA_real_))
  }

  if (unemp_rate_employed_col %in% colnames(synthdata)) {
    day1_source <- synthdata |>
      filter(UnitNumeric %in% source_units, DateNumeric == 1)

    labor_force_employed <- day1_source |>
      summarise(sum(Unemployed / .data[[unemp_rate_employed_col]], na.rm = TRUE)) |>
      pull()
    labor_force_total <- day1_source |>
      summarise(sum(Unemployed / .data[[unemp_rate_total_col]], na.rm = TRUE)) |>
      pull()

    new_rows <- new_rows |>
      mutate(
        !!unemp_rate_employed_col := Unemployed / labor_force_employed,
        !!unemp_rate_total_col := Unemployed / labor_force_total
      )
  }

  bind_rows(synthdata, new_rows)
}


compute_covid_incidence_7d <- function(synthdata) {
  per_capita <- CONSTANTS$per_capita

  synthdata |>
    group_by(UnitNumeric) |>
    mutate(
      `covid incidence` = slide_dbl(
        AnzFallVortag,
        \(x) sum(x, na.rm = TRUE) / first(Population) * per_capita,
        .before = SLIDING_WINDOW$covid_7d_before,
        .after = SLIDING_WINDOW$covid_7d_after,
        .complete = TRUE
      )
    ) |>
    ungroup()
}


convert_vaccinations_to_rates <- function(synthdata) {
  vac_cols <- VACCINATION$dose_columns
  existing_cols <- vac_cols[vac_cols %in% colnames(synthdata)]

  synthdata |>
    mutate(
      across(
        all_of(existing_cols),
        \(x) x / Population
      )
    )
}


compute_covid_growth_rate_7d <- function(synthdata) {
  pct <- CONSTANTS$percent_multiplier
  min_row <- SLIDING_WINDOW$covid_growth_7d_min_row
  lag_days <- SLIDING_WINDOW$lag_7d

  synthdata |>
    group_by(UnitNumeric) |>
    mutate(
      row_idx = row_number(),
      prev_incidence = lag(`covid incidence`, lag_days),
      `incidence growth rate` = if_else(
        row_idx >= min_row &
          !is.na(`covid incidence`) &
          !is.na(prev_incidence) &
          prev_incidence != 0,
        pct * (`covid incidence` - prev_incidence) / prev_incidence,
        NA_real_
      )
    ) |>
    select(-row_idx, -prev_incidence) |>
    ungroup()
}


compute_covid_incidence_14d <- function(synthdata) {
  per_capita <- CONSTANTS$per_capita

  synthdata |>
    group_by(UnitNumeric) |>
    mutate(
      `14 days covid incidence` = slide_dbl(
        AnzFallVortag,
        \(x) sum(x, na.rm = TRUE) / first(Population) * per_capita,
        .before = SLIDING_WINDOW$covid_14d_before,
        .after = SLIDING_WINDOW$covid_14d_after,
        .complete = TRUE
      )
    ) |>
    ungroup()
}


compute_covid_growth_rate_14d <- function(synthdata) {
  pct <- CONSTANTS$percent_multiplier
  min_row <- SLIDING_WINDOW$covid_growth_14d_min_row
  lag_days <- SLIDING_WINDOW$lag_14d

  synthdata |>
    group_by(UnitNumeric) |>
    mutate(
      row_idx = row_number(),
      prev_incidence_14d = lag(`14 days covid incidence`, lag_days),
      `14 days covid incidence growth rate` = if_else(
        row_idx >= min_row &
          !is.na(`14 days covid incidence`) &
          !is.na(prev_incidence_14d) &
          prev_incidence_14d != 0,
        pct * (`14 days covid incidence` - prev_incidence_14d) / prev_incidence_14d,
        NA_real_
      )
    ) |>
    select(-row_idx, -prev_incidence_14d) |>
    ungroup()
}


compute_hospitalization_growth_rate_7d <- function(synthdata) {
  pct <- CONSTANTS$percent_multiplier
  state_units <- UNITS$state_unit_range
  min_row <- SLIDING_WINDOW$hosp_growth_7d_min_row
  lag_days <- SLIDING_WINDOW$lag_7d

  synthdata |>
    group_by(UnitNumeric) |>
    mutate(
      row_idx = row_number(),
      prev_hosp_inc = lag(`Hospitalization incidence 00+`, lag_days),
      `hospitalization inc. growth rate` = if_else(
        UnitNumeric %in% state_units &
          row_idx >= min_row &
          !is.na(`Hospitalization incidence 00+`) &
          !is.na(prev_hosp_inc) &
          prev_hosp_inc != 0,
        pct * (`Hospitalization incidence 00+` - prev_hosp_inc) / prev_hosp_inc,
        NA_real_
      )
    ) |>
    select(-row_idx, -prev_hosp_inc) |>
    ungroup()
}


compute_hospitalization_incidence_14d <- function(synthdata) {
  per_capita <- CONSTANTS$per_capita
  state_units <- UNITS$state_unit_range
  min_row <- SLIDING_WINDOW$hosp_incidence_14d_min_row
  lag_days <- SLIDING_WINDOW$lag_7d

  synthdata |>
    group_by(UnitNumeric) |>
    mutate(
      row_idx = row_number(),
      hosp_prev_7d = lag(`Hospitalizations 00+`, lag_days),
      `14 days hospitalization incidence` = if_else(
        UnitNumeric %in% state_units &
          row_idx >= min_row &
          !is.na(`Hospitalizations 00+`) &
          !is.na(hosp_prev_7d),
        (`Hospitalizations 00+` + hosp_prev_7d) / Population * per_capita,
        NA_real_
      )
    ) |>
    select(-row_idx, -hosp_prev_7d) |>
    ungroup()
}


compute_hospitalization_growth_rate_14d <- function(synthdata) {
  pct <- CONSTANTS$percent_multiplier
  state_units <- UNITS$state_unit_range
  min_row <- SLIDING_WINDOW$hosp_growth_14d_min_row
  lag_days <- SLIDING_WINDOW$lag_14d

  synthdata |>
    group_by(UnitNumeric) |>
    mutate(
      row_idx = row_number(),
      prev_hosp_inc_14d = lag(`14 days hospitalization incidence`, lag_days),
      `14 days hospitalization incidence growth rate` = if_else(
        UnitNumeric %in% state_units &
          row_idx >= min_row &
          !is.na(`14 days hospitalization incidence`) &
          !is.na(prev_hosp_inc_14d) &
          prev_hosp_inc_14d != 0,
        pct * (`14 days hospitalization incidence` - prev_hosp_inc_14d) / prev_hosp_inc_14d,
        NA_real_
      )
    ) |>
    select(-row_idx, -prev_hosp_inc_14d) |>
    ungroup()
}


add_county_type <- function(synthdata) {
  first_county_row <- synthdata |>
    mutate(row_idx = row_number()) |>
    filter(AdmUnitId == UNITS$first_county_admunit_id) |>
    slice(1) |>
    pull(row_idx)

  if (length(first_county_row) == 0 || is.na(first_county_row)) {
    synthdata <- synthdata |>
      mutate(`County type` = NA_character_)
    return(synthdata)
  }

  synthdata |>
    mutate(
      `County type` = if_else(
        row_number() >= first_county_row & !is.na(Name),
        word(Name, 1),
        NA_character_
      )
    )
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

  missing_cols <- col_order[!col_order %in% colnames(synthdata)]

  if (length(missing_cols) > 0) {
    warning("Missing columns: ", paste(missing_cols, collapse = ", "))
    for (col in missing_cols) {
      synthdata <- synthdata |>
        mutate(!!col := NA)
    }
  }

  synthdata |>
    select(all_of(col_order))
}


rename_to_snake_case <- function(synthdata) {
  mapping <- get_column_mapping()

  synthdata |>
    rename(any_of(mapping))
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
