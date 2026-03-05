# config.R
# Central configuration for all constants, paths, and parameters

# === File Paths ===
PATHS <- list(
  raw_data = "data/raw",
  processed_data = "data/processed",
  results = "results",

  # Raw data files
  rki_history = "data/raw/RKI_History.csv",
  rki_admunit = "data/raw/RKI_AdmUnit.csv",
  population = "data/raw/Kreisfreie Staedte und Landkreise Flache, Bevoelkerung.xlsx",
  vaccinations_states = "data/raw/Impfungen Laender.csv",
  vaccinations_counties = "data/raw/Impfungen Landkreise.csv",
  unemployment = "data/raw/Arbeitslose_Kreise.xlsx",
  hospitalizations = "data/raw/Hospitalisierungen.csv",

  # Processed data
  synthdata = "data/processed/synthdata.parquet"
)

# === Time Constants ===
TIME <- list(
  n_days = 245L,
  start_date = as.Date("2022-01-01"),
  end_date = as.Date("2022-09-02"),
  treatment_date = as.Date("2022-04-03"),
  treatment_day = 93L,
  vaccination_cutoff_day = 100L,
  vaccination_cutoff_date = as.Date("2022-04-15")
)

# === Unit Constants ===
UNITS <- list(
  n_total = 419L,
  n_states = 17L,
  n_municipalities = 400L,
  last_municipality = 417L,

  # State unit IDs (UnitNumeric values)
  germany_aggregate = 1L,
  hamburg = 3L,
  mecklenburg_vorpommern = 14L,
  mecklenburg_vorpommern_state_id = "13",

  # Special aggregated units
  mv_counties_aggregated = 418L,
  mv_cities_aggregated = 419L,

  # Unit ranges for different entity types
  state_unit_range = 1L:17L,
  municipality_unit_range = 18L:417L,

  # State names by UnitNumeric
  state_names = c(
    "Bundesrepublik Deutschland",  # 1
    "Schleswig-Holstein",          # 2
    "Hamburg",                     # 3
    "Niedersachsen",               # 4
    "Bremen",                      # 5
    "Nordrhein-Westfalen",         # 6
    "Hessen",                      # 7
    "Rheinland-Pfalz",             # 8
    "Baden-Wuerttemberg",          # 9
    "Bayern",                      # 10
    "Saarland",                    # 11
    "Berlin",                      # 12
    "Brandenburg",                 # 13
    "Mecklenburg-Vorpommern",      # 14
    "Sachsen",                     # 15
    "Sachsen-Anhalt",              # 16
    "Thueringen"                   # 17
 ),

  # Donor pool for state-level analysis
  # Excludes: 1 (Germany), 3 (Hamburg), 5 (Bremen), 12 (Berlin)
  # City-states excluded as they differ structurally from territorial states
  donor_pool_states = c(2L, 4L, 6L:11L, 13L:17L),

  # Berlin districts that get aggregated
  berlin_admunit_id = "11000",
  berlin_first_district_id = "11001",
  berlin_district_nums = 1L:12L,

  # Germany aggregate
  germany_admunit_id = "0",
  germany_admunit_id_alt = "00",  # Alternative format in hospitalization data

  # First county (used as marker for county vs state rows)
  first_county_admunit_id = "01001",
  first_county_admunit_id_unpadded = "1001",

  # AdmUnitId padding rules
  state_ids_1digit = as.character(1:9),
  state_ids_2digit = as.character(10:16),
  min_county_id_4digit = 1001L,
  admunit_id_length_unpadded = 4L
)

# === Synthetic Control Parameters ===
SYNTH <- list(
  mspe_restriction = 5,
  optimization_method = "BFGS",

  # Analysis periods
  optimization_start = as.Date("2022-02-21"),
  optimization_end = as.Date("2022-04-03"),
  plot_start = as.Date("2022-02-21"),
  plot_end = as.Date("2022-05-15"),

  # Predictor observation days
  predictor_days = c(65L, 79L, 93L)
)

# === Column Names ===
COLUMNS <- list(
  dependent_covid = "14 days covid incidence growth rate",
  dependent_hosp = "14 days hospitalization incidence growth rate",
  dependent_hosp_level = "14 days hospitalization incidence",

  predictors = c(
    "Third dose vaccinations",
    "Unemployment rate in relation to employed labor force",
    "Population Density"
  )
)

# === Figure Settings ===
FIGURE <- list(
  width = 1980L,
  height = 1080L,
  pointsize = 25L,
  res = 72L,
  line_width = 4L
)

# === Excel Sheet Names ===
EXCEL_SHEETS <- list(
  population = "edited",
  unemployment = "edited for data import"
)

# === Numeric Constants ===
CONSTANTS <- list(
  per_capita = 100000L,
  percent_multiplier = 100L,
  expected_columns = 56L
)

# === Sliding Window Parameters ===
# For slider::slide_dbl with .complete = TRUE
# Window size = .before + 1 (current) + .after
SLIDING_WINDOW <- list(
  # Window sizes in days
  window_7d = 7L,
  window_14d = 14L,

  # 7-day COVID incidence: window of 7 days
  covid_7d_before = 5L,
  covid_7d_after = 1L,

  # 14-day COVID incidence: window of 14 days
  covid_14d_before = 12L,
  covid_14d_after = 1L,

  # Lag periods for growth rate calculations
  lag_7d = 7L,
  lag_14d = 14L,

  # Minimum row index for valid growth rate calculations
  # Growth rate needs valid incidence at current row AND n days earlier
  covid_growth_7d_min_row = 14L,   # 7-day incidence valid from ~row 7, plus 7-day lag
  covid_growth_14d_min_row = 27L,  # 14-day incidence valid from ~row 13, plus 14-day lag

  # Hospitalization calculations
  hosp_growth_7d_min_row = 8L,     # Need current + 7-day lag
  hosp_incidence_14d_min_row = 8L, # Sum of current 7-day + previous 7-day hospitalizations
  hosp_growth_14d_min_row = 22L    # 14-day hosp incidence valid from ~row 8, plus 14-day lag
)

# === Population Data Config ===
POPULATION <- list(
  rows_to_drop = c(1, 20, 71, 75, 135, 166, 207, 269, 374, 384, 404, 414, 432, 448),
  excel_range = "A7:I480",
  last_municipality_id = "16077"
)

# === Vaccination Column Names ===
VACCINATION <- list(
  dose_columns = c(
    "First dose vaccinations", "Second dose vaccinations",
    "Third dose vaccinations", "Fourth dose vaccinations",
    "Fifth dose vaccinations", "Sixth dose vaccinations"
  )
)

# === Age Groups for Hospitalization ===
HOSPITALIZATION <- list(
  age_groups = c("00+", "00-04", "05-14", "15-34", "35-59", "60-79", "80+")
)

# === Analysis Configurations ===
# Section 4.1.1: MV COVID Incidence Growth Rate
ANALYSIS_411 <- list(
  name = "section_411",
  treated_name = "Mecklenburg-Vorpommern",
  var_dependent = "14 days covid incidence growth rate",
  predictor_days = c(65L, 79L, 93L),
  pool_type = "state",
  table_prefix = c("table1", "table2")
)

# Section 4.1.2: Hamburg COVID Incidence Growth Rate
ANALYSIS_412 <- list(
  name = "section_412",
  treated_name = "Hamburg",
  var_dependent = "14 days covid incidence growth rate",
  predictor_days = c(65L, 79L, 93L),
  pool_type = "hamburg_covid",
  table_prefix = c("table3", "table4")
)

# Section 4.2.1: MV Hospitalization Growth Rate
ANALYSIS_421 <- list(
  name = "section_421",
  treated_name = "Mecklenburg-Vorpommern",
  var_dependent = "14 days hospitalization incidence growth rate",
  predictor_days = c(65L, 72L, 93L),
  pool_type = "state",
  table_prefix = c("table5", "table6")
)

# Section 4.2.2: Hamburg Hospitalization Incidence Level
ANALYSIS_422 <- list(
  name = "section_422",
  treated_name = "Hamburg",
  var_dependent = "14 days hospitalization incidence",
  predictor_days = c(58L, 79L, 93L),
  pool_type = "hamburg_hosp",
  table_prefix = c("table7", "table8")
)

# Section 4.3: MV Cities Municipality-Level
ANALYSIS_43 <- list(
  name = "section_43",
  treated_name = "SK MV aggregated",
  var_dependent = "14 days covid incidence growth rate",
  predictor_days = c(65L, 72L, 93L),
  pool_type = "mv_cities",
  table_prefix = c("table9", "table10"),
  run_robustness = FALSE
)

# === Figures 1 and 2 Configuration ===
FIGURES <- list(
  date_start = as.Date("2022-02-01"),
  date_end = as.Date("2022-06-01"),

  donor_states = c(2L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 15L, 16L, 17L),
  hamburg_unit = 3L,
  mv_unit = 14L,

  covid_var = "covid incidence",
  hosp_var = "Hospitalization incidence 00+",

  colors = list(hamburg = 2L, mv = 3L, donors = 8L),
  line_width = list(main = 4L, donors = 1.5),

  output_fig1 = "results/Figure 1 - COVID Incidence.png",
  output_fig2 = "results/Figure 2 - Hospitalization Incidence.png"
)
