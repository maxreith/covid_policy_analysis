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
  berlin_district_nums = 1L:12L
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
  percent_multiplier = 100L
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
