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
  synthdata = "data/processed/synthdata.feather"
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

  # State unit IDs (UnitNumeric values)
  germany_aggregate = 1L,
  hamburg = 3L,
  mecklenburg_vorpommern = 14L,

  # Special aggregated units
  mv_counties_aggregated = 418L,
  mv_cities_aggregated = 419L,

  # Donor pool for state-level analysis (excludes city-states and specific regions)
  donor_pool_states = c(2L, 4L, 6L:11L, 13L:17L),

  # Berlin districts that get aggregated
  berlin_admunit_id = "11000"
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
