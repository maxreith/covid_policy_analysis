This repository contains my undergraduate thesis on COVID-19 containment measures in Germany. In April 2022, the states of Hamburg and Mecklenburg-Vorpommern were the only states to uphold a series of containment measures against the spread of COVID-19 that had been dropped in other states due to a change in national legislation. Upholding the measures was pretty controversial at the time, but provided a good natural experiment to evaluate the cause effects of these restrictions. In this project, I employed an empirical strategy based on the synthetic control method to estimate the restrictions' causal effects on COVID-19 cases and hospitalizations. For a good overview of synthetic controls, see [Abadie 2021](https://www.aeaweb.org/articles?id=10.1257/jel.20191450).

Using this method, I did not find any effects of the additional restrictions on the spread of COVID-19 and hospitalizations of patients in Hamburg and Mecklenburg-Vorpommern, suggesting that the methods were likely not effective.

## Refactoring Bad Code
The original project was really written in poor code. Perhaps that's unsurprising when you make your economics undergrads take econometrics 1-3 and statistics 1-2, but give no guidance on good coding practices whatsoever. And GPT-4 it wasn't even out by the time I wrote this. 

Now that agentic coding tools exist, I wanted to see whether Claude Code could refactor the original 2,300 lines of code. I approached this using test-driven development: first, I wrote tests to ensure that any output produced by the refactored code would be exactly identical to the output of the original script — `test-synthdata-comparison` is the most important of these. I then prompted Claude Code to read the test files, provided a list of bad coding practices to address, and let it refactor the code. The result is what you see here.

If you're someone who's about to undertake their first empirical project and don't have much coding experience yet, know that there are some great resources you should have a look at! I can recommend Hans-Martin von Gaudecker's course on [Effective Programming Practices for Economists](https://effective-programming-practices.vercel.app/landing-page.html) using Python, or Frank DiTraglia's [Core Empirical Research Methods](https://ditraglia.com/erm/) course using R.

## Repo Structure

```
covid_policy_analysis/
├── R/                              # Refactored R source code
│   ├── config.R                    # Centralized configuration (paths, constants)
│   ├── utils.R                     # General utility functions
│   ├── clean_and_merge_data.R      # Data cleaning pipeline
│   ├── synth_helpers.R             # Synthetic control helper functions
│   ├── test_helpers.R              # Test utility functions
│   ├── analysis_411_mv_covid.R     # MV COVID incidence analysis
│   ├── analysis_412_hh_covid.R     # Hamburg COVID incidence analysis
│   ├── analysis_421_mv_hosp.R      # MV hospitalization analysis
│   ├── analysis_422_hh_hosp.R      # Hamburg hospitalization analysis
│   ├── analysis_43_mv_cities.R     # Municipality-level analysis
│   └── create_figures_1_2.R        # Descriptive figures
├── scripts/                        # Entry point scripts
│   ├── run_analysis.R              # Run full analysis pipeline
│   ├── generate_baselines.R        # Generate test baselines
│   └── generate_figures_baseline.R # Generate figure baselines
├── tests/                          # Test suite
│   ├── testthat.R                  # Test runner
│   ├── testthat/                   # Test files
│   │   ├── test-synthdata-comparison.R  # Validates data pipeline output
│   │   ├── test-411-mv-covid.R     # Tests for MV COVID analysis
│   │   ├── test-412-hh-covid.R     # Tests for Hamburg COVID analysis
│   │   ├── test-421-mv-hosp.R      # Tests for MV hospitalization
│   │   ├── test-422-hh-hosp.R      # Tests for Hamburg hospitalization
│   │   ├── test-43-mv-cities.R     # Tests for municipality analysis
│   │   └── test-figures.R          # Tests for descriptive figures
│   └── baselines/                  # Baseline data for regression tests
├── data/                           # Data directory
│   ├── raw/                        # Raw input data (not committed)
│   └── processed/                  # Processed data files
├── results/                        # Analysis outputs (not committed)
├── original_project/               # Original codebase (read-only reference)
│   ├── Scripts/                    # Original R scripts
│   ├── Data/                       # Original data files
│   └── Figures and Tables/         # Original outputs
├── pixi.toml                       # Pixi environment configuration
├── README.md
└── Covid Measures...pdf            # Thesis PDF
```
