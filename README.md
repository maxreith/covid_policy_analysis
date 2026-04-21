# COVID-19 Policy Analysis

This repository contains my undergraduate thesis on COVID-19 containment measures in Germany. In April 2022, the states of Hamburg and Mecklenburg-Vorpommern were the only states to uphold a series of containment measures against the spread of COVID-19 that had been dropped in other states due to a change in national legislation. Upholding the measures was pretty controversial at the time, but provided a good natural experiment to evaluate the causal effects of these restrictions. In this project, I employ an empirical strategy based on the synthetic control method to estimate the restrictions' causal effects on COVID-19 cases and hospitalizations. For a good overview of synthetic controls, see [Abadie (2021)](https://www.aeaweb.org/articles?id=10.1257/jel.20191450).

Using this method, I did not find any reducing effects on the spread of COVID-19 and hospitalizations of patients in Hamburg and Mecklenburg-Vorpommern, suggesting that the methods were likely not effective.

## Code Refactoring
The original project suffered from "undergrad code": heavy on theory, light on best practices, and written before the advent of modern AI assistants. But with the rise of agentic coding tools, I wanted to see whether Claude Code could refactor the original 2,300 lines of code. Following test-driven development principles, I wrote a test suite, led by `test-synthdata-comparison`,  to ensure that any output produced by the refactored code would be identical to the output of the original script. I then instructed Claude Code to refactor the code using a list of best practices to follow, while making sure that all tests pass.

If you're about to undertake your first empirical project and don't have much coding experience yet, there are some great resources you should have a look at! I can recommend Hans-Martin von Gaudecker's course on [Effective Programming Practices for Economists](https://effective-programming-practices.vercel.app/landing-page.html) using Python, or Frank DiTraglia's [Core Empirical Research Methods](https://ditraglia.com/erm/) course using R.

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
