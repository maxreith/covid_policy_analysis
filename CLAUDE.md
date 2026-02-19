# CLAUDE.md

This file provides guidance to Claude Code when working with code in this repository.

## Mission

This project contains a complete COVID-19 policy analysis with sound methodology and valid results. **However, the code quality is poor and needs comprehensive refactoring using R best practices.**

The goal is to refactor the entire `original_project/` codebase while:
- Preserving the exact analytical approach and results
- Applying modern R coding standards
- Making the code maintainable, readable, and reproducible
- Following the best practices outlined in the global CLAUDE.md

**The conceptual procedure and results are good - we're fixing the implementation, not the analysis.**

## Refactoring Workflow

### Phase 1: Identify Issues (CURRENT PHASE)
Document all code quality issues in the current codebase, including:
- Hard-coded file paths and magic numbers
- Poor variable naming (unclear names, single letters like i/j)
- Lack of configuration management
- Missing documentation
- Code organization problems
- Lack of tests
- Violations of R best practices

### Phase 2: Build Test Suite
Write comprehensive tests that verify the refactored code produces identical results to the original analysis. This ensures we preserve the scientific validity while improving code quality.

### Phase 3: Refactor
Systematically refactor the codebase following best practices while ensuring all tests pass.

## Project Overview

This is a COVID-19 policy analysis project evaluating German state-level measures under the Hotspot Regulation using synthetic control methods. The analysis focuses on two treated states (Hamburg and Mecklenburg-Vorpommern) that implemented hotspot regulations on April 3, 2022.

**Key Facts:**
- **Data**: COVID cases, hospitalizations, vaccinations, unemployment, and population data for 419 German regional entities across 245 days (January 1 - September 2, 2022)
- **Method**: Synthetic control (using R package `Synth`)
- **Treatment Date**: April 3, 2022 (day 93)
- **Treated Units**: Hamburg (state), Mecklenburg-Vorpommern (state + disaggregated municipalities)
- **Primary Outcomes**: 14-day COVID incidence growth rate, hospitalization incidence

## Repository Structure

```
original_project/
├── Scripts/
│   ├── Data Import.R              # Data cleaning pipeline (~90 min runtime)
│   ├── Subsection 4.1.1 - Results.R  # MV COVID incidence analysis
│   ├── Subsection 4.1.2 - Results.R  # Hamburg COVID incidence analysis
│   ├── Subsection 4.2.1 - Results.R  # MV hospitalization analysis
│   ├── Subsection 4.2.2 - Results.R  # Hamburg hospitalization analysis
│   ├── Subsection 4.3 - Results.R    # Municipality-level analysis
│   └── Figures 1 and 2.R.R           # Descriptive figures
├── Data/
│   ├── raw data/
│   │   ├── RKI_History.csv           # Daily COVID cases (RKI)
│   │   ├── RKI_AdmUnit.csv           # Regional codes
│   │   ├── Impfungen Laender.csv     # State vaccinations
│   │   ├── Impfungen Landkreise.csv  # County vaccinations
│   │   ├── Hospitalisierungen.csv    # Hospitalizations by age
│   │   ├── Arbeitslose_Kreise.xlsx   # Unemployment (use "edited for data import" sheet)
│   │   └── Kreisfreie Staedte und Landkreise Flache, Bevoelkerung.xlsx  # Population
│   └── processed data/
│       └── synthdata.xlsx            # Final merged dataset (56 cols, 102,305 rows)
└── Figures and Tables/              # Generated outputs (35+ files)
```

## Data Pipeline

**Input**: 7 raw data files from German public health sources
**Output**: `synthdata.xlsx` with 419 regional units × 245 days = 102,305 observations

**Key transformations:**
1. Filter to 2022 data only
2. Aggregate Berlin districts into single city entity (AdmUnitId "11000")
3. Pad regional codes to 5 digits (e.g., "1001" → "01001")
4. Calculate vaccination rates (limited to April 15 for municipalities due to computation time)
5. Compute 7-day and 14-day COVID incidence and growth rates
6. Create aggregated Mecklenburg-Vorpommern entities (units 418-419)

## Analysis Scripts Pattern

All analysis scripts follow this structure:
1. Load packages (`Synth`, `readxl`, `writexl`)
2. Import `synthdata.xlsx`
3. Set locale for English date labels: `Sys.setlocale("LC_TIME", "C")`
4. Define treatment/control setup using `dataprep()`
5. Run synthetic control: `synth(data.prep.obj, method="BFGS")`
6. Generate validity checks:
   - Placebo tests (run synth for all donor states)
   - Leave-one-out donor robustness
   - Leave-one-out predictor robustness
7. Export tables (Excel) and figures (PNG)

**Common parameters:**
- Treatment date: April 3, 2022 (day 93)
- Optimization period: Feb 21 - April 3 (pre-treatment)
- Plot period: Feb 21 - May 15
- Donor pool: German states excluding city-states (units 2,4,6-11,13-17)

## Environment Management

This project uses **pixi** for environment and dependency management.

**Required R packages:**
- R (>=4.5, <4.6)
- r-dplyr (>=1.1.4)
- r-readr (>=2.1.6)
- r-readxl (>=1.4.5)
- Synth (synthetic control - install via R)
- writexl (install via R)

## Known Code Quality Issues

*(To be populated in Phase 1)*

### Configuration Issues
- Hard-coded file paths throughout scripts
- Magic numbers (day 93, April 15 cutoff) scattered in code
- No central configuration file

### Code Organization
- Poor variable naming (i, j for iterations; unclear names)
- No function documentation
- Repetitive code across analysis scripts
- Hard-coded row/column indices to access data

### Testing
- No automated tests
- No validation that results are reproducible
- Manual verification only

### Documentation
- Minimal inline comments
- No explanation of data transformations
- Assumptions not documented

*(More issues to be added as we analyze the codebase)*

## Best Practices to Follow

All refactored code should follow these principles:

### R Coding Standards
- Use native pipe `|>` instead of magrittr `%>%`
- Follow tidyverse style guide for naming
- Use modern R 4.1+ features
- Write clear, descriptive variable names
- Minimize comments by writing self-documenting code

### Data Management
Apply the three rules for data cleaning:
1. Start with an empty DataFrame
2. Touch every variable just once
3. Touch with a pure function

Use "newspaper style" organization: high-level functions first, details later, `if __name__ == "__main__"` at bottom.

### Configuration
- Create config file for all constants (dates, paths, parameters)
- No magic numbers in analysis code
- Centralize file path management

### Testing
- Write unit tests for all data transformation functions
- Add integration tests that verify full pipeline output
- Test that refactored code produces identical results to original

### Git Workflow
- Commit regularly with clear messages
- Don't commit generated outputs (figures, tables, processed data)
- Keep `.gitignore` updated

## Important Data Quirks

- **Berlin aggregation**: RKI reports 12 districts; aggregated to single city (AdmUnitId "11000")
- **Regional code padding**: 4-digit codes padded to 5 digits for consistency
- **Vaccination limitation**: Municipality vaccinations only through April 15 (computational cost)
- **MV aggregates**: Units 418-419 are synthetic aggregated entities for subsection 4.3
- **Time format**: DateNumeric uses integers (1 = Jan 1, 245 = Sep 2, 2022)
- **Unit numbering**: 1-17 (states, non-sequential), 18-417 (municipalities), 418-419 (MV aggregates)
- **Locale requirement**: Must set `Sys.setlocale("LC_TIME", "C")` for English plot labels
- **Excel sheets**: Some files use "edited" sheets instead of first sheet

## Running the Original Analysis

```r
# Set working directory to original_project/
setwd("original_project")

# Optional: Regenerate synthdata.xlsx (~90 min)
source("Scripts/Data Import.R")

# Run individual analyses
source("Scripts/Subsection 4.1.1 - Results.R")  # MV COVID incidence
source("Scripts/Subsection 4.1.2 - Results.R")  # Hamburg COVID incidence
source("Scripts/Subsection 4.2.1 - Results.R")  # MV hospitalization
source("Scripts/Subsection 4.2.2 - Results.R")  # Hamburg hospitalization
source("Scripts/Subsection 4.3 - Results.R")    # Municipality level
source("Scripts/Figures 1 and 2.R.R")           # Descriptive figures
```

Note: Analysis scripts can run independently without re-running Data Import.R if synthdata.xlsx exists.
