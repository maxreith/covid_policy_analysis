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

## IMPORTANT: Do Not Modify original_project/

**Never edit any files in the `original_project/` directory.** This folder is preserved as a read-only reference of the original codebase. All refactored code should be written to new locations outside of `original_project/`.

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

**Phase 1 Analysis Complete.** Below is a comprehensive list of all identified issues.

---

### 1. Magic Numbers (HIGH PRIORITY)

The codebase is riddled with unexplained numeric constants:

| Magic Number | Meaning | Files |
|-------------|---------|-------|
| `245` | Number of days in dataset (Jan 1 - Sep 2, 2022) | All scripts |
| `93` | Treatment date (April 3 = day 93) | All analysis scripts |
| `100` | April 15 cutoff for municipality vaccinations | Data Import.R:225 |
| `417`, `418`, `419` | Unit numbers (417 = last municipality, 418-419 = MV aggregates) | Data Import.R |
| `17` | Number of states + Germany aggregate | Multiple |
| `c(2,4,6:11,13:17)` | Donor pool state IDs | Analysis scripts |
| `c(4:6,2,3,1)` | Row reordering for table output | All analysis scripts |
| `5` | MSPE restriction multiplier | All analysis scripts |

**Examples:**
```r
# Data Import.R:54 - What is 417?
while (i<=417) {

# Data Import.R:225 - Why 100? (April 15)
while (i<=100) {

# Subsection 4.1.1:52 - Why 93?
list("Third dose vaccinations", 93 , "mean")

# Multiple files - What are these state IDs?
pool<-synthdata[which(synthdata$UnitNumeric%in%c(2,4,6:11,13:17)...
```

---

### 2. Hard-Coded Column/Row Indices (HIGH PRIORITY)

Columns and rows are accessed by numeric index instead of name, making code brittle and unreadable:

**Examples from Data Import.R:**
```r
# Line 21 - What rows are being dropped? Why?
population.data <- read_excel(..., range = "A7:I480")[-c(1,20,71,75,135,166,207,269,374,384,404,414,432,448),]

# Line 68 - What columns are 2:10?
synthdata[rownumbers,2:10] <- placeholder

# Line 110 - Shuffling columns with no explanation
synthdata[,6:12] <- synthdata[,4:10]
synthdata[,13:14] <- NA
synthdata[,4] <- NA

# Line 165 - What are columns 15:19?
synthdata[c((Time*(i-1)+1):(Time*i)), columns] <- population.data[identifier,5:9]

# Line 177 - What are columns 20:31?
synthdata[identifier,20:31] <- unempl.data[i,2:13]

# Line 320 - Complex column selection
synthdata[Time*417+i,c(7:11,15:18,20:25,32:37)]<-t(colSums(...))
```

---

### 3. Poor Variable Naming (HIGH PRIORITY)

**Single-letter variables used throughout:**
- `i`, `j`, `h` - loop counters (no semantic meaning)
- `x` - used for various purposes (line 52, 126, 247)
- `y` - unit numbering vector
- `a` - temporary list storage
- `b` - row index

**Unclear/generic names:**
- `placeholder` - holds temporary data during iteration
- `tofix` - positions of 4-digit AdmUnitIds
- `upperlimit` - last position of 4-digit IDs
- `loopparameter1` - column index (Data Import.R:293)
- `identifier` - row index for matching
- `rownumbers` - row indices
- `olddim` - original dimension of dataframe
- `x1` - age categories (line 247, 263)

**Examples:**
```r
# Data Import.R:52-57 - Completely opaque
x=vector()
i=1
while (i<=417) {
  x[((i-1)*Time+1):(Time*i)]<-i
  i<-i+1
}

# Data Import.R:80-93 - i, j with no semantic meaning
i=1
while (i<=length(Dates)) {
  j=1
  while (j<=dim(Berlinunits)[1]) {
    ...
  }
}
```

---

### 4. Duplicated Code (HIGH PRIORITY)

**Identical data import block in 6 files:**
Every analysis script (4.1.1, 4.1.2, 4.2.1, 4.2.2, 4.3, Figures) contains this ~45-line block:
```r
synthdata <- read_excel("Data/processed data/synthdata.xlsx",
                        col_types = c("numeric", "text", "text", ...)) # 56 types listed

y=1:10
i=1
while (i<=dim(synthdata)[1]/245) {
  y[((1+245*(i-1)):(245*i))]<-i
  i<-i+1
}
synthdata2<-as.data.frame(y)
...
```

**Identical dataprep() call pattern** repeated 4+ times per analysis script (for original, placebo, leave-one-out donor, leave-one-out predictor).

**Identical plotting pattern** (png(), plot(), lines(), legend(), dev.off()) repeated many times.

**Identical PostPre MSPE calculation** repeated in multiple scripts with only list name changes.

---

### 5. No Functions / No Modularization (HIGH PRIORITY)

- **Zero user-defined functions** across 2,000+ lines of code
- Everything is procedural scripts
- No reusable components
- Same logic copy-pasted instead of abstracted

**Should be functions:**
- `load_synthdata()` - standardized data loading
- `run_synthetic_control()` - wrapper for dataprep + synth
- `run_placebo_tests()` - placebo estimation loop
- `run_leave_one_out_donor()` - donor robustness loop
- `run_leave_one_out_predictor()` - predictor robustness loop
- `calculate_mspe_ratio()` - post/pre MSPE calculation
- `create_gaps_plot()` - standard gaps visualization
- `export_synth_tables()` - table export logic

---

### 6. While Loops Instead of For Loops or Vectorization (MEDIUM)

R code uses `while` loops where `for` loops or vectorized operations would be clearer:

```r
# Data Import.R:54-57 - Should be: for (i in 1:417)
i=1
while (i<=417) {
  x[((i-1)*Time+1):(Time*i)]<-i
  i<-i+1
}

# Could be vectorized:
x <- rep(1:417, each = Time)
```

Many loops could be replaced with `lapply()`, `sapply()`, or vectorized operations.

---

### 7. Hard-Coded File Paths (MEDIUM)

All paths are relative strings scattered throughout:

```r
# Data Import.R
"Data/raw data/RKI_History.csv"
"Data/raw data/RKI_AdmUnit.csv"
"Data/raw data/Kreisfreie Staedte und Landkreise Flache, Bevoelkerung.xlsx"
"Data/processed data/synthdata.xlsx"

# Analysis scripts
"Figures and Tables/Figure 3 - MV Growth Rate _ Comparison to Sample Mean.png"
"Figures and Tables/Table 1.xlsx"
```

No centralized path configuration. Assumes specific working directory.

---

### 8. Interactive Commands Left in Code (LOW)

`View()` calls left in production scripts:
```r
# Data Import.R lines 12, 17, 23, 29, 33, 38, 43, 71
View(RKI_History)
View(RKI_AdmUnit)
View(population.data)
...

# Analysis scripts line 26
View(synthdata)
```

These fail in non-interactive environments.

---

### 9. Debug Print Statements (LOW)

Progress `print()` statements left in code:
```r
# Data Import.R:234
print(h)

# Analysis scripts (multiple locations)
print(unit)
print(predictor.number)
```

---

### 10. Inconsistent Code Style (LOW)

**Spacing inconsistencies:**
```r
# Sometimes spaces around operators
i = 1
x = vector()

# Sometimes not
i=1
x=vector()

# Mixed in same file
synthdata[Time*j+i,52] <- 100*(synthdata$`...
```

**Inconsistent indentation** - mix of 2-space and arbitrary indentation.

**Line length** - many lines exceed 120+ characters.

---

### 11. Complex One-Liners (MEDIUM)

Extremely long, complex expressions that are hard to understand:

```r
# Data Import.R:21 - What is this filtering?
population.data <- read_excel("...", sheet = "edited", range = "A7:I480")[-c(1,20,71,75,135,166,207,269,374,384,404,414,432,448),]

# Data Import.R:148-149 - What is happening here?
upperlimit=which(synthdata[,2]=="9780")[length(which(synthdata[,2]=="9780"))]
tofix <- c((sum(synthdata[,2]=="0")+1):(which(synthdata[,2]=="10")[1]-1) , (which(synthdata[,2]=="1001")[1]):upperlimit)

# Subsection scripts - MSPE calculation (lines 224-230)
PostPre[length(PostPre)+1]<-
  (sum((placebolistMV[[i]][[1]][c((1+length(optimization.period)):length(plot.period)),]-
          placebolistMV[[i]][[2]][c((1+length(optimization.period)):length(plot.period)),] %*% placebolistMV[[i]][[3]]$solution.w)^2
  )/(length(plot.period)-length(optimization.period)))/
  (sum((placebolistMV[[i]][[1]][1:length(optimization.period),] -
          placebolistMV[[i]][[2]][1:length(optimization.period),] %*% placebolistMV[[i]][[3]]$solution.w)^2
  )/length(optimization.period))
```

---

### 12. No Error Handling (MEDIUM)

- No checks if files exist before reading
- No validation of data dimensions after import
- No guards against NA/NULL values in calculations
- No tryCatch blocks for potentially failing operations

---

### 13. No Configuration File (MEDIUM)

Parameters that should be in config:
- Treatment date (April 3, 2022 / day 93)
- Optimization period dates
- Plot period dates
- Donor pool unit IDs
- MSPE restriction value (5)
- April 15 cutoff for municipality vaccinations
- File paths
- Figure dimensions (1980x1080, pointsize=25, res=72)

---

### 14. Deprecated/Suboptimal Patterns (LOW)

- Using `$` extraction in loops instead of vectorized operations
- Using `dim(df)[1]` instead of `nrow(df)`
- Growing vectors in loops (`PostPre[length(PostPre)+1]`) instead of pre-allocation
- Not using tidyverse idioms where appropriate

---

### 15. Naming Convention Violations (LOW)

- File names with spaces: `"Figures 1 and 2.R.R"` (double extension)
- Mixed naming: `RKI_History` vs `vac.data` vs `synthdata`
- **Column names with spaces and special characters:**
  - `"14 days covid incidence growth rate"` → should be `covid_incidence_growth_rate_14d`
  - `"Hospitalization incidence 00+"` → should be `hospitalization_incidence_00plus`
  - `"Unemployment rate in relation to employed labor force"` → should be `unemployment_rate`
  - Spaces require backticks to access: `` synthdata$`14 days covid incidence growth rate` ``
  - Makes code harder to read and more error-prone

---

### 16. Script Organization Issues (MEDIUM)

- No clear separation of concerns
- Data loading, transformation, analysis, and output all in one long script
- Section markers using `####` comments create visual clutter
- No main() entry point pattern

---

### Summary: Priority Issues for Refactoring

**Must Fix (High Priority):**
1. Magic numbers → Create config file
2. Hard-coded column indices → Use column names
3. Duplicated code → Create shared functions
4. No functions → Modularize into functions
5. Poor variable naming → Use descriptive names

**Should Fix (Medium Priority):**
6. While loops → For loops or vectorization
7. Hard-coded paths → Centralized path config
8. Complex one-liners → Break into readable steps
9. No error handling → Add validation
10. No config file → Create config.R
11. Script organization → Separation of concerns

**Nice to Fix (Low Priority):**
12. View() calls → Remove
13. Print statements → Remove or use logging
14. Style inconsistencies → Apply formatter
15. Naming conventions → Standardize

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

## Known Bugs in Original Data Import (Replicated for Compatibility)

The refactored `R/clean_and_merge_data.R` replicates the following bugs from the original `Data Import.R` to ensure output matches exactly:

### 1. MV Aggregate Unit 418 DateNumeric Bug
**Location**: Original lines 313-324

The code intends to set DateNumeric and Date columns for unit 418 (LK MV aggregated) to the corresponding dates from a source unit. However, line 313:
```r
synthdata[(Time*417+1):(Time*418),5:6]<-synthdata[which(synthdata$UnitNumeric%in%LKnumbers)[1],5:6]
```
Only uses the FIRST row (`[1]`) from the source, causing ALL 245 rows of unit 418 to have DateNumeric=1 and Date=2022-01-01, rather than incrementing 1-245 with corresponding dates.

**Impact**: Unit 418 has DateNumeric=1 for all 245 rows, while unit 419 has correct DateNumeric values 1-245.

### 2. MV Aggregate Unit 419 Date Inheritance
**Location**: Original line 348

In contrast to unit 418, unit 419 uses:
```r
synthdata[(Time*418+1):(Time*419),5:6]<-synthdata[which(synthdata$UnitNumeric%in%LKnumbers[1]),5:6]
```
Note `LKnumbers[1]` (without `which`), which selects ALL 245 rows from the first source unit, correctly inheriting all dates. This asymmetry appears unintentional.

### 3. Incomplete Unemployment Rate Columns for MV Aggregates
**Location**: Original lines 326-341, 361-377

Only two unemployment rate columns are recalculated for units 418-419:
- "Unemployment rate in relation to employed labor force" (column 26)
- "Unemployment rate in relation to total labor force" (column 27)

The other four unemployment rate columns remain as NA for MV aggregates, even though they could be computed similarly.

**Impact**: These columns have NA values for units 418-419 in the final output.

### 4. colSums NA Propagation
**Location**: Original line 320

The original uses `colSums()` without `na.rm=TRUE`. When source columns contain NA (e.g., vaccination data for days 101-245), the sum returns NA rather than 0. The refactored code replicates this by checking if all values are NA before summing.

These bugs are documented here rather than fixed, because:
1. The test suite verifies exact numerical match to original output
2. The MV aggregate units (418-419) are only used in subsection 4.3 analysis
3. The scientific conclusions are unaffected by these data anomalies
