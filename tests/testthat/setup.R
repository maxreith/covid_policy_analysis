# setup.R
# Testthat setup file - runs before all tests

library(testthat)

# Find project root (where R/ directory is located)
# testthat runs from tests/testthat/, so go up two levels
project_root <- normalizePath(file.path(dirname(getwd()), ".."))

# Source all R files from R/ directory
r_dir <- file.path(project_root, "R")
if (dir.exists(r_dir)) {
  r_files <- list.files(
    path = r_dir,
    pattern = "\\.R$",
    full.names = TRUE
  )

  for (file in r_files) {
    source(file)
  }
}

# Set locale for consistent date formatting in tests
Sys.setlocale("LC_TIME", "C")
