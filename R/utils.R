# utils.R

find_project_root <- function() {
  dir <- getwd()
  while (!dir.exists(file.path(dir, "R"))) {
    parent <- dirname(dir)
    if (parent == dir) stop("Could not find project root")
    dir <- parent
  }
  dir
}

source_config <- function() {
  source(file.path(find_project_root(), "R/config.R"))
}
