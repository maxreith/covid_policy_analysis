# utils.R
# Shared utility functions used across the project

#' Find the project root directory.
#'
#' Traverses upward from the current working directory to find the project root,
#' identified by the presence of an "R" subdirectory.
#'
#' @return Character string with the absolute path to the project root.
#' @examples
#' root <- find_project_root()
#' config_path <- file.path(root, "R/config.R")
find_project_root <- function() {
  dir <- getwd()
  while (!dir.exists(file.path(dir, "R"))) {
    parent <- dirname(dir)
    if (parent == dir) stop("Could not find project root")
    dir <- parent
  }
  dir
}

#' Source the config file from the project root.
#'
#' Loads R/config.R which contains all project constants and parameters.
#'
#' @return NULL (called for side effects of defining config variables)
source_config <- function() {
  source(file.path(find_project_root(), "R/config.R"))
}
