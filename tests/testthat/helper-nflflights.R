suppressPackageStartupMessages({
  library(dplyr); library(purrr); library(readr); library(rvest)
  library(stringr); library(tibble); library(xml2); library(httr2)
  library(nflreadr); library(rlang)
})

# Tests run from tests/testthat/; the project root is two levels up.
proj_root <- normalizePath(file.path("..", ".."), mustWork = FALSE)
if (!dir.exists(file.path(proj_root, "R"))) proj_root <- normalizePath(".", mustWork = FALSE)

for (f in list.files(file.path(proj_root, "R"), pattern = "[.]R$", full.names = TRUE)) source(f)

# Pin the project root so reference tables and outputs resolve identically
# whether tests are run via test_dir() or R CMD check.
options(nflflights.root = proj_root)

proj_file <- function(...) file.path(proj_root, ...)

fixture <- function(slug) {
  path <- proj_file("tests", "testthat", "fixtures", paste0(slug, ".html"))
  parse_post(readChar(path, file.size(path), useBytes = TRUE), slug)
}
