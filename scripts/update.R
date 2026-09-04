#!/usr/bin/env Rscript
# Weekly incremental refresh.
#
# Re-crawls the tag archive so newly published posts are discovered, then
# rebuilds. Posts already in cache/html/ are not re-fetched, so a normal run
# touches the network only for the new week.

suppressPackageStartupMessages({
  library(dplyr); library(purrr); library(readr); library(rvest)
  library(stringr); library(tibble); library(xml2); library(httr2)
  library(nflreadr); library(rlang)
})

for (f in list.files("R", pattern = "[.]R$", full.names = TRUE)) source(f)

write_dataset(build_dataset())
