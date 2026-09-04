#!/usr/bin/env Rscript
# Full rebuild of the charter flight dataset for all in-scope seasons.
# Uses the committed HTML cache where available; pass --refresh to re-fetch.

suppressPackageStartupMessages({
  library(dplyr); library(purrr); library(readr); library(rvest)
  library(stringr); library(tibble); library(xml2); library(httr2)
  library(nflreadr); library(rlang)
})

for (f in list.files("R", pattern = "[.]R$", full.names = TRUE)) source(f)

refresh <- "--refresh" %in% commandArgs(trailingOnly = TRUE)
write_dataset(build_dataset(refresh = refresh))
