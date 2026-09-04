# HTTP fetching with an on-disk cache.
#
# Every post is cached under cache/html/<slug>.html and committed, which makes
# the backfill reproducible, keeps CI incremental (only new slugs hit the
# network), and insulates the history from the blog changing or disappearing.

USER_AGENT <- "nflflights (https://github.com/steodose/nfl-flights; charter flight data pipeline)"

`%||%` <- function(x, y) if (is.null(x)) y else x

#' The project root. Defaults to the working directory, but can be pinned via
#' options(nflflights.root = ...) so tests and scripts resolve paths the same
#' way regardless of where they are invoked from.
root_dir <- function() getOption("nflflights.root", default = getwd())

#' Locate a project file, working both from a source checkout and an installed
#' package.
proj_path <- function(...) {
  parts <- file.path(...)
  installed <- tryCatch(system.file(parts, package = "nflflights"), error = function(e) "")
  if (nzchar(installed) && file.exists(installed)) return(installed)
  file.path(root_dir(), parts)
}

#' Raw GET with retry and throttling. Returns the response body as a string.
fetch_url <- function(url, throttle = 1) {
  req <- httr2::request(url)
  req <- httr2::req_user_agent(req, USER_AGENT)
  req <- httr2::req_timeout(req, 30)
  req <- httr2::req_retry(req, max_tries = 3, backoff = function(i) 2^i)
  req <- httr2::req_throttle(req, rate = 1 / throttle)
  resp <- httr2::req_perform(req)
  httr2::resp_body_string(resp)
}

cache_path <- function(slug) file.path(root_dir(), "cache", "html", paste0(slug, ".html"))

#' Fetch a post, using the on-disk cache unless `refresh` is TRUE.
fetch_post <- function(url, slug = sub("^.*/", "", url), refresh = FALSE) {
  path <- cache_path(slug)
  if (!refresh && file.exists(path)) {
    return(readChar(path, file.size(path), useBytes = TRUE))
  }
  html <- fetch_url(url)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeChar(html, path, eos = NULL, useBytes = TRUE)
  html
}

#' Read a reference lookup table, returning NULL when absent.
read_reference <- function(file) {
  path <- proj_path("inst", "reference", file)
  if (!file.exists(path)) return(NULL)
  readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
}
