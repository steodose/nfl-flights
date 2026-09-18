# Pipeline: wire discovery -> fetch -> parse -> normalize -> enrich -> validate.

# Earliest season in scope. There is no upper bound: new seasons are picked up
# automatically as JetTip publishes them.
FIRST_SEASON <- 2023

# "Row ID" leads the file: Tableau uses it as the level of detail when drawing
# each flight path as its own map layer line. Assigned after the final sort, so
# it is stable for a given dataset.
OUTPUT_COLUMNS <- c(
  "Row ID",
  "season", "week", "post_slug", "post_url", "flight_date", "direction", "leg_seq",
  "away_team", "away_team_abbr", "away_conf", "away_division",
  "home_team", "home_team_abbr", "home_conf", "home_division",
  "venue_note", "matchup_note",
  "dep_airport", "dep_time_raw", "dep_lat", "dep_lon",
  "arr_airport", "arr_time_raw", "arr_lat", "arr_lon",
  "flight_number", "flight_note", "carrier_code", "airline",
  "aircraft_type", "aircraft_note", "airport_note",
  "distance_mi", "scraped_at"
)

extdata_path <- function(file) file.path(root_dir(), "inst", "extdata", file)

#' Parse every post in the index into raw legs.
collect_legs <- function(idx, refresh = FALSE) {
  purrr::map2_dfr(idx$url, idx$slug, function(url, slug) {
    html <- fetch_post(url, slug, refresh = refresh)
    parse_post(html, slug)
  })
}

#' Run the whole pipeline and return both raw and enriched tables.
build_dataset <- function(min_season = FIRST_SEASON, refresh = FALSE, urls = NULL) {
  idx <- discover_posts(min_season, urls = urls)
  message("Discovered ", nrow(idx), " posts across seasons ",
          paste(range(idx$season), collapse = "-"))

  raw <- collect_legs(idx, refresh = refresh)
  message("Parsed ", nrow(raw), " legs (", sum(!raw$parse_ok), " unparsed)")

  flights <- normalize_legs(raw)
  flights <- enrich_legs(flights)

  flights <- dplyr::left_join(flights, idx[, c("slug", "url", "season", "week")],
                              by = c("post_slug" = "slug"))
  flights$post_url <- flights$url
  flights$scraped_at <- Sys.Date()

  validate_dataset(flights, idx, previous = read_existing())

  flights <- dplyr::arrange(flights, .data$season, .data$week, .data$flight_date,
                            .data$away_team, .data$leg_seq)
  flights[["Row ID"]] <- seq_len(nrow(flights))

  list(
    index = idx,
    raw = raw,
    flights = carry_forward_scraped_at(flights[, OUTPUT_COLUMNS])
  )
}

#' Keep `scraped_at` unchanged when nothing else changed.
#'
#' `scraped_at` is stamped with the run date, so without this every weekly run
#' would rewrite all 1,662 rows purely to advance a date -- defeating the
#' workflow's "commit only if changed" check and burying real data changes in
#' full-file diffs. The column therefore records when the data last *changed*,
#' not when the scraper last ran.
carry_forward_scraped_at <- function(flights) {
  prev <- read_existing()
  if (is.null(prev) || nrow(prev) != nrow(flights)) return(flights)

  cmp <- setdiff(names(flights), "scraped_at")
  if (!all(cmp %in% names(prev))) return(flights)

  # read_existing() returns all-character columns, so compare on that footing.
  # readr round-trips our NAs as "", so normalise both sides before comparing.
  as_text <- function(df) {
    out <- vapply(df[, cmp, drop = FALSE], as.character, character(nrow(df)))
    out[is.na(out)] <- ""
    out
  }
  if (identical(as_text(flights), as_text(prev))) {
    flights$scraped_at <- prev$scraped_at
  }
  flights
}

#' The previously committed dataset, used only for the row-count regression
#' check. Read as character throughout so sparse note columns cannot trip
#' readr's type guessing.
read_existing <- function() {
  path <- extdata_path("charter_flights.csv")
  if (!file.exists(path)) return(NULL)
  readr::read_csv(path, col_types = readr::cols(.default = readr::col_character()),
                  progress = FALSE)
}

write_dataset <- function(res) {
  dir.create(dirname(extdata_path("x")), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(res$flights, extdata_path("charter_flights.csv"), na = "")
  readr::write_csv(res$raw, extdata_path("charter_flights_raw.csv"), na = "")
  readr::write_csv(res$index, extdata_path("post_index.csv"), na = "")
  message("Wrote ", nrow(res$flights), " rows to ", extdata_path("charter_flights.csv"))
  invisible(res)
}
