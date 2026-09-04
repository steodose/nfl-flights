# Normalization: raw parsed legs -> canonical teams, dates, airlines, direction.

MONTHS <- c("January", "February", "March", "April", "May", "June", "July",
            "August", "September", "October", "November", "December")

#' Resolve an <h2> heading ("Wednesday, December 24") to a real date.
#'
#' Headings carry no year, so the post's publication date anchors it: of the
#' candidate years, take the one landing closest to publication. This handles
#' the December/January rollover without special-casing it, and ignores the
#' weekday token entirely because the source contains typos ("Thurday").
resolve_date <- function(date_text, published_date) {
  out <- as.Date(rep(NA, length(date_text)))
  m <- stringr::str_match(date_text, "([A-Za-z]+)\\s+(\\d{1,2})")

  for (i in seq_along(date_text)) {
    month <- m[i, 2]; day <- m[i, 3]
    if (is.na(month) || !(month %in% MONTHS) || is.na(published_date[i])) next
    pub_year <- as.integer(format(published_date[i], "%Y"))
    cands <- as.Date(paste(month, day, c(pub_year - 1L, pub_year, pub_year + 1L)), format = "%B %d %Y")
    cands <- cands[!is.na(cands)]
    if (length(cands) == 0L) next
    out[i] <- cands[which.min(abs(as.numeric(cands - published_date[i])))]
  }
  out
}

#' team_name / short-name -> team_abbr, using nflverse plus a small alias table.
team_lookup <- function() {
  teams <- nflreadr::load_teams(current = FALSE)
  teams <- teams[!duplicated(teams$team_abbr), ]

  base <- tibble::tibble(
    name = teams$team_name,
    team_abbr = teams$team_abbr
  )
  aliases <- read_reference("team_aliases.csv")
  if (!is.null(aliases)) {
    base <- dplyr::bind_rows(base, tibble::tibble(name = aliases$name, team_abbr = aliases$team_abbr))
  }
  base[!duplicated(base$name), ]
}

#' Attach abbr / conference / division for one side of the matchup.
attach_team_meta <- function(df, raw_col, prefix) {
  teams <- nflreadr::load_teams(current = FALSE)
  teams <- teams[!duplicated(teams$team_abbr), ]
  lookup <- team_lookup()

  abbr <- lookup$team_abbr[match(df[[raw_col]], lookup$name)]
  idx <- match(abbr, teams$team_abbr)

  df[[paste0(prefix, "_team")]] <- teams$team_name[idx]
  df[[paste0(prefix, "_team_abbr")]] <- abbr
  df[[paste0(prefix, "_conf")]] <- teams$team_conf[idx]
  df[[paste0(prefix, "_division")]] <- teams$team_division[idx]
  df
}

#' Split a flight number into its carrier prefix.
#'
#' Prefixes are letters-only (ICAO "DAL", IATA "DL") or digit+letter ("5Y"),
#' and never mix with the numeric part, so the alternation is unambiguous.
#' The numeric part is optional and may contain "?", because the source
#' publishes partially-known designators ("VS6??", "VIR6**", bare "RZN").
split_flight_number <- function(flight_number) {
  m <- stringr::str_match(toupper(flight_number), "^([A-Z]{2,3}|[0-9][A-Z])[0-9?*]*$")
  m[, 2]
}

#' Map carrier prefixes to airline names.
#'
#' 2023 and 2025 use ICAO 3-char codes (DAL8862, GTI8086); 2024 uses IATA
#' 2-char (DL8863, 5Y8088). Keying on both retires the hand-maintained
#' 494-row Flight_Number -> Airline tab in the Google Sheet.
attach_airline <- function(df) {
  carriers <- read_reference("carriers.csv")
  df$carrier_code <- split_flight_number(df$flight_number)

  if (is.null(carriers)) {
    df$airline <- NA_character_
    return(df)
  }

  airline <- carriers$airline[match(df$carrier_code, carriers$code)]
  # A handful of segments name the airline in words instead of a designator
  # ("Virgin Atlantic"); match those against the same table.
  fallback <- carriers$airline[match(toupper(df$flight_number), toupper(carriers$code))]
  df$airline <- ifelse(is.na(airline), fallback, airline)
  df
}

#' Label each leg outbound or return.
#'
#' Heuristic: within a matchup in a single post, the legs on the earliest date
#' are the trip out, everything later is the trip home. The source never states
#' direction explicitly.
attach_direction <- function(df) {
  df <- dplyr::group_by(df, .data$post_slug, .data$away_team_raw, .data$home_team_raw)
  df <- dplyr::mutate(df, direction = ifelse(
    .data$flight_date == suppressWarnings(min(.data$flight_date, na.rm = TRUE)),
    "outbound", "return"
  ))
  dplyr::ungroup(df)
}

normalize_legs <- function(df) {
  df$flight_date <- resolve_date(df$date_text, df$published_date)
  df <- attach_team_meta(df, "away_team_raw", "away")
  df <- attach_team_meta(df, "home_team_raw", "home")
  df <- attach_airline(df)
  df <- attach_direction(df)
  df
}
