# Parsing: post HTML -> one row per flight leg.
#
# Post bodies look like:
#   <h2>Saturday, December 27</h2>
#   <li><strong>Baltimore Ravens @ Green Bay Packers</strong><br />
#       GRB (1130pm CT) - BWI (230am ET), GTI8055, B744</li>
#
# Two structural details the original script missed:
#   * the <h2> date heading, which is the only source of a flight date and the
#     basis for telling an outbound leg from a return leg; and
#   * multiple <br />-separated legs inside a single <li>, used when a team
#     splits across two aircraft.

CONTENT_SELECTOR <- "div.blog-main"

# DEP (time) - ARR (time), FLIGHT, AIRCRAFT
#
# Airport slots accept a "/"-separated list, because JetTip writes the
# candidates when the destination field is not yet settled ("LHR / STN"), and
# accept mixed case to tolerate source typos ("BWi"). Codes longer than three
# letters occur where a city is written instead ("London"); those resolve via
# inst/reference/airport_fixes.csv.
AIRPORT_TOKEN <- "[A-Za-z]{2,}(?:\\s*/\\s*[A-Za-z]{2,})*"

LEG_PATTERN <- paste0(
  "^(", AIRPORT_TOKEN, ")\\s*(?:\\(([^)]*)\\))?",   # departure airport(s) + optional time
  "\\s*-\\s*",
  "(", AIRPORT_TOKEN, ")\\s*(?:\\(([^)]*)\\))?",    # arrival airport(s) + optional time
  "\\s*,\\s*(.+)$"                                # flight number + aircraft tail
)

# Segments naming ground transport rather than a flight.
GROUND_PATTERN <- "^(bussing|busing|bus|driving|train)\\b"

# Matchups that are not games: in-season practice and training-camp trips.
NON_GAME_PATTERN <- "practice|training camp"

#' Extract the publication date, which anchors the year for the <h2> headings
#' (headings themselves carry only "Wednesday, December 24").
parse_published_date <- function(doc) {
  header <- rvest::html_text2(rvest::html_elements(doc, ".page-header-copy"))
  if (length(header) == 0L) return(NA)
  m <- stringr::str_match(paste(header, collapse = " "), "Posted on [A-Za-z]+,\\s*([A-Za-z]+)\\s+(\\d{1,2}),\\s*(\\d{4})")
  if (is.na(m[1, 1])) return(NA)
  as.Date(paste(m[1, 2], m[1, 3], m[1, 4]), format = "%B %d %Y")
}

#' Split an <li>'s inner HTML into a matchup string and its leg segments.
split_list_item <- function(node) {
  inner <- as.character(rvest::html_children(node))
  raw <- paste(as.character(node), collapse = "")
  raw <- sub("^<li[^>]*>", "", raw)
  raw <- sub("</li>\\s*$", "", raw)

  strong <- rvest::html_element(node, "strong")
  matchup <- if (!is.na(strong)) rvest::html_text2(strong) else NA_character_

  # Drop the leading <strong> block; whatever follows is the leg text.
  rest <- sub("<strong[^>]*>.*?</strong>", "", raw)
  segments <- strsplit(rest, "<br\\s*/?>", perl = TRUE)[[1]]
  segments <- vapply(segments, function(s) {
    txt <- rvest::html_text2(xml2::read_html(paste0("<div>", s, "</div>")))
    stringr::str_squish(txt)
  }, character(1), USE.NAMES = FALSE)
  segments <- segments[nzchar(segments)]

  list(matchup = stringr::str_squish(matchup %||% NA_character_), segments = segments)
}

#' Split "Away @ Home (venue note)" into its parts.
#'
#' The trailing parenthetical is a venue, not part of the team name -- failing
#' to split it is what produces the "Los Angeles Chargers (@ Sao Paolo, Brazil"
#' rows in the current Google Sheet.
parse_matchup <- function(matchup) {
  parts <- stringr::str_split_fixed(matchup, "\\s+@\\s+", 2)
  away_raw <- stringr::str_squish(parts[, 1])
  home_raw <- stringr::str_squish(parts[, 2])

  # The closing parenthesis is optional because the source sometimes omits it
  # ("Los Angeles Chargers (@ Sao Paolo, Brazil").
  TRAILING_PAREN <- "\\(([^)]*)\\)?\\s*$"
  strip_paren <- function(x) stringr::str_squish(sub("\\s*\\([^)]*\\)?\\s*$", "", x))

  venue <- stringr::str_match(home_raw, TRAILING_PAREN)[, 2]
  # "(@ Sao Paolo, Brazil)" / "(in London)" -> keep just the place.
  venue <- stringr::str_squish(sub("^(@|in)\\s+", "", venue))

  # The away side carries editorial annotations rather than venues
  # ("Green Bay Packers (1)", "New York Giants (Week 3)").
  away_note <- stringr::str_squish(stringr::str_match(away_raw, TRAILING_PAREN)[, 2])

  list(
    away_team_raw = strip_paren(away_raw),
    home_team_raw = strip_paren(home_raw),
    venue_note = if (length(venue) && !is.na(venue) && nzchar(venue)) venue else NA_character_,
    matchup_note = if (length(away_note) && !is.na(away_note) && nzchar(away_note)) away_note else NA_character_
  )
}

#' Split the "FLIGHT, AIRCRAFT (note)" tail of a leg.
#'
#' The trailing parenthetical holds a tail number ("N192DN") or a remark
#' ("both aircraft"). The original regex, `(?<=, )\\w+$`, returned NA whenever
#' one was present -- the source of the Null aircraft values in the dashboard.
parse_leg_tail <- function(tail) {
  parts <- stringr::str_split_fixed(tail, "\\s*,\\s*", 2)
  flight_raw <- stringr::str_squish(parts[, 1])
  aircraft_raw <- stringr::str_squish(parts[, 2])

  # Both fields may carry a trailing parenthetical: a tail number or remark on
  # the aircraft ("N192DN", "both aircraft"), or an uncertainty marker on the
  # flight number ("DAL8866 (?)").
  drop_paren <- function(x) stringr::str_squish(sub("\\s*\\([^)]*\\)\\s*$", "", x))
  take_paren <- function(x) stringr::str_match(x, "\\(([^)]*)\\)\\s*$")[, 2]

  list(
    flight_number = na_if_blank(drop_paren(flight_raw)),
    flight_note = na_if_blank(take_paren(flight_raw)),
    aircraft_type = na_if_blank(toupper(drop_paren(aircraft_raw))),
    aircraft_note = na_if_blank(take_paren(aircraft_raw))
  )
}

#' Take the first of a "/"-separated airport list; the rest are alternatives
#' the source had not yet resolved.
split_airport <- function(token) {
  if (is.na(token)) return(list(airport = NA_character_, alt = NA_character_))
  parts <- stringr::str_squish(strsplit(token, "/", fixed = TRUE)[[1]])
  parts <- toupper(parts[nzchar(parts)])
  list(
    airport = parts[1],
    alt = if (length(parts) > 1L) paste(parts[-1], collapse = "/") else NA_character_
  )
}

na_if_blank <- function(x) {
  # Squish first: captured groups keep the source's stray whitespace, and a
  # trailing space survives the CSV write but is stripped on read, which makes
  # an otherwise-unchanged rebuild look like a data change.
  x <- stringr::str_squish(as.character(x))
  x[is.na(x) | !nzchar(x) | x == "TBD"] <- NA_character_
  x
}

#' Parse one post's HTML into a tibble of legs.
parse_post <- function(html, slug = NA_character_) {
  doc <- xml2::read_html(html)
  content <- rvest::html_element(doc, CONTENT_SELECTOR)
  if (is.na(content)) stop("No content container (", CONTENT_SELECTOR, ") in post: ", slug, call. = FALSE)

  published <- parse_published_date(doc)

  # Walk h2 and li in document order, restricted to the article body so the
  # site navigation ("Airports", "Diversions", "Login") is never picked up --
  # a real weakness of the original `html_nodes("h2, li")` selector.
  nodes <- rvest::html_elements(content, xpath = ".//h2 | .//li")

  rows <- list()
  current_date_text <- NA_character_

  for (node in nodes) {
    tag <- xml2::xml_name(node)
    if (identical(tag, "h2")) {
      current_date_text <- stringr::str_squish(rvest::html_text2(node))
      next
    }

    item <- split_list_item(node)
    if (is.na(item$matchup) || !nzchar(item$matchup) || length(item$segments) == 0L) next
    if (!grepl("@", item$matchup)) next
    # Practice / training-camp trips are travel, but not games.
    if (grepl(NON_GAME_PATTERN, item$matchup, ignore.case = TRUE)) next

    mu <- parse_matchup(item$matchup)

    for (i in seq_along(item$segments)) {
      seg <- item$segments[[i]]

      # "Bussing" and friends: the team travelled overland, so there is no
      # flight to record. Skipped deliberately rather than logged as a failure.
      if (grepl(GROUND_PATTERN, seg, ignore.case = TRUE)) next

      m <- stringr::str_match(seg, LEG_PATTERN)
      ok <- !is.na(m[1, 1])
      dep <- split_airport(if (ok) m[1, 2] else NA_character_)
      arr <- split_airport(if (ok) m[1, 4] else NA_character_)
      tail <- if (ok) parse_leg_tail(m[1, 6]) else list(
        flight_number = NA_character_, flight_note = NA_character_,
        aircraft_type = NA_character_, aircraft_note = NA_character_
      )

      alts <- c(
        if (!is.na(dep$alt)) paste0("dep alt: ", dep$alt),
        if (!is.na(arr$alt)) paste0("arr alt: ", arr$alt)
      )

      rows[[length(rows) + 1L]] <- tibble::tibble(
        post_slug = slug,
        published_date = published,
        date_text = current_date_text,
        away_team_raw = mu$away_team_raw,
        home_team_raw = mu$home_team_raw,
        venue_note = mu$venue_note,
        matchup_note = mu$matchup_note,
        leg_seq = i,
        dep_airport = dep$airport,
        dep_time_raw = if (ok) na_if_blank(m[1, 3]) else NA_character_,
        arr_airport = arr$airport,
        arr_time_raw = if (ok) na_if_blank(m[1, 5]) else NA_character_,
        flight_number = tail$flight_number,
        flight_note = tail$flight_note,
        aircraft_type = tail$aircraft_type,
        aircraft_note = tail$aircraft_note,
        airport_note = if (length(alts)) paste(alts, collapse = "; ") else NA_character_,
        segment_raw = seg,
        parse_ok = ok
      )
    }
  }

  if (length(rows) == 0L) {
    return(tibble::tibble())
  }
  dplyr::bind_rows(rows)
}
