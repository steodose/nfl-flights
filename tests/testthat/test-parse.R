test_that("every leg in each fixture parses", {
  for (slug in c("nfl-2023-week-one-team-charter-flights",
                 "nfl-2024-week-14-team-charter-flight-schedule",
                 "nfl-2025-week-17-team-charter-flights")) {
    d <- fixture(slug)
    expect_gt(nrow(d), 20)
    expect_true(all(d$parse_ok), info = slug)
  }
})

test_that("site navigation is not mistaken for flight rows", {
  d <- fixture("nfl-2025-week-17-team-charter-flights")
  expect_false(any(c("Airports", "Diversions", "Login") %in% d$away_team_raw))
})

test_that("a trailing parenthetical is kept out of the aircraft type", {
  # The original regex returned NA here, which is the source of the "Null"
  # aircraft values in the dashboard.
  d <- fixture("nfl-2025-week-17-team-charter-flights")
  ne <- dplyr::filter(d, away_team_raw == "New England Patriots")
  expect_equal(unique(ne$aircraft_type), "B763")
  expect_equal(unique(ne$aircraft_note), "N36NE")
})

test_that("both legs of a split-aircraft item are captured", {
  d <- fixture("nfl-2024-week-14-team-charter-flight-schedule")
  gb <- dplyr::filter(d, away_team_raw == "Green Bay Packers")
  expect_equal(nrow(gb), 4)
  expect_setequal(unique(gb$flight_number), c("DL8863", "DL8959"))
  expect_setequal(unique(gb$aircraft_type), c("B752", "B739"))
})

test_that("a venue parenthetical is split off the home team", {
  mu <- parse_matchup("Kansas City Chiefs @ Los Angeles Chargers (@ Sao Paolo, Brazil")
  expect_equal(mu$home_team_raw, "Los Angeles Chargers")
  expect_equal(mu$venue_note, "Sao Paolo, Brazil")
})

test_that("editorial annotations are split off the away team", {
  mu <- parse_matchup("New York Giants (Week 3) @ Dallas Cowboys")
  expect_equal(mu$away_team_raw, "New York Giants")
  expect_equal(mu$matchup_note, "Week 3")
})

test_that("alternative airports keep the first and record the rest", {
  a <- split_airport("LHR / STN")
  expect_equal(a$airport, "LHR")
  expect_equal(a$alt, "STN")
})

test_that("airport codes are upper-cased, tolerating source typos", {
  expect_equal(split_airport("BWi")$airport, "BWI")
})

test_that("ground travel is skipped rather than recorded as a flight", {
  expect_match("Bussing", GROUND_PATTERN, ignore.case = TRUE)
})
