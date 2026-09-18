test_that("completed seasons cover weeks 1-18; the newest may be in progress", {
  idx <- readr::read_csv(proj_file("inst", "extdata", "post_index.csv"), show_col_types = FALSE)
  expect_true(all(idx$game_type == "REG"))
  expect_true(all(idx$season >= 2023))

  seasons <- sort(unique(idx$season))
  for (s in setdiff(seasons, max(seasons))) {
    expect_setequal(idx$week[idx$season == s], 1:18)
  }
  latest <- idx$week[idx$season == max(seasons)]
  expect_setequal(latest, seq_along(latest))
})

test_that("validation rejects an unmapped team", {
  flights <- tibble::tibble(
    post_slug = "p", away_team_raw = "Springfield Atoms", away_team_abbr = NA_character_,
    home_team_raw = "B", home_team_abbr = "KC", dep_airport = "A", dep_lat = 1,
    arr_airport = "B", arr_lat = 1, flight_number = "DAL1", airline = "Delta Airlines",
    flight_date = Sys.Date(), distance_mi = 100, parse_ok = TRUE
  )
  expect_error(
    validate_dataset(flights, tibble::tibble(slug = "p")),
    "Unmapped away team names"
  )
})

test_that("validation rejects a shrinking dataset", {
  flights <- tibble::tibble(
    post_slug = "p", away_team_raw = "A", away_team_abbr = "KC",
    home_team_raw = "B", home_team_abbr = "SF", dep_airport = "A", dep_lat = 1,
    arr_airport = "B", arr_lat = 1, flight_number = "DAL1", airline = "Delta Airlines",
    flight_date = Sys.Date(), distance_mi = 100, parse_ok = TRUE
  )
  expect_error(
    validate_dataset(flights, tibble::tibble(slug = "p"), previous = flights[c(1, 1), ]),
    "Row count dropped"
  )
})

test_that("Row ID leads the file and is a unique sequential key", {
  # Tableau uses this as the level of detail for map-layer flight paths, so
  # its position and uniqueness are a contract, not an implementation detail.
  f <- readr::read_csv(proj_file("inst", "extdata", "charter_flights.csv"),
                       col_types = readr::cols(.default = readr::col_character()))
  expect_equal(names(f)[1], "Row ID")
  expect_equal(as.integer(f[["Row ID"]]), seq_len(nrow(f)))
})
