test_that("carrier prefixes are split from both ICAO and IATA designators", {
  expect_equal(split_flight_number("DAL8862"), "DAL")  # ICAO, 2023/2025
  expect_equal(split_flight_number("DL8863"), "DL")    # IATA, 2024
  expect_equal(split_flight_number("5Y8088"), "5Y")    # digit-led IATA
  expect_equal(split_flight_number("RZN"), "RZN")      # no numeric part
  expect_equal(split_flight_number("VS6??"), "VS")     # partially known
  expect_equal(split_flight_number("VIR6**"), "VIR")
})

test_that("both designator formats resolve to the same airline", {
  df <- attach_airline(tibble::tibble(flight_number = c("DAL8862", "DL8863", "GTI8086", "5Y8088")))
  expect_equal(df$airline, c("Delta Airlines", "Delta Airlines", "Atlas Air", "Atlas Air"))
})

test_that("an airline written in words still resolves", {
  df <- attach_airline(tibble::tibble(flight_number = "Virgin Atlantic"))
  expect_equal(df$airline, "Virgin Atlantic Airlines")
})

test_that("heading dates are anchored to the publication year", {
  # A post published in late December carries January headings for the
  # following calendar year.
  pub <- as.Date("2025-12-23")
  expect_equal(resolve_date("Wednesday, December 24", pub), as.Date("2025-12-24"))
  expect_equal(resolve_date("Friday, January 2", pub), as.Date("2026-01-02"))
})

test_that("the weekday token is ignored, since the source contains typos", {
  expect_equal(resolve_date("Thurday, October 7", as.Date("2021-10-05")), as.Date("2021-10-07"))
})

test_that("the earliest date in a matchup is the outbound trip", {
  df <- tibble::tibble(
    post_slug = "p", away_team_raw = "A", home_team_raw = "B",
    flight_date = as.Date(c("2025-12-26", "2025-12-27"))
  )
  expect_equal(attach_direction(df)$direction, c("outbound", "return"))
})
