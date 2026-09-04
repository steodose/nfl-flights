test_that("haversine reproduces a known great-circle distance", {
  # LAX -> GRU, the 2025 week 1 Brazil game.
  d <- haversine_mi(33.94254, -118.40807, -23.43556, -46.47306)
  expect_equal(round(d), 6163)
})

test_that("distance is zero for identical endpoints", {
  expect_equal(haversine_mi(40, -80, 40, -80), 0)
})

test_that("documented airport corrections are applied", {
  df <- apply_airport_fixes(tibble::tibble(dep_airport = "LX", arr_airport = "LONDON"))
  expect_equal(df$dep_airport, "LAX")
  expect_equal(df$arr_airport, "LHR")
})

test_that("every airport in the published dataset has coordinates", {
  f <- readr::read_csv(proj_file("inst", "extdata", "charter_flights.csv"),
                       col_types = readr::cols(.default = readr::col_character()))
  expect_equal(sum(is.na(f$dep_lat)), 0)
  expect_equal(sum(is.na(f$arr_lat)), 0)
})
