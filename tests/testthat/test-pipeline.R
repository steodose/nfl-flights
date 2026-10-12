test_that("scraped_at carries forward per row, not all-or-nothing", {
  prev <- tibble::tibble(
    `Row ID` = c("1", "2"), flight_number = c("DAL1", "DAL2"),
    aircraft_note = c("", "N1"), scraped_at = c("2026-09-01", "2026-09-08")
  )
  today <- as.Date("2026-10-11")
  flights <- tibble::tibble(
    `Row ID` = 1:4,
    flight_number = c("AAL9", "DAL1", "DAL2", "GTI3"),
    aircraft_note = c(NA, NA, "N2", NA),
    scraped_at = today
  )
  out <- carry_forward_scraped_at(flights, prev)

  # DAL1 shifted from Row ID 1 to 2 and its NA round-trips as "": unchanged.
  expect_equal(out$scraped_at[2], as.Date("2026-09-01"))
  # DAL2's note changed, and the new legs are new: all stamped today.
  expect_equal(out$scraped_at[c(1, 3, 4)], rep(today, 3))
  expect_s3_class(out$scraped_at, "Date")
})

test_that("scraped_at is left alone when there is no previous dataset", {
  flights <- tibble::tibble(`Row ID` = 1L, flight_number = "DAL1",
                            scraped_at = as.Date("2026-10-11"))
  expect_identical(carry_forward_scraped_at(flights, NULL), flights)
})
