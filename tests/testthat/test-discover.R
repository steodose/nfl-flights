test_that("week slugs parse as both digits and number words", {
  expect_equal(classify_slug("nfl-2024-week-12-team-charter-flight-schedule")$week, 12L)
  expect_equal(classify_slug("nfl-2023-week-five-team-charter-flights")$week, 5L)
  expect_equal(classify_slug("nfl-2025-week-eighteen-team-charter-flights")$week, 18L)
})

test_that("post types are classified rather than pattern-skipped", {
  expect_equal(classify_slug("nfl-2025-week-one-team-charter-flights")$game_type, "REG")
  expect_equal(classify_slug("nfl-2024-preseason-week-three-team-charter-flights")$game_type, "PRE")
  expect_equal(classify_slug("nfl-2023-wild-card-team-charter-flights")$game_type, "POST")
  expect_equal(classify_slug("nfl-2024-divisional-round-team-charter-flights")$week, 20L)
})

test_that("an unrecognisable slug errors instead of being misfiled", {
  expect_error(classify_slug("nfl-2024-week-oneish-team-charter-flights"), "Unrecognised week token")
})

test_that("a season missing a week is rejected", {
  idx <- tibble::tibble(season = 2025L, week = 1:17)
  expect_error(assert_complete_regular_season(idx, 2025), "missing week")
})
