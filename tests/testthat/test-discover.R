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

test_that("a completed season missing a week is rejected", {
  idx <- tibble::tibble(season = c(rep(2025L, 17), 2026L), week = c(1:17, 1L))
  expect_error(assert_regular_season_coverage(idx), "missing week")
})

test_that("the newest season may still be in progress", {
  idx <- tibble::tibble(season = c(rep(2025L, 18), rep(2026L, 2)), week = c(1:18, 1:2))
  expect_true(assert_regular_season_coverage(idx))
})

test_that("a gap in the in-progress season is still rejected", {
  idx <- tibble::tibble(season = c(rep(2025L, 18), 2026L, 2026L), week = c(1:18, 1L, 3L))
  expect_error(assert_regular_season_coverage(idx), "missing week")
})

test_that("a duplicated week is rejected", {
  idx <- tibble::tibble(season = rep(2026L, 3), week = c(1L, 2L, 2L))
  expect_error(assert_regular_season_coverage(idx), "duplicate week")
})
