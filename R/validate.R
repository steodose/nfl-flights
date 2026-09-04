# Validation: assert the dataset is sound before anything is written.
#
# These run inside the GitHub Action, so a blog format change fails the build
# rather than quietly publishing corrupt data to the dashboard.

fail_if <- function(bad, what) {
  if (length(bad) == 0L) return(invisible(TRUE))
  stop(what, ": ", paste(utils::head(unique(bad), 20), collapse = ", "), call. = FALSE)
}

validate_dataset <- function(flights, idx, previous = NULL, max_parse_fail_rate = 0.01) {
  # Every discovered post must contribute at least one leg.
  empty <- setdiff(idx$slug, unique(flights$post_slug))
  fail_if(empty, "Posts that produced no flight legs")

  fail_if(unique(flights$away_team_raw[is.na(flights$away_team_abbr)]), "Unmapped away team names")
  fail_if(unique(flights$home_team_raw[is.na(flights$home_team_abbr)]), "Unmapped home team names")
  fail_if(unique(flights$dep_airport[is.na(flights$dep_lat)]), "Unknown departure airport codes")
  fail_if(unique(flights$arr_airport[is.na(flights$arr_lat)]), "Unknown arrival airport codes")
  fail_if(
    unique(flights$flight_number[is.na(flights$airline) & !is.na(flights$flight_number)]),
    "Unmapped carrier prefixes"
  )
  fail_if(unique(flights$post_slug[is.na(flights$flight_date)]), "Posts with unresolvable flight dates")

  bad_dist <- flights$distance_mi[!is.na(flights$distance_mi) & (flights$distance_mi <= 0 | flights$distance_mi >= 8000)]
  fail_if(bad_dist, "Implausible distances (mi)")

  rate <- mean(!flights$parse_ok)
  if (rate > max_parse_fail_rate) {
    stop("Parse failure rate ", round(rate * 100, 2), "% exceeds ",
         round(max_parse_fail_rate * 100, 2), "%", call. = FALSE)
  }

  # A shrinking dataset means the scrape lost something.
  if (!is.null(previous) && nrow(flights) < nrow(previous)) {
    stop("Row count dropped from ", nrow(previous), " to ", nrow(flights), call. = FALSE)
  }

  invisible(TRUE)
}
