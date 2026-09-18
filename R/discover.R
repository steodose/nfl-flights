# Discovery: enumerate charter-flight posts from the blog's tag archive.
#
# The blog's slugs are not generable from (season, week): weeks appear both as
# digits ("week-12") and as number words ("week-five"), and the trailing segment
# varies ("-team-charter-flights" vs "-team-charter-flight-schedule"). The tag
# archive is therefore the source of truth for which posts exist.

TAG_ARCHIVE_URL <- "https://blog.jettip.net/tag/nfl-charter-flights"
BLOG_BASE_URL <- "https://blog.jettip.net"

# Number words used by the blog for weeks 1-18.
WEEK_WORDS <- c(
  one = 1L, two = 2L, three = 3L, four = 4L, five = 5L, six = 6L,
  seven = 7L, eight = 8L, nine = 9L, ten = 10L, eleven = 11L, twelve = 12L,
  thirteen = 13L, fourteen = 14L, fifteen = 15L, sixteen = 16L,
  seventeen = 17L, eighteen = 18L
)

# Playoff round slugs -> the week numbers nflverse uses for them.
PLAYOFF_WEEKS <- c(
  "super-wild-card" = 19L, "wild-card" = 19L, "wildcard" = 19L,
  "divisional" = 20L,
  "conference-championship" = 21L, "conference-champ" = 21L,
  "super-bowl" = 22L
)

#' Crawl every page of the tag archive and return the distinct post URLs.
tag_archive_urls <- function(max_pages = 50L) {
  seen <- character(0)
  for (page in seq_len(max_pages)) {
    url <- if (page == 1L) TAG_ARCHIVE_URL else paste0(TAG_ARCHIVE_URL, "/p", page)
    html <- tryCatch(fetch_url(url), error = function(e) NULL)
    if (is.null(html)) break

    hrefs <- rvest::html_attr(rvest::html_elements(xml2::read_html(html), "a"), "href")
    hrefs <- hrefs[!is.na(hrefs)]
    hrefs <- ifelse(startsWith(hrefs, "/"), paste0(BLOG_BASE_URL, hrefs), hrefs)
    posts <- unique(hrefs[grepl("^https://blog\\.jettip\\.net/nfl-\\d{4}-.*charter", hrefs)])

    new <- setdiff(posts, seen)
    if (length(new) == 0L) break
    seen <- c(seen, new)
  }
  seen
}

#' Classify a post slug into season / game_type / week.
#'
#' Preseason and playoff posts are classified rather than pattern-skipped so
#' that an unrecognised post type can never be silently misfiled as a regular
#' season week -- anything unclassifiable errors instead.
classify_slug <- function(slug, overrides = read_reference("post_overrides.csv")) {
  if (!is.null(overrides) && slug %in% overrides$slug) {
    row <- overrides[match(slug, overrides$slug), ]
    return(list(
      season = as.integer(row$season),
      game_type = as.character(row$game_type),
      week = as.integer(row$week)
    ))
  }

  season <- suppressWarnings(as.integer(stringr::str_match(slug, "nfl-(\\d{4})-")[, 2]))
  if (is.na(season)) stop("Cannot determine season from slug: ", slug, call. = FALSE)

  if (grepl("preseason", slug, fixed = TRUE)) {
    return(list(season = season, game_type = "PRE", week = NA_integer_))
  }

  for (nm in names(PLAYOFF_WEEKS)) {
    if (grepl(nm, slug, fixed = TRUE)) {
      return(list(season = season, game_type = "POST", week = unname(PLAYOFF_WEEKS[[nm]])))
    }
  }

  token <- stringr::str_match(slug, "week-([a-z0-9]+)")[, 2]
  if (is.na(token)) stop("Cannot determine week from slug: ", slug, call. = FALSE)

  week <- if (grepl("^\\d+$", token)) {
    as.integer(token)
  } else if (token %in% names(WEEK_WORDS)) {
    unname(WEEK_WORDS[[token]])
  } else {
    stop(
      "Unrecognised week token '", token, "' in slug: ", slug,
      ". Add an entry to inst/reference/post_overrides.csv.",
      call. = FALSE
    )
  }

  list(season = season, game_type = "REG", week = week)
}

#' Discover posts from `min_season` onward, for the requested post types.
#'
#' Deliberately open-ended rather than a fixed season range: a new season then
#' starts flowing through the moment JetTip publishes its first post, with no
#' annual edit and no calendar arithmetic.
discover_posts <- function(min_season = FIRST_SEASON,
                           include_preseason = FALSE,
                           include_playoffs = FALSE,
                           urls = NULL) {
  urls <- urls %||% tag_archive_urls()
  slugs <- sub("^.*/", "", urls)

  classified <- lapply(slugs, classify_slug)
  idx <- tibble::tibble(
    slug = slugs,
    url = urls,
    season = vapply(classified, `[[`, integer(1), "season"),
    game_type = vapply(classified, `[[`, character(1), "game_type"),
    week = vapply(classified, `[[`, integer(1), "week")
  )

  keep <- c("REG", if (include_preseason) "PRE", if (include_playoffs) "POST")
  idx <- idx[idx$season >= min_season & idx$game_type %in% keep, ]
  idx <- idx[order(idx$season, idx$week), ]

  if (identical(keep, "REG")) assert_regular_season_coverage(idx)
  idx
}

#' Check week coverage, allowing the newest season to still be under way.
#'
#' Completed seasons must hold weeks 1-18 exactly once. The most recent season
#' is normally in progress, so it only has to be contiguous from week 1 -- that
#' still catches a week the scraper missed, without failing every week of a
#' live season.
assert_regular_season_coverage <- function(idx) {
  seasons <- sort(unique(idx$season))
  if (length(seasons) == 0L) stop("No regular season posts discovered", call. = FALSE)
  latest <- max(seasons)

  for (s in seasons) {
    weeks <- sort(idx$week[idx$season == s])
    dupes <- weeks[duplicated(weeks)]
    if (length(dupes) > 0L) {
      stop("Season ", s, " has duplicate week(s): ", paste(unique(dupes), collapse = ", "), call. = FALSE)
    }

    expected <- if (s == latest) seq_len(length(weeks)) else 1:18
    missing <- setdiff(expected, weeks)
    if (length(missing) > 0L) {
      stop("Season ", s, " is missing week(s): ", paste(missing, collapse = ", "), call. = FALSE)
    }
    if (any(weeks > 18L)) {
      stop("Season ", s, " has week(s) above 18: ",
           paste(weeks[weeks > 18L], collapse = ", "), call. = FALSE)
    }
  }
  invisible(TRUE)
}
