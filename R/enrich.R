# Enrichment: airport coordinates and great-circle distance.

EARTH_RADIUS_MI <- 3958.8

#' Great-circle distance in statute miles.
#'
#' Implemented directly rather than via geosphere so the pipeline has one
#' fewer dependency to install in CI.
haversine_mi <- function(lat1, lon1, lat2, lon2) {
  to_rad <- function(d) d * pi / 180
  dlat <- to_rad(lat2 - lat1)
  dlon <- to_rad(lon2 - lon1)
  a <- sin(dlat / 2)^2 + cos(to_rad(lat1)) * cos(to_rad(lat2)) * sin(dlon / 2)^2
  2 * EARTH_RADIUS_MI * asin(pmin(1, sqrt(a)))
}

#' Apply documented corrections for airport codes the source got wrong.
apply_airport_fixes <- function(df) {
  fixes <- read_reference("airport_fixes.csv")
  if (is.null(fixes)) return(df)
  for (col in c("dep_airport", "arr_airport")) {
    hit <- match(df[[col]], fixes$from)
    df[[col]] <- ifelse(is.na(hit), df[[col]], fixes$to[hit])
  }
  df
}

#' Attach coordinates for both endpoints and the distance between them.
enrich_legs <- function(df) {
  df <- apply_airport_fixes(df)
  airports <- read_reference("airports.csv")
  if (is.null(airports)) stop("inst/reference/airports.csv is missing", call. = FALSE)

  dep <- match(df$dep_airport, airports$iata)
  arr <- match(df$arr_airport, airports$iata)

  df$dep_lat <- airports$lat[dep]
  df$dep_lon <- airports$lon[dep]
  df$arr_lat <- airports$lat[arr]
  df$arr_lon <- airports$lon[arr]
  df$distance_mi <- round(haversine_mi(df$dep_lat, df$dep_lon, df$arr_lat, df$arr_lon))
  df
}
