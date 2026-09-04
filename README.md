# NFL Charter Flights

Scrapes NFL team charter flight schedules from [blog.jettip.net](https://blog.jettip.net/tag/nfl-charter-flights),
normalizes and enriches them, and publishes a single flat CSV that powers a
[Tableau Public dashboard](https://public.tableau.com/app/profile/stephan.teodosescu/viz/2025NFLCharterFlights/Dashboard).

**Scope:** regular season only (weeks 1–18), seasons 2023–2025. 54 posts, ~1,660 flight legs.

## Output

`inst/extdata/charter_flights.csv` is the dashboard's data source — one row per
flight leg, fully enriched (teams, conference, airline, coordinates, distance).
Two companion files sit alongside it:

| File | Purpose |
| --- | --- |
| `charter_flights.csv` | Enriched dataset. The Tableau source. |
| `charter_flights_raw.csv` | Pre-normalization legs, for tracing a row back to the blog text. |
| `post_index.csv` | Every discovered post with its season, week and URL. |

### Columns

`season`, `week`, `post_slug`, `post_url`, `flight_date`, `direction`, `leg_seq`,
`away_team`, `away_team_abbr`, `away_conf`, `away_division`, `home_team`,
`home_team_abbr`, `home_conf`, `home_division`, `venue_note`, `matchup_note`,
`dep_airport`, `dep_time_raw`, `dep_lat`, `dep_lon`, `arr_airport`,
`arr_time_raw`, `arr_lat`, `arr_lon`, `flight_number`, `flight_note`,
`carrier_code`, `airline`, `aircraft_type`, `aircraft_note`, `airport_note`,
`distance_mi`, `scraped_at`

## Usage

```sh
Rscript scripts/build_all.R              # full rebuild (uses the HTML cache)
Rscript scripts/build_all.R --refresh    # full rebuild, re-fetching every post
Rscript scripts/update.R                 # weekly incremental refresh
Rscript -e 'testthat::test_dir("tests/testthat")'
```

A [GitHub Action](.github/workflows/update.yml) runs `update.R` every Wednesday
morning, runs the tests, and commits the result if anything changed.

## How it works

1. **Discover** — crawl `/tag/nfl-charter-flights`. Slugs are not generable from
   (season, week): weeks appear both as digits (`week-12`) and number words
   (`week-five`), and the trailing segment varies (`-team-charter-flights` vs
   `-team-charter-flight-schedule`, used throughout 2024). Preseason and playoff
   posts are *classified* and then filtered out, so an unrecognised post type
   errors rather than being misfiled as a regular-season week.
2. **Fetch** — cached in `cache/html/`, which is committed. Only new posts hit
   the network, and the history survives the blog changing.
3. **Parse** — walk `<h2>` date headings and `<li>` items in document order,
   restricted to the article body. One `<li>` can hold several
   `<br />`-separated legs, when a team splits across two aircraft.
4. **Normalize** — resolve dates against the post's publication date, teams
   against `nflreadr::load_teams()`, and airlines from the flight-number prefix.
5. **Enrich** — join airport coordinates, compute great-circle distance.
6. **Validate** — unmapped teams, unknown airports, unmapped carriers,
   implausible distances, or a shrinking dataset all fail the build.

## Reference tables

Small, hand-checked lookups in `inst/reference/`:

- `carriers.csv` — carrier prefix → airline. 2023 and 2025 use ICAO 3-char codes
  (`DAL8862`, `GTI8086`); **2024 uses IATA 2-char** (`DL8863`, `5Y8088`). Keying
  on both replaces the hand-maintained 494-row flight-number map this project
  previously kept in Google Sheets.
- `airports.csv` — IATA → name, city, coordinates (from OpenFlights).
- `team_aliases.csv` — short forms the blog uses (`Kansas City`) and typos.
- `airport_fixes.csv` — documented corrections for codes the source got wrong.
- `post_overrides.csv` — season/week for any slug the parser cannot classify.

When the blog introduces a new carrier or airport, validation fails with the
offending value; add a row to the relevant file and re-run.

## Known caveats

- **`direction` is a heuristic.** The source never states which way a team is
  flying. Within a matchup in one post, legs on the earliest date are labelled
  `outbound` and later ones `return`.
- **Times are kept as published text** (`dep_time_raw`, `arr_time_raw`, e.g.
  `130pm CT`). They are local to each airport and are not parsed into timestamps.
- **Ground travel is excluded.** Items reading `Bussing` are skipped, as are
  in-season practice and training-camp trips.
- **Ten legs have no flight number**, published as `TBD`; their `airline` is
  empty.

## Refreshing the dashboard

Tableau Public has no HTTP/CSV connector and refreshes only through
cloud-storage connectors, so a Google Sheet acts as a one-cell passthrough:

```
=IMPORTDATA("https://raw.githubusercontent.com/<user>/nfl-flights/main/inst/extdata/charter_flights.csv")
```

The repo must be public for this to work, since Sheets cannot authenticate to a
private raw URL.

## Package readiness

The repo is laid out as an R package (`R/` holds functions only, data ships in
`inst/extdata/`, `DESCRIPTION` and `.Rbuildignore` are in place) so it can be
turned into one without restructuring. Roxygen docs, `NAMESPACE` and
`R CMD check` are deliberately deferred.

Data: [Doug Kroll / JetTip](https://blog.jettip.net). Viz: Stephan Teodosescu.
