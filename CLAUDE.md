# CLAUDE.md

Scraper + pipeline for NFL team charter flights from blog.jettip.net, feeding a
Tableau Public dashboard. See `README.md` for the user-facing overview; this
file covers what's needed to work on the code.

## Commands

```sh
Rscript scripts/build_all.R              # full rebuild from the HTML cache
Rscript scripts/build_all.R --refresh    # re-fetch every post (hits the network 54x)
Rscript scripts/update.R                 # incremental: only new posts are fetched
Rscript -e 'testthat::test_dir("tests/testthat")'
```

Expected output of a clean build grows as JetTip publishes: as of 2026 week 5
it is **59 posts, 1,820 legs, 0 unparsed, 60 tests passing**. Counts should only
ever go up; a drop is a regression, not a data change.

## Layout

`R/` holds functions only — no top-level side effects, so the repo can become an
R package without restructuring. All execution lives in `scripts/`. Data ships
in `inst/extdata/` rather than `data/`, because `data/` is reserved for `.rda`
in packages **and** because the Google Sheet formula hardcodes the raw path —
moving the file breaks the dashboard.

Pipeline order: `discover` -> `fetch` -> `parse` -> `normalize` -> `enrich` ->
`validate`, wired together in `R/pipeline.R`.

Paths resolve via `root_dir()` (`R/fetch.R`), which reads
`options(nflflights.root)` and falls back to `getwd()`. Tests set that option
rather than `setwd()`, because testthat resets the working directory per file.

## Things that will bite you

- **Slugs are not generable** from (season, week). Weeks appear as digits
  (`week-12`) and as words (`week-five`); the trailing segment is
  `-team-charter-flights` in 2023/2025 but `-team-charter-flight-schedule`
  throughout 2024. Discovery crawls `/tag/nfl-charter-flights` (paginated to
  `/p29`) for this reason. Do not reintroduce URL generation.
- **Airline codes change format by season.** 2023 and 2025 use ICAO 3-char
  (`DAL8862`, `GTI8086`); 2024 uses IATA 2-char (`DL8863`, `5Y8088`).
  `inst/reference/carriers.csv` is keyed on both.
- **One `<li>` can hold several legs**, `<br />`-separated, when a team splits
  across two aircraft. Losing these was a bug in the original script.
- **Trailing parentheticals are everywhere** and mean different things: a tail
  number or remark on the aircraft (`B763 (N192DN)`), a venue on the home team
  (`(@ Sao Paolo, Brazil)` — sometimes unclosed), an editorial note on the away
  team (`(Week 3)`), an uncertainty marker on a flight number (`DAL8866 (?)`).
  Each goes to its own column; none belong in the value.
- **`<h2>` headings carry no year.** They are anchored against the post's
  publication date, which handles the December/January rollover. Weekday tokens
  are ignored because the source contains typos ("Thurday").
- **Site nav is inside `<li>` too.** Parsing is scoped to `div.blog-main`; a
  bare `html_nodes("h2, li")` picks up "Airports", "Login", etc.

## Validation is the safety net

`R/validate.R` runs before every write and **fails the build** on: unmapped
teams, unknown airport codes, unmapped carrier prefixes, unresolvable dates,
implausible distances, a parse-failure rate above 1%, or a shrinking row count.

When the blog introduces a new carrier or airport, the build fails with the
offending value named. The fix is a row in the relevant `inst/reference/` table,
not a loosened assertion. `airport_fixes.csv` corrections carry a `note` column
explaining the reasoning — keep that habit.

## Scope

Regular season only (weeks 1-18), 2023 onward -- `discover_posts()` takes a
`min_season` with no upper bound, so a new season flows through automatically.
Only *completed* seasons must hold all 18 weeks; the newest season need only be
contiguous from week 1, so a live season does not fail the build every week.
Preseason and playoff posts are
classified and *then* filtered, so an unrecognised post type errors rather than
being misfiled as a regular-season week. Re-enabling either is an argument to
`discover_posts()`; earlier seasons need `team_aliases.csv` entries for the
2019-20 city-name format ("Tampa", "Los Angeles (Rams)") and spelled-out
airlines ("Atlas 8106").

## Known-good spot checks

Regression cases with hand-verified expected values, all covered by tests:

| Case | Expectation |
| --- | --- |
| 2025 W17 NE @ NYJ | `aircraft_type` B763, `aircraft_note` N36NE (was NA) |
| 2025 W1 KC/LAC | `home_team` intact, `venue_note` "Sao Paolo, Brazil" |
| 2024 W14 GB @ DET | 4 rows: DL8863/B752 and DL8959/B739, both directions |
| 2023 W1 DET @ KC | `home_team_abbr` KC via `team_aliases.csv` |
| 2025 W17 BAL @ GB | outbound BWI->ATW, return GRB->BWI (asymmetric) |
| 2026 W1 SF @ LAR | `@ vs.` matchup normalised; Melbourne, QFA, 7,927 mi |

`direction` is a heuristic — earliest date in a matchup is outbound. The source
never states it.
