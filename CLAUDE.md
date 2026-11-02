# CLAUDE.md — PrescribedFirePlanner

R Shiny training tool for the Prescribed Fire Training Center: how often, and when, a station's weather falls inside a
burn prescription. Owner: Mike Crimmins (University of Arizona). GitHub: `mcrimmins/PrescribedFirePlanner`.

**Read first:** `docs/SPEC.md` (what v1 does), `docs/ARCHITECTURE.md` (how it's built),
`docs/carryover_from_NFDRSChartBuilder.md` (FEMS API and Shiny lessons, the background for most rules below).

## How we work
- **Claude can't run R on Mike's machine.** Mike runs tests in RStudio on Windows and pastes back the transcript.
  A pass in Claude's Linux container is only a smoke test.
- **Mike runs all git commands** in the RStudio terminal. Claude never runs git through the file bridge (it leaves `.lock` files behind).
- Work in small steps and **stop for review** after each one. Ask before expensive or ambiguous work; say which assumptions were made.
- **Claude verifies every file it writes to the laptop.** Writes go out from a fresh copy, then Claude reads the laptop file back and
  diffs it against its own copy before saying it's done. (Early on, writes made right after an edit delivered the pre-edit version;
  this was Claude's write path, not RStudio.) Claude also re-reads a laptop file before editing it, in case Mike changed it.
- Keep `.R` files ASCII-only (Windows encoding). Put °, ², etc. in UI strings with `\u` escapes.

## Architecture rules (see `docs/ARCHITECTURE.md`)
- **One standard hourly table.** Every data source (FEMS now; ERA5 and synthetic forecasts later) is converted by `hourly_*()`
  in `R/hourly.R` and passes `hourly_validate()`. Nothing downstream knows the source.
- **Prescription logic exists once:** `rx_flag()` in `R/rx.R`. `in_rx` is `NA` when any constrained variable is missing.
- **Aggregation exists once:** `clim_summarise()` in `R/climatology.R`. Every heatmap, table, pass rate and tooltip number comes from it.
  Never copy an aggregation chain into a plot, a module or an export (that was ChartBuilder's debt).
- **Plots only draw.** `R/plots.R` takes summaries and draws them; it does no calculation.
- **Tooltips come from `R/tooltips.R` only**, using the shared formatters. Every tooltip states what the mark is, the number behind it,
  and how many hours it's based on. Times are labeled "LST" (or the clock-time zone when the toggle is on).
- **Constants live in `R/constants.R` only** (`RX_BASELINE_YEARS`, `RX_VARS`, `CLIM_SMOOTH_DAYS`, ...). No magic years or windows elsewhere.
- v1 prescription variables: temperature, RH, 20-ft wind speed, wind direction (a sector that may wrap through north),
  precipitation and solar radiation. NFDRS variables get added later as rows in `RX_VARS`, not as special cases.
- Day of year uses a 365-day calendar (Feb 29 → Feb 28). Smoothing wraps from Dec into Jan and is exact (sums ÷ counts).
- **Hours are LST everywhere in the analysis.** FEMS labels hours in local standard time all year (confirmed in FL, CO, CA).
  `hour_clock` and `time_clock` are display-only columns for the clock-time toggle; never flag or aggregate on clock time unless
  the toggle asks for it, and never feed clock time back into the data.
- **A loaded station is a list, not a data frame:** `st <- hourly_load_station(id)` gives `st$meta`, `st$hourly`, `st$qc_log`,
  `st$grid_added`, `st$source`. Pass `st` around. Don't keep metadata in attributes, because data-frame subsetting drops them.
- **Single-number conditions use `if`, never `ifelse()`:** `ifelse()` with a length-1 test returns one value, which then gets
  recycled over every hour (this was a real bug in `.rx_in_sector()`, caught by `test_rx_flag.R`).
- **Screening runs before flagging:** `hourly_qc()` (lenient) sets clear sensor faults to `NA` and logs them; screened hours
  are excluded hours. Thresholds are `QC_*` constants.
- **Wind direction:** 360 is treated as 0; calm hours (0 mph) pass a direction limit.
- **Data-quality diagnostics exist once:** `quality_*()` in `R/quality.R`, plus `sun_*()` in `R/sun.R`. The Data quality tab and the
  network tests both call them; don't re-derive coverage, timing steps or sun times anywhere else. Every report item says what
  was found, what the app did, and what it affects.

## FEMS data rules (carryover §1)
- The API is GraphQL; HTTP 200 can still carry an `errors` array, and `fems_gql()` checks it. Auth is `FEMS_USER` + `FEMS_KEY` in a gitignored `.Renviron`.
- Request **one station at a time** with local station time. Never batch stations into one request.
- `date`/`hour` come from `display_hour_lst` by `substr()`. Never parse it into POSIXct or convert time zones.
- Always send `sortBy: "observation_time", sortOrder: "asc"` when paging.
- Use observed records only (`record_type == "O"`). `vapor_press_def` is in Pa; `station_id` must be character.
- **Weather window (fixed 2026-11-02):** `weatherObs` takes UTC bounds, so the request is padded one day each side and trimmed
  to local dates. Before the fix the last local day lost 17:00–23:00. Smoke test checks C7/C8/E5 guard it; don't remove the padding.
- Cache downloads in `reactiveValues` for the session only; never on disk.
- **Retry transient failures:** `fems_gql()` retries 502/503/504 and timeouts (2, 5, 15 s), never 401/403 or GraphQL errors,
  then raises `fems_unavailable` for a plain message in the UI. FEMS does return 502s now and then.
- **The hourly grid isn't always complete.** FEMS gap-fills older records (8760/8784 rows a year), but from about Nov 2025 missing
  hours are simply absent. `hourly_from_fems()` adds the missing rows as `NA` before anything else, so every later step
  (screening, flags, window lengths, counts) can treat consecutive rows as consecutive hours. Snapshots stay raw.
- **Observation minute is per station** (SAGUARO :08, BLOXHAM :29, unchanged 2005–2025); the hour label is that hour rounded down.
- **Sensor averaging changed mid-record:** solar radiation steps about +0.5–0.7 h from 2020 at SAGUARO and BLOXHAM (likely
  network-wide); SAGUARO's temperature and wind also step about +0.5 h from 2021 (station-specific). This fits a switch to hourly
  averages ending at the observation minute. Hour labels did not change. Don't "correct" this in the data; report it (Data quality tab).

## R/Shiny namespace rules (carryover §2)
- No `library()` in `R/` files, except the existing `tibble`/`dplyr` in `fems_api.R`. Write the package name before everything else
  (`jsonlite::`, `httr::`, `plotly::`, `leaflet::`). `app.R` attaches shiny, bslib and dplyr, and sources `R/` in a fixed order.
- Public functions carry a layer prefix: `hourly_`, `rx_`, `clim_`, `plot_`, `tip_`, `mod_`. Never reuse a name shiny,
  dplyr or plotly exports (`filter`, `validate`, `hr`, `layout`, ...). Internal helpers start with a dot.
- On an "unused argument" error from a function shiny exports, run `find("<name>")` first.
  RStudio's Restart R does **not** clear the global environment; use `rm(list = ls())`.

## Tests (carryover §3)
- `tests/test_<topic>.R`, run with `source("tests/test_<topic>.R")` with the .Rproj open. Paths are relative to the project root.
- Each test writes a transcript to `tests/logs/` (gitignored) and ends with `VERDICT: PASS` or `VERDICT: FAIL`.
  Check levels: **PASS/FAIL** = the code, or the data breaking an assumption the app relies on (decides the verdict);
  **OK/WARN** = a data-quality finding about a station (listed in the summary, never fails the test); **INFO** = diagnostics only.
  Larger diagnostic tables go to CSVs next to the transcript.
- Wrap the test body in `local({ ... })` and start it with `source("tests/helpers.R", local = TRUE)`, so only what `R/` defines
  lands in the global environment. Test helpers start with a dot.
- Tests that need no network use hand-built data and hand-checked answers. Network tests:
  `test_fems_smoke.R` (quick, one week), `test_download_por.R` (one station's full 2005–2025 record: structure, completeness,
  units, time of day, stuck sensors), `test_download_stations.R` (four stations in four time zones, Jan and Jul: contract, UTC offsets, DST convention).
- Claude checks new tests against a fake FEMS in its container first, including injected faults (UTC labels, deg C, mm)
  that the test must catch.
- Never run regex over rendered Shiny HTML: walk the `shiny.tag` tree or use `grepl(fixed = TRUE)`.
  Check UI shape by evaluating only the `ui <- ...` expression.
- After a surgical rewrite, prove the untouched regions are byte-identical (`git show HEAD:<file>`) rather than asserting it.

## Data files
- `data/station_metadata_FEMS3_042225.csv`: station list, coordinates (used for the map and sun times).
- `data/stations_bundled/`: raw FEMS weather snapshots for the bundled stations (`<id>.rds`, xz) plus `manifest.csv`, built
  by `scripts/build_station_cache.R` and **committed to git**. The app only reads them; screening and quality checks run at load
  time, so never bake screening into a snapshot. To change the list: edit `bundled` in `stations_featured.csv`, rerun the
  script, run `test_station_cache.R`, commit, redeploy. Bundled to start: 21202, 80802, 50507, 44102.
- `data/stations_featured.csv`, `data/rx_presets.csv`: drafted by Claude, each row with `reviewed = FALSE`
  until Mike or PFTC check it. The UI marks unreviewed rows. Never mark a row reviewed without Mike saying so.
- Synthetic forecast error sizes are assumptions until reviewed. Forecast views are always watermarked "SYNTHETIC — FOR TRAINING ONLY."

## Layout (summary of `docs/SPEC.md`)
Tabs: Station (Leaflet map) · Climatology · Past event · Same dates · Forecast · About, with one persistent sidebar:
station, preset, and limit controls (checkbox + min/max number boxes kept in sync with a range slider, inline validation).
Date controls live in their own tabs and carry forward one way as defaults. Every chart is plotly with hover tooltips.

## Deploy and git (carryover §5)
- Posit Connect: `viz.datascience.arizona.edu`, account `crimmins`. Public link.
- Deploy with `rsconnect::deployApp(appName = "...")` from the console, never RStudio's Republish button.
  Before deploying, be on `main`, have `git status --short` empty, and tag the release.
- Choose the Connect app name and URL once (still open) and never rename it.
- `.rscignore` controls what gets bundled for deploy (it excludes `.Renviron`, `CLAUDE.md`, `tests/`, `archive/`, `docs/`, `scripts/`).
  `.gitignore` doesn't affect deploys.
- A new Connect app needs `FEMS_USER` and `FEMS_KEY` set as environment variables in the Connect UI, or every query returns 401.
- `main` is always deployable. Use short-lived feature branches; run `git log --oneline <branch>..main` before merging, and delete the branch after.
  Close the R session before switching branches. Keep the repo out of OneDrive.

## Layout of the repo
`app.R` (root) · `R/` helpers, sourced explicitly · `data/` · `www/` logos (root) · `tests/` (+ `tests/logs/`, gitignored) ·
`scripts/` one-off processing · `docs/` spec, architecture, notes · `archive/` superseded versions.

## Open items
- The Connect app name and URL.
- Review of the lenient QC thresholds against PFTC stations.
- Find the source of the 2020 solar change and SAGUARO's 2021 temperature and wind change.
- Whether a per-hour precipitation limit is enough, or trainees need "hours since rain".
