# PrescribedFirePlanner — structure (proposal)

*Draft 2026-11-02, for review. Covers the smallest useful version (v0.1) and how it grows into the v1 in `docs/SPEC.md`.*

## v0.1: the smallest useful version
- Pick **one station**: a featured list plus search across the metadata CSV.
- Enter a **prescription**: min/max for temperature, RH, wind speed, wind direction sector, precipitation and solar radiation.
- Fetch the station's **2005–2025 weather** from FEMS (weather feed only).
- Show **% of hours in prescription by month × hour** as a heatmap and a table, with the number of hours excluded for missing data.
- A basic **Data quality** tab: the coverage grid, the screening log and the timing-step items, with the sidebar badge.

Not in v0.1: the day-of-year chart, sun lines, per-variable panels, past-event views, synthetic forecast, presets and PDF.
Each of these is a new view on the same three layers below, not a new pipeline.

## Three layers, each written once

```
FEMS (later ERA5, synthetic forecast)
   │  hourly_*()      → standard hourly table          R/hourly.R
   ▼
rx_flag()              → + pass_<var>, in_rx            R/rx.R
   │
   ▼
clim_summarise()       → share/mean by any grouping     R/climatology.R
   │
   ▼
plot_*() / tables       (no calculation in plots)       R/plots.R
```
Beside the main line: `quality_*()` in `R/quality.R` reads the hourly table (after `hourly_qc()`) and returns report items
for the Data quality tab; `sun_*()` in `R/sun.R` supplies solar noon, sunrise and sunset to both the quality checks and the sun lines on charts.

### 1. Standard hourly table (`R/hourly.R`)
One row per station-local hour. Every data source must produce this; nothing after this layer knows where the data came from.

| column | type | notes |
|---|---|---|
| `station_id` | chr | |
| `source` | chr | `"FEMS"` now; `"ERA5"`, `"SYNTH"` later |
| `date`, `hour` | Date, int 0–23 | local station time from `display_hour_lst` |
| `year`, `month`, `week`, `doy` | int | `doy` on a 365-day calendar (Feb 29 → Feb 28); `week` = `ceiling(doy/7)`, capped at 52 |
| `temperature` °F, `relativeHumidity` %, `windSpeed` mph, `windDirection` deg, `precipitation` in, `solarRadiation` W/m² | dbl | names kept from the download contract |

- `hourly_from_fems(wx)`: maps the weather download to the table (observed records only) and **completes the hourly grid**:
  every station-local hour from the first to the last date gets a row, with `NA` values where FEMS sent nothing. FEMS gap-fills
  older records but, from about Nov 2025, leaves missing hours out entirely (found in the bundled snapshots: CRANE lacks 163
  hours, BLOXHAM 59). Without this, absent hours wouldn't count as excluded, and two hours either side of a gap would look
  consecutive, which would lengthen burn windows and stuck-value runs. `attr(h, "grid_filled")` records how many rows were added.
- `hourly_add_calendar(h, tz)`: adds year, month, week and doy (all from the LST date), plus two **display-only** clock-time
  columns from the station's IANA zone: `time_clock` (POSIXct, for time-series views) and `hour_clock` (for hour-of-day
  grouping when the clock-time toggle is on). At stations without DST these equal LST.
- `hourly_qc(h)`: lenient screening. It sets clear faults to `NA` and attaches `attr(h, "qc_log")`, one row per removed run
  (rule, variable, start_date, start_hour, hours, days, value), for the data-quality report. The rules, in this order,
  with thresholds in `QC_*` constants (tuned on the four bundled stations, 2 Nov 2026):
  - **limits:** a reading outside temperature −40..130 °F, RH 0..100, wind 0..120 mph, gust 0..200, precipitation 0..6 in/hr or
    solar 0..1500 W/m² removes that variable for the **whole local day plus one day either side**. A failing sensor drifts
    first: BLOXHAM read 112 °F the day before it passed 130 °F, in February 2005.
  - **spike:** a single hour differing from both neighbours in the same direction by more than 20 °F, or 40 % RH.
  - **stuck:** the same value for 24 h or more (temperature, RH below 100, non-zero wind and gust, direction while the wind blows).
    Daylight solar radiation (> 50 W/m²) is never 24 h long, so it uses 6 identical readings in a row; SAGUARO's longest real run is 3.
  - **long calm:** wind 0 for more than 24 h, which sets wind speed, gust and direction to missing.
  - Derived columns follow: dewpoint and VPD go with temperature or RH, the hot-dry-windy index with wind.
- `hourly_validate(h)`: checks columns, types, ranges and duplicate keys; `stop()`s with a list of problems. Used by the app at load time and by tests.
- `hourly_load_station(station_id, prefer_bundled = TRUE, start_date, end_date, progress = NULL)` returns a **station object**,
  a list: `$meta` (from `hourly_station_meta()`: name, lat/lon, elevation, tz, standard offset, DST, metadata precipitation),
  `$hourly`, `$qc_log`, `$grid_added` and `$source` (`type` "bundled" or "FEMS live", plus a `detail` sentence). It's a list,
  not attributes, because data-frame subsetting drops attributes. The raw weather comes from `hourly_read_bundled()` when the
  station is in the manifest and `prefer_bundled` is true (the Refresh button sets it false); otherwise from FEMS. Either way the
  same steps follow, so a screening change applies to both. Takes about 1 s for a bundled station.
- `hourly_baseline(h)` keeps the `RX_BASELINE_YEARS` rows; the full record stays loaded for past-event views.
- `hourly_read_bundled(station_id)`: reads `data/stations_bundled/<id>.rds`, checks its checksum against the manifest and
  checks the download contract; returns `NULL` (so the app falls back to FEMS) with a warning if either fails.

### 2. Prescription and flagging (`R/rx.R`)
A prescription is a small tibble: `var`, `min`, `max`, with `NA` meaning no limit. Wind direction is a clockwise sector
from `min` to `max` that may wrap through north (e.g. 315 → 45). Variable metadata (label, units, linear or circular)
lives in one table, `RX_VARS`, in `R/constants.R`. Adding NFDRS variables later means adding rows there and mapping their columns.

- `rx_new(...)` / `rx_validate(rx)`
- `rx_flag(h, rx)`: adds one `pass_<var>` per constrained variable and `in_rx`, which is `TRUE` only if all of them pass.
  If any constrained variable is missing for an hour, that hour's `in_rx` is `NA`, so it drops out of both the numerator and the denominator and is counted as missing.
  Unconstrained variables are ignored, including when they're missing.
  Wind direction: 360 is treated as 0, and a **calm hour (wind speed 0) passes** a direction limit whatever direction is
  reported. A missing wind speed with a direction limit counts as missing.
- v1 (hour totals): `rx_flag` also adds `only_<var>` per constrained variable: `TRUE` when that limit fails and every
  other constrained limit passes ("the one limit holding the hour out"), `NA` when `in_rx` is `NA`. With one limit on,
  `only_<var>` is simply `!pass_<var>`.

### 3. The one aggregation (`R/climatology.R`)
```r
clim_summarise(flagged, value = "in_rx", by = c("month", "hour"), smooth_days = 0)
#> by-columns, n_valid, n_missing, n_years, stat
#>   stat = share TRUE for a logical value (in_rx, pass_rh, ...),
#>          mean       for a numeric value (relativeHumidity, ...)
```
- `value` may name several columns. The result is then long, one row per group per value, so a tooltip can get
  `in_rx`, every `pass_<var>` and the variable means for the same cell from one call.
- Every view and table calls this function. The month × hour heatmap, the month × hour table, the per-limit pass rates
  (`value = "pass_relativeHumidity"`), the later day-of-year chart (`by = c("doy", "hour"), smooth_days = 7`) and the
  per-variable panels (`value = "relativeHumidity"`) all use it.
- How it works: count `n_valid` and the sum of `value` per group, then (only when `by` has `doy`) take a rolling sum over the
  window, wrapping from Dec into Jan, and divide. Because it's sums ÷ counts, the smoothed result is exact; `n_years` counts
  distinct years inside the window. About 0.1 s at 190k rows. Asking for `smooth_days > 0` without `doy` in `by` is an error,
  not a silent no-op. Output columns: the `by` columns, `value`, `kind` ("share" or "mean"), `n_valid`, `n_missing`, `n_years`, `stat`.
- v1 (hour totals): the output gains `n_true` (the summed hours behind a share) so tables count hours from the same
  call instead of multiplying `stat` back out. Per-year counts use `by = c("year", "month")` or `c("year", "date")`, no
  smoothing; the median year, range and total are taken from those rows in `R/totals.R` (`totals_*`, presentation of
  counts only: no flagging, no second aggregation), which also writes the CSV header lines.
- The one planned addition is percentiles for the daily range charts in v1. `clim_quantiles()` will share the same
  grouping and wrap-around window helper (`.clim_window()`) rather than repeat it.

### Data quality (`R/quality.R`) and sun times (`R/sun.R`)
Written once, used by the Data quality tab **and** by the network tests (which currently carry their own copies in
`tests/helpers.R`; those move here when this file exists).
- `quality_report(st)` (a station object) → a data frame of items: `section` (screening, timing, coverage, plausibility, source),
  `severity` ("affects" or "info"), `variable`, `start`, `end`, `found`, `action`, `affects`; "affects" items first.
  `quality_badge(rep)` counts them. Screening is grouped, **one item per rule and variable** (e.g. "Wind reported calm for more
  than 24 h 39 times ... longest 294 h"); the run-by-run detail stays in `st$qc_log` for a table on the tab. Bundled stations
  come out at 4–7 badge items each.
- Always reported (info): the data source, the observation minute, and hours added by grid completion.
- `quality_coverage(h)` → % reporting per year × variable; `quality_gaps(h)` → the longest gaps.
- **Calm share:** wind calm in more than `QUALITY_CALM_SHARE` (20%) of baseline hours is an "affects" item, with the trend
  (BLOXHAM: about 18% in 2005–07 rising to 42% in 2023–25), since a wind-speed minimum then excludes many hours.
- **Precipitation:** the median annual total against the metadata average. Below 0.4 or above 2.5 → units suspect. 0.4–0.7 →
  "the gauge probably misses snowfall" (CRANE 0.51, ESTES PARK 0.67). Single dry or wet years are compared with the station's
  **own** median, not the metadata average.
- `quality_timing(h, meta)` → per year × variable daily-cycle peak hour (first harmonic) and solar centroid minus solar noon,
  then `quality_step()` on each series. That finds the split year that best separates two levels and reports it when the step is
  ≥ `QUALITY_STEP_H` (0.3 h) and ≥ 4 × the year-to-year spread.
- `quality_plausibility(h, meta)` → physical ranges, annual precipitation vs the metadata average, solar near 0 at night.
- `sun_noon_lst(lon, std_offset_h, doy)`, `sun_times(lat, lon, std_offset_h, doy)`: solar noon, sunrise, sunset and civil twilight in LST.

### Download robustness (`R/fems_api.R`)
- `fems_gql()` retries on HTTP 502/503/504 and on timeouts or connection failures, waiting `FEMS_RETRY_WAITS` = c(2, 5, 15) s.
  It never retries 401/403 (credentials) or GraphQL `errors` (bad query).
- After the last attempt it signals a classed condition, `fems_unavailable`, which the station module catches and shows as
  "FEMS isn't responding right now. Try again in a few minutes."
- For testing without a network, `fems_gql()` takes an internal `.post = httr::POST` argument that a test can replace with a stub.

### Plots and the app
- `R/plots.R`: `plot_hour_heatmap(summary, x = "month", fill = "stat", ...)` and friends. These take a `clim_summarise()` result and only draw it.
- **Tooltips:** `R/tooltips.R` builds the hover text from summary columns, with one function per mark type
  (`tip_cell()`, `tip_hour()`, `tip_range()`, `tip_station()`) and shared formatters for %, units, hours and "LST".
  Plots pass it to plotly as `text` with `hoverinfo = "text"`, so every chart uses the same wording.
  It's tested without a browser: `test_tooltips.R` checks the strings, using `grepl(fixed = TRUE)`, never regex on rendered HTML.
- **Shiny modules:** `R/mod_station.R` returns a reactive `station_id`, `R/mod_rx.R` returns a reactive `rx`,
  `R/mod_quality.R` takes `hourly()` and the station metadata and returns the badge count, and
  `R/mod_climatology.R` takes `hourly()` and `rx()`. `app.R` attaches shiny, bslib and dplyr, sources `R/` in a fixed order
  (constants → fems_api → fems_download → sun → hourly → quality → rx → climatology → tooltips → plots → mod_*), and wires the modules together.
- The quality report is computed once per station load and cached with the hourly table.
- **Cache:** `reactiveValues` keyed by `station_id`, for the session only (carryover §1).
- **Bundled snapshots:** `scripts/build_station_cache.R` (run by Mike, not the app) reads the stations with `bundled = TRUE`
  from `stations_featured.csv`, downloads each with `fems_download_weather()` from 2005-01-01 to today, and saves the raw
  result with `saveRDS(compress = "xz")` to `data/stations_bundled/<id>.rds`. It writes `data/stations_bundled/manifest.csv`
  (`station_id, name, file, start, end, rows, downloaded_at, md5`). These are committed to git; about 2–4 MB per station.
- Data files in `data/`: `stations_featured.csv` (`station_id, name, note, bundled, reviewed`); in v1, `rx_presets.csv`
  (long format: `preset_id, name, var, min, max, ignition_start, ignition_end, min_window_h, reviewed`).

## Naming, to avoid the ChartBuilder collisions
- Public functions carry a layer prefix: `hourly_`, `rx_`, `clim_`, `quality_`, `sun_`, `totals_`, `plot_`, `tip_`, `mod_`. No bare names that shiny, dplyr or
  plotly also export (`filter`, `validate`, `hr`, `layout`, ...).
- Internal helpers and every test helper start with a dot.
- No `library()` in `R/` beyond the existing `dplyr` and `tibble` in `fems_api.R`; everything else is namespaced (`plotly::plot_ly`).
- Constants go in `R/constants.R` only: `RX_BASELINE_YEARS`, `RX_VARS`, `CLIM_SMOOTH_DAYS`, `QC_*`, `QUALITY_*`.
  The exceptions are the download layer's own `FEMS_*` constants (`FEMS_ENDPOINT`, `FEMS_PER_PAGE`, `FEMS_RETRY_WAITS`),
  which stay in `R/fems_api.R` / `R/fems_download.R` so the tests can source the download layer on its own.
- Hour-of-day grouping uses `hour` (LST) by default, or `hour_clock` when the toggle is on. `clim_summarise()` takes the column
  name in `by`, so the toggle changes an argument, not the code.

## Tests
Each test runs with `source("tests/test_x.R")`, writes a transcript to `tests/logs/` and ends with `VERDICT: PASS` or `VERDICT: FAIL`.
Shared dot-prefixed helpers go in `tests/helpers.R`.
- `test_fems_smoke.R` (exists): the network check, updated so the last day must have all 24 hours once the window fix is in.
- `test_download_por.R` (exists): one station's full record. Covers structure, % reporting per year and month, units checked
  against physics and metadata (deg F, inches vs the metadata's annual average, W/m2 near 0 at night), solar timing vs computed solar noon,
  diurnal peaks, the morning temperature jump vs sunrise, and stuck sensors and spikes. Writes CSVs.
- `test_download_stations.R` (exists): four stations (AZ, FL, CO, CA) in Jan and Jul. Covers the contract, UTC offsets, solar timing,
  and the answer to the DST question.
- `test_rx_flag.R`: no network. Hand-built hours covering each limit at its edges, wind sectors that wrap through north,
  360 = 0, calm hours passing a direction limit, and missing values.
- `test_climatology.R`: no network. Known answers by hand, smoothing that wraps from Dec into Jan, Feb 29, and the month + smoothing error.
- `test_hourly.R`: no network. Contract validation against good and broken inputs. Grid completion: absent hours, including
  a half-day gap and a missing midnight, become `NA` rows; a complete grid is unchanged. Each QC rule fires on a planted fault
  (including a 120 h RH-at-1% run) and leaves real-looking weather alone (a monsoon outflow drop, fog at 100%, a 23 h calm).
  Clock-time columns are checked across the March and November changeovers.
- `test_tooltips.R`: no network. Tooltip text for known summaries: counts, %, units, the failed-limit wording.
- `test_quality.R`: no network. Planted cases: a 0.5 h temperature step from 2021 is found with the right year; a 120 h stuck
  run appears in the screening log; coverage counts; a drought year is "info", not "affects".
- `test_fems_retry.R`: no network. A stubbed `.post` returns 502, 502, then 200: one result after two waits. Always 502:
  `fems_unavailable` after the last wait. 401: no retry.
- `test_station_cache.R`: each manifest entry has its file, the checksum matches, the contract holds, the dates cover
  2005 to `end`. Optional network part: one week from each file matches a live download.
- `test_hour_labels.R` (exists): per-year timing of every variable and the raw observation times, to diagnose averaging or labelling changes.
The network-free tests can also run in Claude's container first, but Mike's run on Windows is the real check.

## Build order (each item ends with Mike running a test)
1. **Download layer (done 2026-11-02):** fix the weather-window bug in `fems_download_weather()` (request a day earlier and a day later in UTC,
   then trim to local dates), and add the retry to `fems_gql()`. Tests: `test_fems_retry.R` plus re-running the smoke test.
   Then **1b.** `scripts/build_station_cache.R` and `test_station_cache.R`; build the four snapshots.
2. **Core layers (written 2026-11-02, pending Mike's run):** `constants.R`, `sun.R`, `hourly.R` (with QC), `quality.R`, `rx.R`,
   `climatology.R`, plus `test_sun.R`, `test_hourly.R`, `test_rx_flag.R`, `test_climatology.R`, `test_quality.R` (no network;
   the hourly, climatology and quality tests also run the real bundled snapshots). Still to do: switch the network tests from
   their own copies in `tests/helpers.R` to the `R/` functions.
3. **v0.1 app (written 2026-11-02, pending Mike's run):** `tooltips.R`, `plots.R`, the four modules and `app.R`, plus
   `test_tooltips.R` and `test_ui.R` (the UI evaluated from `app.R`, and `shiny::testServer()` runs of the prescription and climatology
   modules). Also checked in headless Chromium: loading from a featured button and from the map popup, the number-box/slider
   sync, the inline messages, the clock toggle, hover text, and the Data quality and Table views.
   - The station module adds `st$report` (from `quality_report()`) at load, so the badge and the tab share one report.
   - After a load the app switches to Climatology; the sidebar badge opens Data quality.
   - Heatmap hours run top to bottom (midnight at the top), on a fixed 0–100% blue scale (`CHART_SEQ`).
   - The starting prescription is `RX_EXAMPLE` in `constants.R`, labelled "an example, not a recommendation", until presets exist.
   - Needs `shiny`, `bslib`, `leaflet` and `plotly` (all namespaced except shiny and bslib, which `app.R` attaches).
4. First deploy to Connect with `deployApp(appName = ...)` once the app name and URL are decided.
