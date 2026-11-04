# Prescribed Fire Weather Explorer (RxWx) — v1 spec

*Repository: PrescribedFirePlanner.*

*Draft 2026-11-02. Mike Crimmins (University of Arizona) for the Prescribed Fire Training Center (PFTC).
Builds on NFDRSChartBuilder; conventions in `docs/carryover_from_NFDRSChartBuilder.md`.*

## Purpose and users
A training tool for **new PFTC trainees** to learn how prescribed-fire weather windows vary by place, by time of year
and by time of day, and how to read a forecast and its uncertainty. It is used **in instructor-led classes and
self-paced**, so every view has short plain-language explanations and opens from a preset scenario.
It will be developed iteratively with PFTC feedback and could become operational later; v1 is not an operational planning tool.

## Questions it answers
1. In a typical year, **when** (month, week, hour of day) are conditions inside this prescription, and **for how long each day**?
2. What were conditions, hour by hour, during a **specific past period** (e.g. a burn that was carried out)?
3. How did the **same dates** compare **across years**?
4. Given a **forecast** with uncertainty, would I expect to be in prescription, and how did it actually turn out?

## Inputs
- **Station:** one at a time. A curated list of featured stations is offered first, and any FEMS RAWS station can be found by search.
  Four stations are **bundled** with the app and load instantly, with no FEMS download: FLAGSTAFF (AZ), HAYWARD (WI),
  OLUSTEE (FL) and REDDING (CA), chosen with partner feedback. The list changes with feedback.
- **Prescription:** loaded from an **editable preset**. Each variable has an optional min and max (inclusive; blank = no limit):
  temperature, RH, 20-ft wind speed, wind-direction sector, precipitation and solar radiation.
  Precipitation is limited per hour (default max 0, i.e. no rain that hour); solar radiation in W/m².
  Wind direction: 0 and 360 are the same direction, and **calm hours (0 mph) pass a direction limit**,
  since direction doesn't matter with no wind (the wind-speed limit still applies).
  NFDRS variables (1-hr/10-hr fuel moisture, ERC, BI) can be added later the same way.
  A preset also carries its **ignition period** (start/end hour) and a **minimum usable window** (hours).
  *Built 2026-11-02:* five DRAFT presets for PFTC review (Example (all day), Grass / rangeland, Southern pine understory,
  Western conifer understory, Pile burning) in `RX_PRESETS` / `RX_PRESET_META`. Choosing one fills every control; the
  note under the menu says "Edited from this preset" once anything differs. The **ignition period is in local clock
  time** (start inclusive, end exclusive; e.g. 09:00-17:00 is the hours starting 09 to 16), so at a daylight-saving
  station it falls an hour earlier in LST in summer. Hour totals, longest runs and usable days count only the ignition
  hours; the Climatology heatmap washes out cells outside it, the Past event strip shows in-prescription hours outside
  it pale, and the Same dates colours run 0 to the period's length. The minimum usable window (sidebar) sets the
  usable-day rule on Past event and Same dates.
- **Dates:** the climatology period (fixed baseline, below), a past date range for the event view (up to about 14 days),
  and a forecast issue date/time for the synthetic forecast.

## Layout
A bslib page with tabs along the top (**Station · Climatology · Past event · Same dates · Forecast · Data quality · About**) and one sidebar
that stays the same on every tab. Before a station is loaded, the sidebar has search and the featured list. Afterwards it shows
the station, the preset menu and the prescription controls, as two sections (Station, Prescription) separated by a rule.
The sidebar is kept compact: each limit is an on/off checkbox and one two-handle **range slider** (no number boxes); a
handle at the end of its slider means no limit on that side. The wind-direction sector is two 16-point compass menus
(from, to; clockwise, may wrap through north). Inline messages explain any problem. Ignition hours and minimum window follow. Changing a limit recomputes the flags and redraws without downloading again (debounced).
**Date controls live in their own tabs**, because each tab's dates mean something different. Choices carry forward one way as
defaults: a Past event range becomes the starting window for Same dates, and its start becomes the default Forecast issue time.
Every chart's subtitle states the dates it shows. The **Station** tab is a Leaflet map of every FEMS station, with featured stations
highlighted. Clicking a marker shows a popup with a load button, and the download shows a progress bar.
After loading, the sidebar's station card shows a **badge** (e.g. "3 notes") that links to the Data quality tab.

## Interactivity and tooltips
Every chart is interactive (plotly; Leaflet for the map): hover, zoom and pan, legend toggles, and plotly's PNG button.
Every hover tooltip says **what the mark is, the number behind it, and how many hours it's based on**,
using the same wording, units and time label ("LST", or the clock-time zone when the toggle is on) everywhere:
- **Hour × day / month × hour cell:** date window (e.g. "Mar 14–28") and hour; "In prescription: 64% of 147 hours (21 years)";
  hours excluded for missing data; the limit failed most often in that cell and how often it failed; typical (mean) values of the constrained variables.
- **Per-variable panel:** that variable's typical value, the limit, and the % of hours within that limit.
- **Daily range chart:** date, median, 25–75% and 10–90% ranges, and the prescription range.
- **Sun lines:** sunrise or sunset time on that date.
- **Past event / Same dates hour:** timestamp, each variable's value, and in or out, naming the limits it failed.
- **Forecast hour:** lead time, central value and band; after the reveal, the observed value and the forecast error.
- **Map marker:** station name and ID, elevation, years of data, and whether it's featured or unreviewed.

## Views
- **Data quality** (in v0.1): a heads-up report for the loaded station, built when it loads. Each item says **what was found,
  what the app did, and what it affects**, marked either "affects results" (amber; these count toward the badge) or "for information" (grey).
  - **Coverage:** a year × variable grid of % of hours reporting, plus the longest gaps.
  - **Screening log:** each run removed by screening, e.g. "RH stuck at 1% for 120 h from 12 Jul 2008. Removed; otherwise
    those hours would count as in prescription for any RH maximum."
  - **Timing:** per year and variable, the hour at which the daily cycle peaks, and solar timing against computed solar noon.
    A step is reported with its year and size, e.g. "Temperature and wind switch to hourly averages from 2021 (~30 min later);
    hour-of-day results mix both conventions."
  - **Plausibility:** ranges against physical limits, and annual precipitation against the metadata average
    (e.g. "2020: 4.1 in vs 13 in average, a real drought year, included").
- **Climatology:**
  - **Hour × day-of-year chart** (WeatherSpark style, the main view): hour of day on the vertical axis and Jan–Dec across.
    Each cell is colored by the chance that hour is in prescription, smoothed with a centered ±7-day window.
    Overlays: sunrise, sunset and civil twilight lines for the station's location, the preset's ignition period,
    and an orange outline around cells at or above a settable threshold (default 50% of hours), marking the climatological
    window by outline as well as colour. Colour ramp: one hue, white to navy (ColorBrewer Blues), fixed 0–100%.
    *Built 2026-11-02* as the first view of the Climatology tab ("Day of year", then "Month × hour" and "Table"): every
    3rd day is drawn (the ±7-day smoothing makes daily columns redundant; it keeps the widget under 2 MB). By day of year
    the threshold is a contour line rather than cell outlines; the ignition period is a dashed band (an hour earlier in
    LST through daylight saving). Hover: date, hour, share and hours behind it, that day's sunrise and sunset, the
    limit that fails most often, typical values.
  - **Per-variable panels on the same axes:** the hourly climatology of each constrained variable (e.g. median RH)
    with the prescription limits drawn as contour lines, so trainees can see which limit closes the window, at which hours and in which season.
    Wind direction is shown as the % of hours in the allowed sector.
    *Built 2026-11-02 ("By variable" view), changed in the build:* each panel is coloured by the **share of hours that pass
    that one limit**, on the main chart's ramp and 0-100% scale, with the same threshold contour; the mean value is in
    the hover. Mean values with limit contours looked right but misled (a mean wind of 7 mph sits inside a 2-12 mph limit
    even when a third of the hours fail), while the share shows directly which limit closes the window, when.
  - **Daily range charts:** for each variable, the median with 25th–75th and 10th–90th percentile bands
    through the year, with the prescription range shaded on top.
    *Built 2026-11-02 ("Daily ranges" view):* percentiles of the hourly values over a ±7-day window, from the ignition
    period's hours when one is set (else all 24 hours, and the note says so); temperature, RH, wind speed and solar
    radiation (not precipitation, which is nearly always 0). Hover: median, middle half, 8 in 10 hours, share within
    the limit.
  - A month × hour table of the same percentages for quick reading.
  - Daily window length: the longest consecutive run of in-prescription hours within the ignition period,
    summarized by month and week as the median run and the % of days with a run ≥ the minimum usable window.
  - A table of how often each limit is met on its own, so trainees can see which variable is limiting.
- **Hour totals (report out), on every analysis tab:** counts of hours, so trainees can report "how many burnable hours"
  as well as "what share". Each tab shows a **totals table under its chart**, a **CSV download**, and the same table in the PDF.
  - **Rows by tab:** Climatology by **month** (plus a year row); Past event by **day** (plus an event total); Same dates by
    **year** for the window (per-day rows in the CSV); Forecast by **day**, from the central forecast, with the observed
    count beside it after the reveal.
  - **Columns:** hours in prescription (all limits met); for each constrained variable, **hours that limit is met on its own**
    and **hours that miss the prescription only because of that limit** (every other limit passes), which shows which
    limit is holding the window shut; and hours excluded for missing or screened data, so every count can be checked.
  - **Climatology counts are per year:** for each month, the **median year, the range (lowest and highest year, named)
    and the 2005–2025 total**, e.g. "Mar: 118 h in a typical year (64 h in 2010 to 171 h in 2018); 2,474 h over 21 years".
    A year-month with less than 80% of its hours reporting is left out of the median and range (and footnoted), since
    missing hours would read as fewer burnable hours; it still counts toward the total and the excluded-hours column.
  - Counts follow the ignition period when one is set (the column header says so), and the hour convention shown (LST or clock).
  - **CSV:** a tidy table with header lines starting `#` giving station, ID, data source and snapshot date, the prescription,
    years, hour convention and app version, so a saved file explains itself. Named `rxwx_<station>_<tab>_<date>.csv`.
  - Computed with `clim_summarise()` (sums, no smoothing) on columns from `rx_flag()`; no second counting path.
- **Past event** (built 2026-11-02): up to 14 days from the station's whole record (default: its last 7 days; Earlier and
  Later buttons step by the window's length). A top strip colours each hour in prescription, out, or not known; below it,
  one panel each for temperature, RH, wind speed and wind direction (plus precipitation or solar radiation when limited),
  with the prescription range shaded, markers blue when the hour is in prescription and orange where that limit failed,
  and night (sunset to sunrise) shaded. The hover names each failing limit with its value and the limit. Under the chart:
  hours in prescription by day with the longest consecutive run, the same per limit, and Daily and Hourly CSVs.
  Clock-time toggle at DST stations. The longest run stops at midnight (a day's window, not an overnight one).
- **Same dates, many years** (built 2026-11-02): one window of the year, up to 92 days (default 15 Mar to 15 Apr; it may
  wrap through New Year, and a button copies the Past event dates), for every year in the record. A year x day heatmap:
  colour = hours in prescription that day (0-24, or 0 to the ignition period's length; the climatology ramp), outline = a **usable day**, one with a run of at
  least N consecutive hours in prescription (slider, default 4 h; the run stops at midnight). The hover gives the day's
  hours, its longest run and the limit that alone held out the most hours. Below: per year, hours in prescription,
  usable days, longest run and data coverage, then typical year, lowest and highest (years named; a year under 80%
  coverage is starred and left out); the same per limit; Per year and Daily CSVs. 29 Feb is its own column, blank in
  non-leap years; a 29 Feb window end is read as 28 Feb.
- **Season tracker** (scoped 2026-11-04 from collaborator feedback; the "Rx Burn Period" component of the project doc,
  BPT-style; *not built yet*):
  - **Placement:** a tab after Climatology. It uses the sidebar's station, prescription, ignition period and minimum window.
  - **Year pull-down:** the current year by default, then every year in the record.
  - **Range:** opens on the whole calendar year, with a settable season window (start and end month-day) that zooms
    every chart to it.
  - **Chart 1:** one bar per day, the hours in prescription within the ignition period.
    - Behind the bars: the 2005-2025 **typical year (median), a 25-75% band and the record max**, smoothed ±7 days.
    - Days with a usable window (a run of at least the minimum) are marked.
  - **Charts 2+:** stacked panels, one per limit on, sharing the date axis and zoom with chart 1. Each shows the hours
    per day that limit is met; the hover adds the hours that miss only because of it.
  - **Current year:** observed days are topped up live from FEMS from the snapshot's end to today (one request, cached
    for the session).
  - **Forecast days:** FEMS forecast records (`observation_type` "F"), confirmed by `tests/test_fems_forecast.R`
    (4 Nov 2026, all four bundled stations):
    - **Hourly**, about 7 days ahead (179 hours), on the same station-local (LST) hours as the observations.
    - Every variable the app uses is filled: temperature, RH, wind speed and direction, precipitation, solar radiation.
    - Forecast hours start at the hour after the latest observation, with **no overlap**, so today is part observed,
      part forecast. Today's bar is stacked, with the observed hours solid and the forecast hours hatched.
    - FEMS returns **no issue-time field**, so the label reads "FEMS forecast, fetched <time>; starts <first forecast hour>".
    - NFDRS forecasts are hourly too (for later NFDRS variables).
    - Forecast hours go through the same flagging but not the screening rules (they are model output, not sensors).
      They are drawn after a "today" line and never added to observed totals; season totals report observed and
      "observed + forecast" separately.
    - The live request (snapshot end to today plus the forecast) takes well under a second per station.
  - **Interaction:** tooltips give the date, hours in prescription, longest run, the limit holding the day out, and
    observed or forecast. Zoom and pan are linked across the panels. **Clicking a day opens its week on Past event.**
  - **Totals:** season-to-date hours in prescription and usable days against the typical year, plus a CSV, in the same
    style as the other tabs.
- **Synthetic forecast:** *deferred 2026-11-04* (decide after the Season tracker).
- **Synthetic forecast:**
  - Pick a past issue time and get a 1–7 day forecast of temperature, RH, wind speed and wind direction as a
    **central value with an uncertainty band that widens with lead time**. Precipitation and solar limits are
    not forecast; the forecast view says they are ignored there.
  - Show the hours likely in prescription, then **reveal** the observed values.
  - Always watermarked **"SYNTHETIC — FOR TRAINING ONLY."**
- **PDF summary (exploratory):** a one-page report with station, prescription, climatology heatmap and window table.
  Kept in v1 only if it works on Connect without LaTeX or headless Chrome.

## Data sources
- **FEMS GraphQL API**, using the copied `R/fems_api.R` and `R/fems_download.R`. v1 fetches the **weather feed only**;
  the NFDRS fetch stays in the download layer for when those variables come in.
  Rows are **observed records only** (`record_type == "O"`), on **station-local hours** from `display_hour_lst`.
  - **Weather window (fixed 2026-11-02):** the weather request's start and end are in UTC, which used to drop 17:00–23:00 on
    the last local day. The request is now padded by a day on each side and trimmed to local dates; the smoke test guards it.
- **Station metadata:** `data/station_metadata_FEMS3_042225.csv`. The curated station list and presets live in
  `data/` as small CSVs, **drafted by Claude and marked unreviewed** until Mike or PFTC check them.
- **Climatology baseline:** 2005–2025, defined once as a constant. It's trimmed to each station's record, and the years used are shown.
- **Missing data:** "% of hours in prescription" is counted only over hours where every constrained variable is present.
  The number of excluded hours is shown.
- **Hour of day:** FEMS labels hours in local **standard** time (LST) all year. This was confirmed at FL, CO and CA stations
  by `test_download_stations.R`, and the hour labels line up with solar noon (`test_download_por.R`).
  All analysis is done in LST. Charts **default to LST**. At stations that observe daylight saving, a **clock-time toggle**
  shows summer hours one hour later (13:00 LST → 14:00 clock), and hour-of-day charts then step by an hour at the
  March and November changeovers, which the chart notes. The toggle is hidden at stations without DST (e.g. Arizona).
- **Observation minute:** each station reports at its own fixed minute past the hour (SAGUARO :08, BLOXHAM :29, every year
  2005–2025), and the hour label is that hour rounded down. So the "13:00" cell holds a reading taken during 13:00–13:59.
  The app keeps each station's observation minute, shows it on the Data quality tab and in tooltips ("observed at 13:29").
- **Sensor averaging changed mid-record (found 2026-11-02):** solar radiation shifts about +0.5–0.7 h from **2020** at both
  SAGUARO and BLOXHAM, so this is likely network-wide. At SAGUARO only, temperature and wind also shift about +0.5 h from
  **2021**; BLOXHAM's don't. The fit is with a switch from a reading at the observation minute to an hourly average ending
  there, which is about 30 min earlier in effective time. The app does **not** correct this; the Data quality tab detects and
  reports it per station.
- **FEMS outages:** the server sometimes returns 502/503/504. The download retries (waits of 2, 5 and 15 s) and then shows a
  plain "FEMS isn't responding right now" message instead of an error page.
- **Data screening (lenient):** before flagging, clear sensor faults are set to missing. These are: a variable stuck at one value for
  24 h or more (with exceptions for RH at 100%, zero rain and night-time solar), calm wind for more than 24 h, isolated
  one-hour spikes, and values outside physical limits. Screened hours count as excluded hours and appear in the data-quality
  summary. Thresholds are constants, drafted by Claude for review.
- **Sun times:** sunrise, sunset and civil twilight are computed from the station's latitude and longitude in the metadata CSV, with no extra data source.
- **Calendar:** day of year uses a 365-day calendar, with Feb 29 merged into Feb 28. Smoothing wraps around from Dec into Jan.
- **Bundled station snapshots:** for the bundled stations, the raw FEMS weather download (2005 through the build date) is
  saved by `scripts/build_station_cache.R` as one compressed file per station in `data/stations_bundled/`, with a manifest
  (download date, range, rows, checksum). Files are committed to git and deployed with the app. The app loads them
  read-only, applies screening and quality checks at load time exactly as for a live download, and labels them
  "bundled snapshot of <date>". A **Refresh from FEMS** button fetches the live record. Changing the list means editing
  `bundled` in `data/stations_featured.csv`, rerunning the script and redeploying.
- Downloads are cached in memory per session, with no disk cache. Bundled snapshots are read-only files shipped with the app.
- **Source-agnostic design:** all analysis works on one standard hourly table, so ERA5 data and synthetic forecasts can be swapped in later.

## Synthetic forecast method (v1)
- Forecast = the observed value plus a smooth, autocorrelated error whose spread grows with lead time, set per variable.
- The band is the 10th–90th percentile.
- Values are kept physical (RH 0–100, wind ≥ 0).
- A fixed random seed per scenario keeps a class exercise reproducible.
- Error sizes are drafted from typical NWS point-forecast skill and labeled as assumptions until reviewed.

## In v1 / out of v1
| In v1 | Deferred |
|---|---|
| One station, curated list plus search | Multiple stations or comparison |
| Editable presets, 6 weather variables | NFDRS variables (fuel moisture, ERC, BI) |
| Data quality tab with badge | Correcting the averaging change, or a single-convention baseline option |
| 4 bundled station snapshots, refreshable from FEMS | Scheduled automatic snapshot refresh |
| | ERA5 / Open-Meteo gridded hourly data |
| Climatology %, window length, limiting-variable table | Real or archived forecasts |
| Hour totals by month/day and by variable: table, CSV, PDF | |
| Past event and same-dates-across-years views | 1-hr fine dead fuel moisture (Fosberg) calculator |
| Season tracker: current year + FEMS forecast, any past year | Smoke dispersion, mixing height |
| *(Synthetic forecast with reveal: deferred)* | |
| Exploratory one-page PDF | Image export, saved user prescriptions |
| Public link on Posit Connect (viz.datascience.arizona.edu) | Operational use |

## Open items
- PFTC review of the drafted presets, curated stations and forecast error sizes.
- Whether a per-hour precipitation limit is enough, or trainees need "hours since measurable rain".
- Whether the synthetic forecast should also cover precipitation and solar radiation.
- Season tracker: whether the synthetic forecast tab is still wanted once real forecasts are in. (What FEMS forecasts
  contain: resolved 4 Nov 2026, hourly to about 7 days.)
- Hour totals: whether PFTC wants "hours in prescription" counted only in daylight or the ignition period by default,
  and whether the 80% coverage rule for the median year suits their stations.
- The Connect app name and URL, chosen once and not renamed.
- Review of the lenient screening thresholds against PFTC stations.
- Find the source of the 2020 solar change (FEMS/WIMS processing or RAWS standard?) and SAGUARO's 2021 change (datalogger
  upgrade?). The app reports both either way.
- *Resolved 2026-11-02:* FEMS LST is standard time all year (clock-time toggle added). The SAGUARO 06–07 temperature jump
  follows sunrise through the year (1.3–2.1 h after it), so it's physical, not a clock artifact.
