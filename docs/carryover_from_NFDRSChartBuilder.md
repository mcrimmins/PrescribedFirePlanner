# Carry-over notes from NFDRSChartBuilder

Background for the new prescribed fire planning Shiny app. It reuses code from NFDRSChartBuilder
(repo: github.com/mcrimmins/NFDRSchartBuilder, local: `C:\Users\Crimmins\RProjects\NFDRSChartBuilder`).
These lessons come from building and deploying that app and apply here too.

---

## 1. FEMS GraphQL API (reused code: `R/fems_api.R`, `R/fems_download.R`)

- **The API is GraphQL, not REST.** One endpoint: `POST https://fems.fs2c.usda.gov/api/ext-climatology/graphql`.
  Request bodies are `{"query": ..., "variables": ...}`. **GraphQL returns HTTP 200 even when a request fails**,
  so always check the `errors` array as well as the status code.
- **Auth is HTTP Basic.** Username = FEMS account username (`FEMS_USER`), password = the API key (`FEMS_KEY`).
  Both go in a gitignored `.Renviron`. Regenerating a key in the FEMS UI invalidates the old one, so check that first after a sudden 401.
- **Schema introspection is turned off on the server.** The FEMS API guide PDF is the only schema reference, and it has typos
  (for example `ndfr_date`). Only trust field names that have been confirmed to return data.
- **Local station time only works for one station at a time.** A multi-station query with `dateTimeFormat: "LocalStationTime"`
  is rejected. Local time is what makes `date` and `hour` correct, so the download layer requests stations **one at a time**
  (`.fetch_by_station()`). Don't merge these into a single batched request.
- **Time stamps:** get `date` and `hour` by applying `substr()` to `display_hour_lst`. No timezone handling is needed.
  `observation_time_lst` differs by a few seconds between the two feeds, so it can't be used as a join key.
- **There's no date-range cap.** A 21-year single request returned about 184k rows.
- **Paging:** 0-indexed `page`/`per_page` plus `_metadata { total_count page_count }`. Use large pages (default 100000);
  the time per request matters more than the row count. **Always send `sortBy: "observation_time", sortOrder: "asc"`**,
  because the two queries sort differently by default and paging without a fixed sort can drop or duplicate rows.
- **Unit and type quirks:**
  - `vapor_press_def` is in **pascals**. Divide by 1000 to get kPa.
  - `peak_gust_dir` is plain degrees, 0–359.
  - `station_id` comes back as an integer. Convert it to character to match join keys and the metadata CSV.
- **Parse speed:** let jsonlite simplify the response in C (about 0.2 s per 25k-row page). Building a list of tibbles took 200+ s.
- **The data comes on a gap-filled hourly grid.** Every year has exactly 8760 or 8784 rows, so row counts don't show how much of the time the station was actually reporting.
- **Caching:** ChartBuilder caches per station in memory for the session only (`reactiveValues`). It deliberately doesn't cache to disk,
  so it doesn't depend on writable storage on Posit Connect.

### The UTC vs local time bug (fixed, worth knowing for fire planning)
The pre-GraphQL app labeled `date`/`hour` in UTC rather than station-local time. At SAGUARO (MST) that shifted everything 7 hours.
Daily means were barely affected, but **anything at a set time of day was badly wrong**. Under the old UTC labels, the "13:00 local" values were:
temperature 58.5 °F (correct: 84.6), RH 50.8% (correct: 21.4), VPD +272% off, Ignition Component +238% off.
Precipitation from 17:00–23:00 local (about 40% of the total) was credited to the next day.
**This app's burn-window and time-of-day logic depends on correct local hours, so keep using `display_hour_lst`.**

---

## 2. R/Shiny namespace hygiene (two real bugs)

1. **Don't call `library()` in files that app.R sources** for packages whose functions share names with shiny's.
   For example, `library(jsonlite)` masked `shiny::validate()`, and every `validate(need(...))` broke.
   Write the package name before the function instead (`jsonlite::fromJSON`, `httr::GET`).
2. **Test helpers can mask shiny functions.** A test helper named `hr()` broke `shiny::hr()` in the UI.
   Start test helper names with a dot (`.hr`). RStudio's *Restart R* **doesn't** clear the global environment
   (it's restored from `.Rproj.user/`). Use `rm(list = ls())`.
   When Shiny reports "unused argument" for a function it exports, run `find("<name>")` first.

---

## 3. Test scripts

- Claude can't run R on Mike's machine. Test scripts in `tests/*.R` write a transcript to `tests/logs/` (gitignored) and state a verdict.
  Run them as `source("tests/test_x.R")` with the .Rproj open. All paths are relative to the project root.
- **Don't use regex to pull text out of rendered Shiny HTML** (emoji or other multibyte text throws off `regmatches()` on Windows).
  Walk the `shiny.tag` tree and read `x$attribs` instead. `grepl(..., fixed = TRUE)` is still safe.
- A test that passes in Claude's Linux container is only a smoke test. The real check is Mike's run in RStudio on Windows.
- To check the UI shape without launching the app, parse `app.R` and evaluate only the `ui <- ...` expression,
  stubbing any missing widgets (and `bslib::bs_theme` if it's used).
- After a surgical rewrite, confirm the untouched regions are byte-identical with `git show HEAD:app.R` instead of just asserting they are.

---

## 4. Project layout (same as ChartBuilder)

```
app.R      must stay at the root (Shiny / Posit Connect)
R/         helper code; also source() it explicitly from app.R
data/      station metadata etc.
www/       logos and static assets (must stay at the root)
tests/     test scripts; tests/logs/ is gitignored
scripts/   one-off processing scripts, not app code
docs/      API guide, notes
archive/   superseded versions
```

---

## 5. Deploy and git

- Posit Connect server: `viz.datascience.arizona.edu`, account `crimmins`.
- **Deploy with `rsconnect::deployApp(appName = "...")` from the console**, not RStudio's Republish button.
  Republish deploys whatever is in the working directory and goes to whichever app you deployed to last.
  Before deploying, be on `main` and confirm `git status --short` is empty. Tag releases.
- **`.rscignore` controls what gets bundled for deploy**; `.gitignore` has no effect on it. Put `.Renviron`, `CLAUDE.md`,
  `tests/`, `archive/`, `docs/`, `scripts/` in it.
- **A new Connect app needs `FEMS_USER` and `FEMS_KEY` set as environment variables in the Connect UI**, or every query returns 401.
- **Keep the repo out of OneDrive.** OneDrive sync plus RStudio file locks broke a `git checkout` partway through
  (`git restore .` recovered it). Close the R session before switching branches.
- Branches: `main` is stable and always deployable, with short-lived feature branches merged back in and deleted.
  Before merging, run `git log --oneline <branch>..main`.
- Mike runs git commands himself in the RStudio terminal. Claude's file bridge can't delete files, so git run through it leaves `.lock` files behind.
- ChartBuilder's own lesson: choose the Connect app name and URL carefully the first time. Renaming later strands anyone holding the old link.

---

## 6. Known debt in ChartBuilder code (don't copy it as-is)

- The daily aggregation chain is copied verbatim in three places (two plots plus the CSV export). Write it once, as a function, in this app.
- The 2005–2025 climatology baseline is hardcoded in five places. Make it one constant.
- `precip_cum` must be computed *before* the month-range filter so accumulation starts on Jan 1.
