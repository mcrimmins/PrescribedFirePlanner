# ==============================================================================
# tests/test_tooltips.R -- hover text, no network, no browser
#
# Checks the formatters and the heatmap-cell tooltips by plain string
# matching (grepl(fixed = TRUE); never regex on rendered HTML), on a
# hand-made summary and on a real bundled station.
#
#     source("tests/test_tooltips.R")
# ==============================================================================

local({
  source("tests/helpers.R", local = TRUE)
  .T <- .test_open("test_tooltips")
  has <- function(x, s) all(grepl(s, x, fixed = TRUE))

  tryCatch({
    .env_header("Tooltips (no network)")
    .section("A. Setup")
    .source_core("A")
    r <- .run("source R/tooltips.R", source("R/tooltips.R"))
    .check("A4 R/tooltips.R loads", r$ok)

    .section("B. Formatters")
    .check("B1 tip_pct: <1%, 64%, 100%, n/a", identical(tip_pct(c(0.003, 0.64, 1, NA)), c("<1%", "64%", "100%", "n/a")))
    .check("B2 tip_hours: singular, thousands separator", identical(tip_hours(c(1, 1234)), c("1 hour", "1,234 hours")))
    .check("B3 tip_hour_label: LST and clock time",
           identical(tip_hour_label(c(13, 5)), c("13:00 LST", "05:00 LST")) && tip_hour_label(13, TRUE) == "13:00 clock time")
    .check("B4 tip_value rounds and adds units",
           tip_value("temperature", 77.6) == "78 \u00b0F" && tip_value("relativeHumidity", 21.6) == "22%" &&
             tip_value("windSpeed", 6.2) == "6 mph" && tip_value("precipitation", 0.034) == "0.03 in/hr" &&
             tip_value("temperature", NA) == "n/a")

    .section("C. Cell tooltips on a hand-made summary")
    d <- data.frame(year = c(2020, 2020, 2021, 2021, 2020, 2021),
                    month = c(1, 1, 1, 1, 2, 2), hour = 13L, hour_clock = 14L,
                    temperature = c(50, 60, 70, NA, 80, 80), relativeHumidity = c(30, 40, 60, 30, 20, 20),
                    windSpeed = 5, windDirection = 200, precipitation = 0, solarRadiation = 500)
    fl <- rx_flag(d, rx_new(temperature = c(40, 85), relativeHumidity = c(25, 55)))
    vals <- c("in_rx", "pass_temperature", "pass_relativeHumidity", "temperature", "relativeHumidity")
    long <- clim_summarise(fl, vals, by = c("month", "hour"))
    tc <- tip_cells(long, "month", "hour", c("temperature", "relativeHumidity"), obs_minute = 8)
    jan <- tc$text[tc$month == 1]; feb <- tc$text[tc$month == 2]
    .say("    January tooltip: ", jan)
    .check("C1 names the cell: month, hour, LST and the reading minute", has(jan, "<b>January, 13:00 LST (read at :08)</b>"))
    .check("C2 share, hours and years: 2 of 3 valid hours, 2 years", has(jan, "In prescription: <b>67%</b> of 3 hours (2 years)"))
    .check("C3 excluded hours are counted", has(jan, "Excluded: 1 hour (missing or screened)"))
    .check("C4 names the limit failed most often", has(jan, "Fails most often on: RH (25% of hours)"))
    .check("C5 typical values carry names and units", has(jan, "Typical: 60 \u00b0F, RH 40%"))
    .check("C6 a cell where every limit fails says so", has(feb, "In prescription: <b>0%</b> of 2 hours (2 years)") &&
             has(feb, "Fails most often on: RH (100% of hours)"))
    longc <- clim_summarise(fl, vals, by = c("month", "hour_clock"))
    tcc <- tip_cells(longc, "month", "hour_clock", c("temperature", "relativeHumidity"), obs_minute = 8)
    .check("C7 clock-time cells say 'clock time' and drop the LST reading minute",
           has(tcc$text, "14:00 clock time") && !any(grepl("read at", tcc$text, fixed = TRUE)))
    tn <- tip_cells(clim_summarise(rx_flag(d, rx_new()), "in_rx", by = c("month", "hour")), "month", "hour")
    to <- tip_cells(long, "month", "hour", c("temperature", "relativeHumidity"), obs_minute = 8, threshold = 50)
    .check("C7b with a 50% outline: January (67%) says so, February (0%) doesn't",
           has(to$text[to$month == 1], "Outlined: at or above your 50% threshold") &&
             !grepl("Outlined", to$text[to$month == 2], fixed = TRUE))
    .check("C8 no limits: no 'fails' or 'typical' lines", !any(grepl("Fails", tn$text, fixed = TRUE)) &&
             !any(grepl("Typical", tn$text, fixed = TRUE)))

    .section("D. A real station")
    mf <- hourly_bundled_manifest()
    if (!nrow(mf)) {
      .info("D0 skipped", "no bundled snapshots")
    } else {
      st <- hourly_load_station(mf$station_id[1])
      b <- hourly_baseline(st$hourly)
      rx <- rx_new(temperature = c(40, 85), relativeHumidity = c(20, 55), windSpeed = c(2, 15))
      fl <- rx_flag(b, rx)
      vars <- rx_constrained(rx)
      long <- clim_summarise(fl, c("in_rx", paste0("pass_", vars), vars), by = c("month", "hour"))
      tm <- .run("tip_cells for 288 cells", tip_cells(long, "month", "hour", vars, obs_minute = 8))
      tc <- tm$value
      .check("D1 one tooltip per cell (288)", nrow(tc) == 288)
      .check("D2 every tooltip states the share, the hours and the years",
             all(grepl("In prescription: <b>", tc$text, fixed = TRUE)) && all(grepl(" years)", tc$text, fixed = TRUE)))
      .check("D3 no raw NA or NaN leaks into the text", !any(grepl("NA", tc$text, fixed = TRUE)) && !any(grepl("NaN", tc$text, fixed = TRUE)))
      .check("D4 builds in under 2 s", tm$secs < 2, paste(tm$secs, "s"))
      .say("    example: ", tc$text[tc$month == 4 & tc$hour == 14])
    }

    .section("E. Map markers")
    ts <- tip_station("21202", "SAGUARO", 2264, "AZ", featured = TRUE, bundled = TRUE, note = "Sonoran Desert")
    .check("E1 station tooltip: name, id, state, elevation, bundled, note",
           has(ts, "<b>SAGUARO</b> (21202), AZ") && has(ts, "2,264 ft") && has(ts, "loads instantly") && has(ts, "Sonoran Desert"))
    .check("E2 an unbundled station says it downloads", has(tip_station("1", "X", 10), "Downloads from FEMS"))
  }, error = function(e) {
    .T$abort <- paste("test aborted:", conditionMessage(e))
    .say(""); .say("ABORTED: ", conditionMessage(e))
  })
  .test_finish()
})
