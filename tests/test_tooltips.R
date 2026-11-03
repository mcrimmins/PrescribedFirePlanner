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

    .section("D1. Day-of-year cells")
    ldoy <- data.frame(doy = c(110L, 110L), hour = 10L, value = c("in_rx", "pass_temperature"), kind = "share",
                       n_valid = 315L, n_missing = 0L, n_years = 21L, n_true = c(130, 300), stat = c(130, 300) / 315)
    td <- tip_cells(ldoy, x_col = "doy", hour_col = "hour", rx_vars = "temperature", obs_minute = 24,
                    sun = data.frame(doy = 110L, sunrise = 5.3, sunset = 18.75))
    .check("D1.1 a day-of-year cell names the date and the smoothing window",
           grepl("20 Apr (\u00b17 days), 10:00 LST (read at :24)", td$text, fixed = TRUE), td$text)
    .check("D1.2 ... and that day's sunrise and sunset", grepl("Sunrise 05:18, sunset 18:45 LST", td$text, fixed = TRUE))

    .section("D1b. By-variable and daily-range tooltips")
    lv <- data.frame(doy = 110L, hour = 10L, value = c("relativeHumidity", "pass_relativeHumidity"), kind = c("mean", "share"),
                     n_valid = 315L, n_missing = 0L, n_years = 21L, n_true = c(NA, 195), stat = c(38.2, 195 / 315))
    tv <- tip_var_cells(lv, "relativeHumidity", lo = 20, hi = 45)
    .check("D1b.1 a by-variable cell gives the mean and the share within that limit",
           grepl("Mean RH: <b>38%</b>", tv$text, fixed = TRUE) && grepl("within your limit (20% to 45%): <b>62%</b> of 315 hours", tv$text, fixed = TRUE),
           tv$text)
    .check("D1b.2 a no-rain limit reads 'no rain'", .tip_limit("precipitation", NA, 0) == "no rain")
    qv <- data.frame(doy = 219L, value = "relativeHumidity", n = 2520L, q10 = 15, q25 = 23, q50 = 32, q75 = 45, q90 = 62)
    tr <- tip_daily_range(qv, "relativeHumidity", pass = 0.59, hours_label = "hours 09:00-17:00 clock time")
    .check("D1b.3 a daily-range tooltip: median, middle half, 8 in 10, share within the limit",
           grepl("Median RH: <b>32%</b>", tr, fixed = TRUE) && grepl("Middle half of hours: 23% to 45%", tr, fixed = TRUE) &&
             grepl("8 in 10 hours: 15% to 62%", tr, fixed = TRUE) && grepl("Within your limit: <b>59%</b> of 2,520 hours", tr, fixed = TRUE), tr)

    .section("D2. Past event hour tooltips")
    eh <- data.frame(date = as.Date("2024-03-12"), hour = c(14L, 15L, 16L, 17L), hour_clock = c(15L, 16L, 17L, 18L),
                     time_clock = as.POSIXct("2024-03-12 15:00", tz = "America/Denver") + 0:3 * 3600,
                     temperature = c(70, 90, 70, NA), relativeHumidity = c(30, 12, 30, 30), windSpeed = c(5, 5, 0, 5),
                     windDirection = c(225, 225, 90, 10), precipitation = c(0, 0, 0.05, 0))
    rxe <- rx_new(temperature = c(40, 85), relativeHumidity = c(20, 55))
    te <- tip_event_hours(rx_flag(eh, rxe), rxe, obs_minute = 8)
    .check("D2.1 time in LST with the observation minute", grepl("Tue 12 Mar 2024, 14:00 LST (read at :08)", te[1], fixed = TRUE), te[1])
    .check("D2.2 an in-prescription hour says so", grepl("<b>In prescription</b>", te[1], fixed = TRUE))
    .check("D2.3 an out hour names each failing limit with its value and limit",
           grepl("temperature 90 \u00b0F (limit 40 \u00b0F to 85 \u00b0F)", te[2], fixed = TRUE) &&
             grepl("RH 12% (limit 20% to 55%)", te[2], fixed = TRUE), te[2])
    .check("D2.4 calm is labelled; rain is shown when it fell", grepl("(calm)", te[3], fixed = TRUE) && grepl("Rain 0.05 in/hr", te[3], fixed = TRUE))
    .check("D2.5 a missing limited value: 'Not known: no data for temperature'", grepl("<b>Not known</b>: no data for temperature", te[4], fixed = TRUE), te[4])
    .check("D2.6 wind direction in compass words", grepl("from SW (225\u00b0)", te[1], fixed = TRUE) && tip_compass(350) == "N" && tip_compass(10) == "N")
    tcl <- tip_event_hours(rx_flag(eh, rxe), rxe, clock = TRUE)
    .check("D2.7 clock-time labels", grepl("15:00 clock time", tcl[1], fixed = TRUE), tcl[1])

    .section("D3. Same dates day tooltips")
    dd <- data.frame(date = as.Date(c("2011-03-27", "2011-03-28")), n_true = c(14, 0), n_valid = c(24, 20), n_missing = c(0, 4),
                     longest = c(5L, 0L), start_hour = c(3L, NA), usable = c(TRUE, FALSE), held = c("relativeHumidity", NA),
                     held_h = c(7, NA))
    tw <- tip_window_days(dd, 4)
    .check("D3.1 date, hours in prescription and the run", grepl("Sun 27 Mar 2011", tw[1], fixed = TRUE) &&
             grepl("<b>14</b> of 24 hours with data", tw[1], fixed = TRUE) && grepl("Longest run 5 h, 03:00-08:00 LST", tw[1], fixed = TRUE), tw[1])
    .check("D3.2 usable days say so; the limit holding the day out is named",
           grepl("a usable day</b> (4+ h)", tw[1], fixed = TRUE) && grepl("Held out most by RH alone: 7 hours", tw[1], fixed = TRUE))
    .check("D3.3 a day with none in prescription, and missing hours, reads plainly",
           grepl("No hours in prescription", tw[2], fixed = TRUE) && grepl("4 hours not known", tw[2], fixed = TRUE) &&
             !grepl("usable", tw[2], fixed = TRUE), tw[2])

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
