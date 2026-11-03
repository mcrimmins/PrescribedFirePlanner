# ==============================================================================
# tests/test_totals.R -- hour totals (R/totals.R), no network
#
#   B  hand-built counts: median year, named range, total, the coverage rule
#   C  display tables
#   D  the CSV: header lines, reads back, numbers survive the round trip
#   E  a real bundled station: totals agree with the heatmap's own counts
#
#     source("tests/test_totals.R")
# ==============================================================================

local({
  source("tests/helpers.R", local = TRUE)
  .T <- .test_open("test_totals")

  tryCatch({
    .env_header("Hour totals (no network)")
    .section("A. Setup")
    .source_core("A")

    .section("B. Hand-built counts")
    # Jan, 100 hours a year, 2005-2009; TRUE hours 10, 20, 30, 40, 50.
    # 2009 has 30 of its hours missing (70% usable < 80%): out of the median/range.
    mk <- function(year, n_true, n_na, month = 1, n = 100)
      data.frame(year = year, month = month,
                 in_rx = c(rep(TRUE, n_true), rep(FALSE, n - n_true - n_na), rep(NA, n_na)))
    fl <- rbind(mk(2005, 10, 0), mk(2006, 20, 0), mk(2007, 30, 0), mk(2008, 40, 0), mk(2009, 50, 30),
                mk(2005, 5, 0, month = 2))
    py <- totals_per_year(fl, character(0))
    sm <- totals_summary(py)
    j <- sm[sm$period == "Jan" & sm$value == "in_rx", ]
    .check("B1 typical year = median of the years used (20, 30 -> 25)", j$median == 25, j$median)
    .check("B2 the range names its years: 10 h in 2005, 40 h in 2008", j$low == 10 && j$low_year == 2005 && j$high == 40 && j$high_year == 2008,
           sprintf("%g (%d) - %g (%d)", j$low, j$low_year, j$high, j$high_year))
    .check("B3 total counts every year, including the one left out (150)", j$total == 150)
    .check("B4 the 70%-usable year is left out and listed; its missing hours are counted",
           j$years_used == 4 && j$years_left_out == "2009" && j$hours_excluded == 30)
    .check("B5 a month with one year: median = low = high = that year (Feb, 5 h in 2005)",
           with(sm[sm$period == "Feb", ], median == 5 && low == 5 && high == 5 && low_year == 2005))
    y <- sm[sm$period == "Year", ]
    .check("B6 the Year row: per-year sums across months (2005: 15 h) and an all-years total (155)",
           y$total == 155 && py$n_true[py$period == "Year" & py$year == 2005] == 15)
    .check("B7 periods are in calendar order, Year last", identical(unique(as.character(sm$period)), c("Jan", "Feb", "Year")))
    tie <- totals_summary(totals_per_year(rbind(mk(2010, 7, 0), mk(2011, 7, 0)), character(0)))
    .check("B8 ties name the earliest year", tie$low_year[1] == 2010 && tie$high_year[1] == 2010)

    .section("C. Display tables")
    wx <- .fake_wx(seq(as.Date("2005-01-01"), as.Date("2006-12-31"), by = "day"))
    h <- hourly_add_calendar(hourly_from_fems(wx), "America/Phoenix")
    rx <- rx_new(temperature = c(NA, 80), relativeHumidity = c(25, NA))
    f <- rx_flag(h, rx); vars <- rx_constrained(rx)
    s2 <- totals_summary(totals_per_year(f, vars))
    dsp <- totals_display(s2, vars)
    .check("C1 main table: 12 months + Year, 6 columns", nrow(dsp$main) == 13 && ncol(dsp$main) == 6,
           paste(names(dsp$main), collapse = " | "))
    .check("C2 by-limit table: a 'met' and an 'only one failing' row per limit, months + Year across",
           nrow(dsp$by_limit) == 2 * length(vars) && identical(names(dsp$by_limit), c("Limit", "Hours", month.abb, "Year")),
           paste(names(dsp$by_limit), collapse = " | "))
    .check("C3 range cell reads like '64 (2005) to 171 (2006)'",
           grepl("^[0-9,]+ \\([0-9]{4}\\) to [0-9,]+ \\([0-9]{4}\\)$", dsp$main[[3]][1]), dsp$main[[3]][1])
    .check("C4 display tables are plain ASCII (no encoding surprises in the browser or PDF)",
           all(!grepl("[^ -~]", c(names(dsp$main), unlist(dsp$main), names(dsp$by_limit)))))

    .section("D. CSV")
    meta <- list(name = "TEST STATION", station_id = "99999", state = "AZ")
    ln <- totals_csv_lines(s2, meta, "test data", rx, c(2005, 2006), "local standard time (LST)", "summary")
    .check("D1 header lines start with #, then a CSV header", all(startsWith(ln[1:8], "#")) && startsWith(ln[9], "\"period\""))
    .check("D2 the header states station, prescription, years and the coverage rule",
           any(grepl("TEST STATION (99999), AZ", ln, fixed = TRUE)) &&
             any(grepl("Temperature up to 80", ln, fixed = TRUE)) && any(grepl("Relative humidity at least 25", ln, fixed = TRUE)) &&
             any(grepl("Years: 2005-2006", ln, fixed = TRUE)) && any(grepl(paste0(TOTALS_COVERAGE_MIN, "%"), ln, fixed = TRUE)))
    tf <- tempfile(fileext = ".csv"); writeLines(ln, tf)
    back <- utils::read.csv(tf, comment.char = "#", stringsAsFactors = FALSE)
    .check("D3 reads back with read.csv(comment.char = '#'): same rows and totals",
           nrow(back) == nrow(s2) && all(back$total_h == s2$total) && all(back$measure == s2$value))
    ly <- totals_csv_lines(totals_per_year(f, vars), meta, "test data", rx, c(2005, 2006), "LST", "per_year")
    by <- utils::read.csv(textConnection(ly), comment.char = "#", stringsAsFactors = FALSE)
    .check("D4 per-year CSV has year, period, measure, hours and in_median columns",
           all(c("year", "period", "measure", "hours_true", "hours_usable", "in_median") %in% names(by)))
    .check("D5 file names are tidy", grepl("^rxwx_test-station_climatology_totals_[0-9]{8}\\.csv$", totals_csv_name(meta)),
           totals_csv_name(meta))
    .check("D6b a no-rain limit reads 'Precipitation none (0 in/hr)'",
           grepl("Precipitation none (0 in/hr)", totals_rx_text(rx_new(precipitation = c(NA, 0))), fixed = TRUE))
    .check("D6 a wrapping wind sector is described clockwise",
           grepl("from 315 to 45, clockwise", totals_rx_text(rx_new(windDirection = c(315, 45))), fixed = TRUE))

    .section("E. A real station")
    mf <- hourly_bundled_manifest()
    if (!nrow(mf)) {
      .info("E0 skipped", "no bundled snapshots")
    } else {
      st <- hourly_load_station(mf$station_id[1])
      ex <- RX_EXAMPLE[RX_EXAMPLE$on, ]
      rxe <- rx_validate(data.frame(var = ex$var, min = ex$min, max = ex$max, stringsAsFactors = FALSE))
      fl <- rx_flag(hourly_baseline(st$hourly), rxe); vars <- rx_constrained(rxe)
      t0 <- Sys.time(); py <- totals_per_year(fl, vars); sm <- totals_summary(py)
      .info("E1 station", sprintf("%s: %d years, %.1f s", st$meta$name, length(unique(fl$year)),
                                  as.numeric(difftime(Sys.time(), t0, units = "secs"))))
      yr <- sm[sm$period == "Year" & sm$value == "in_rx", ]
      .check("E2 the all-years total equals the in-prescription hours in the flagged table",
             yr$total == sum(fl$in_rx, na.rm = TRUE), sprintf("%s h", format(yr$total, big.mark = ",")))
      mo <- sm[sm$period %in% month.abb & sm$value == "in_rx", ]
      .check("E3 the 12 month totals add up to the year total", sum(mo$total) == yr$total)
      hm <- clim_summarise(fl, "in_rx", by = c("month", "hour"))
      .check("E4 ... and to the heatmap's own counts (month x hour n_true)", sum(hm$n_true) == yr$total)
      .check("E5 low <= median <= high wherever a median exists",
             all(sm$low <= sm$median & sm$median <= sm$high, na.rm = TRUE))
      for (v in vars) {
        p <- sm$total[sm$period == "Year" & sm$value == paste0("pass_", v)]
        o <- sm$total[sm$period == "Year" & sm$value == paste0("only_", v)]
        .check(paste0("E6 ", v, ": in-prescription <= limit met; only-this-failing <= hours out"),
               yr$total <= p && o <= sum(!fl$in_rx, na.rm = TRUE),
               sprintf("in %s, met %s, only failing %s", yr$total, p, o))
      }
      d <- totals_display(sm, vars)
      .say("    Main table:")
      for (k in seq_len(nrow(d$main))) .say("      ", paste(unlist(d$main[k, ]), collapse = " | "))
      .say("    By limit (median year):")
      .say("      ", paste(names(d$by_limit), collapse = " | "))
      for (k in seq_len(nrow(d$by_limit))) .say("      ", paste(unlist(d$by_limit[k, ]), collapse = " | "))
    }

    .section("F. Past event: totals by day")
    vars <- rx_constrained(rx)    # section E reused the name for the bundled station
    e3 <- f[f$date >= as.Date("2005-03-10") & f$date <= as.Date("2005-03-12"), ]
    te <- totals_event(e3, vars)
    ir <- te[te$value == "in_rx", ]
    .check("F1 one row per day per measure, plus Total rows", nrow(te) == (3 + 1) * length(totals_measures(vars)) &&
             identical(ir$day, c("2005-03-10", "2005-03-11", "2005-03-12", "Total")))
    .check("F2 daily in-prescription hours match the flagged table, and Total is their sum",
           all(ir$n_true[1:3] == tapply(e3$in_rx, e3$date, sum, na.rm = TRUE)) && ir$n_true[4] == sum(e3$in_rx, na.rm = TRUE))
    .check("F3 the longest run matches clim_runs, and Total gives the longest single day",
           all(ir$longest_run[1:3] == clim_runs(e3)$longest) && ir$longest_run[4] == max(ir$longest_run[1:3]))
    de <- totals_event_display(te, vars)
    .check("F4 display: a row per day + Total; runs read like '5 h, 10:00-15:00' or 'none'",
           nrow(de$main) == 4 && all(grepl("^([0-9]+ h, [0-9]{2}:00-[0-9]{2}:00|none|[0-9]+ h \\(longest day\\))$", de$main[[3]])),
           paste(de$main[[3]], collapse = " | "))
    .check("F5 by-limit table: days + Total across", identical(names(de$by_limit), c("Limit", "Hours", "10 Mar", "11 Mar", "12 Mar", "Total")),
           paste(names(de$by_limit), collapse = " | "))
    ld <- totals_event_csv_lines(te, meta, "test data", rx, as.Date(c("2005-03-10", "2005-03-12")), "LST", "daily")
    bd <- utils::read.csv(textConnection(ld), comment.char = "#", stringsAsFactors = FALSE)
    .check("F6 daily CSV reads back, with the dates in the header", nrow(bd) == nrow(te) &&
             any(grepl("Dates: 2005-03-10 to 2005-03-12", ld, fixed = TRUE)) && "longest_run_h" %in% names(bd))
    lh <- totals_event_csv_lines(e3, meta, "test data", rx, as.Date(c("2005-03-10", "2005-03-12")), "LST", "hourly")
    bh <- utils::read.csv(textConnection(lh), comment.char = "#", stringsAsFactors = FALSE)
    .check("F7 hourly CSV: 72 hours, with in_rx, pass_ and only_ columns and the values",
           nrow(bh) == 72 && all(c("date", "hour_lst", "in_rx", "pass_temperature", "only_relativeHumidity", "temperature") %in% names(bh)))
    .check("F8 file names carry the tab", grepl("_past-event_daily_", totals_csv_name(meta, "daily", tab = "past-event"), fixed = TRUE))
  }, error = function(e) {
    .T$abort <- paste("test aborted:", conditionMessage(e))
    .say(""); .say("ABORTED: ", conditionMessage(e))
  })
  .test_finish()
})
