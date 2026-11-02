# ==============================================================================
# tests/test_climatology.R -- clim_summarise(), no network
#
#   B  hand-checked shares, means and counts
#   C  smoothing: wraps from Dec into Jan, and equals a brute-force window
#   D  errors and grouping by any column (hour_clock, week)
#   E  a real station (bundled snapshot): speed and internal consistency
#
#     source("tests/test_climatology.R")
# ==============================================================================

local({
  source("tests/helpers.R", local = TRUE)
  .T <- .test_open("test_climatology")

  tryCatch({
    .env_header("clim_summarise (no network)")
    .section("A. Setup")
    .source_core("A")

    .section("B. Hand-checked answers")
    d <- data.frame(year  = c(2020, 2020, 2020, 2021, 2021, 2021, 2021, 2021),
                    month = c(1, 1, 1, 1, 1, 2, 2, 2),
                    hour  = c(13, 13, 14, 13, 13, 13, 13, 13),
                    in_rx = c(TRUE, FALSE, TRUE, TRUE, NA, FALSE, FALSE, NA),
                    temperature = c(50, 60, 70, 80, NA, 30, 40, 50))
    s <- clim_summarise(d, "in_rx", by = c("month", "hour"))
    r <- s[s$month == 1 & s$hour == 13, ]
    .check("B1 Jan 13:00: 2 of 3 valid hours in, 1 missing, 2 years",
           r$n_valid == 3 && r$n_missing == 1 && r$n_years == 2 && abs(r$stat - 2 / 3) < 1e-12,
           sprintf("valid %d, missing %d, years %d, share %.3f", r$n_valid, r$n_missing, r$n_years, r$stat))
    r <- s[s$month == 2 & s$hour == 13, ]
    .check("B2 Feb 13:00: 0 of 2 in, 1 missing, 1 year", r$n_valid == 2 && r$n_missing == 1 && r$n_years == 1 && r$stat == 0)
    .check("B3 kind is 'share' for a logical value", all(s$kind == "share"))
    m <- clim_summarise(d, "temperature", by = "month")
    .check("B4 mean of a numeric value, ignoring NA (Jan 65, Feb 40)",
           identical(m$stat, c(65, 40)) && all(m$kind == "mean") && identical(m$n_missing, c(1L, 0L)))
    lg <- clim_summarise(d, c("in_rx", "temperature"), by = "month")
    .check("B5 several values -> long output, one row per group per value",
           nrow(lg) == 4 && setequal(unique(lg$value), c("in_rx", "temperature")))
    .check("B6 rows are sorted by the grouping columns", identical(s$month, sort(s$month)))

    .section("C. Day-of-year smoothing")
    sm <- data.frame(year = rep(2020:2021, each = 365), doy = rep(1:365, 2), hour = 12L)
    sm$in_rx <- sm$doy == 1
    k1 <- clim_summarise(sm, "in_rx", by = c("doy", "hour"), smooth_days = 1)
    st <- function(dd) k1$stat[k1$doy == dd]
    .check("C1 a TRUE on 1 Jan spreads to 31 Dec and 2 Jan with a +/-1 day window (1/3 each)",
           abs(st(365) - 1 / 3) < 1e-12 && abs(st(1) - 1 / 3) < 1e-12 && abs(st(2) - 1 / 3) < 1e-12 && st(3) == 0 && st(364) == 0)
    .check("C2 window counts: 6 hours (3 days x 2 years) per day", all(k1$n_valid == 6))
    set.seed(42)
    rnd <- data.frame(year = sample(2005:2010, 6000, TRUE), doy = sample(1:365, 6000, TRUE), hour = sample(c(6L, 13L), 6000, TRUE))
    rnd$in_rx <- ifelse(runif(6000) < 0.1, NA, runif(6000) < 0.4)
    k7 <- clim_summarise(rnd, "in_rx", by = c("doy", "hour"), smooth_days = 7)
    brute <- function(dd, hh) {
      win <- ((dd - 1 + (-7:7)) %% 365) + 1
      x <- rnd[rnd$hour == hh & rnd$doy %in% win, ]
      c(valid = sum(!is.na(x$in_rx)), true = sum(x$in_rx, na.rm = TRUE), miss = sum(is.na(x$in_rx)),
        years = length(unique(x$year[!is.na(x$in_rx)])))
    }
    probe <- expand.grid(dd = c(1, 2, 7, 8, 180, 358, 359, 365), hh = c(6L, 13L))
    agree <- all(vapply(seq_len(nrow(probe)), function(i) {
      b <- brute(probe$dd[i], probe$hh[i]); r <- k7[k7$doy == probe$dd[i] & k7$hour == probe$hh[i], ]
      r$n_valid == b[["valid"]] && r$n_missing == b[["miss"]] && r$n_years == b[["years"]] &&
        abs(r$stat - b[["true"]] / b[["valid"]]) < 1e-12
    }, logical(1)))
    .check("C3 +/-7 day smoothing equals a brute-force window (counts, share, years), incl. across New Year", agree)

    .section("D. Errors and other groupings")
    e <- tryCatch(clim_summarise(d, "in_rx", by = c("month", "hour"), smooth_days = 7), error = function(e) conditionMessage(e))
    .check("D1 smoothing by month is an error, not a silent no-op", is.character(e) && grepl("doy", e, fixed = TRUE))
    e2 <- tryCatch(clim_summarise(d, "nope"), error = function(e) conditionMessage(e))
    .check("D2 an unknown column is an error", is.character(e2) && grepl("nope", e2, fixed = TRUE))
    d$hour_clock <- d$hour + 1L
    hc <- clim_summarise(d, "in_rx", by = c("month", "hour_clock"))
    .check("D3 grouping by hour_clock works (the clock-time toggle)", "hour_clock" %in% names(hc) && nrow(hc) == 3)

    .section("E. A real station")
    mf <- hourly_bundled_manifest()
    if (!nrow(mf)) {
      .info("E0 skipped", "no bundled snapshots")
    } else {
      st <- hourly_load_station(mf$station_id[1])
      b <- hourly_baseline(st$hourly)
      fl <- rx_flag(b, rx_new(temperature = c(40, 85), relativeHumidity = c(20, 55), windSpeed = c(2, 15)))
      t1 <- .run("month x hour", clim_summarise(fl, c("in_rx", "pass_temperature", "pass_relativeHumidity", "pass_windSpeed"),
                                                by = c("month", "hour")))
      t2 <- .run("doy x hour, smoothed", clim_summarise(fl, "in_rx", by = c("doy", "hour"), smooth_days = CLIM_SMOOTH_DAYS))
      mh <- t1$value; dh <- t2$value
      .check("E1 month x hour: 288 cells per value", nrow(mh) == 288 * 4, paste(nrow(mh), "rows"))
      .check("E2 counts add up to the baseline hours", sum(mh$n_valid[mh$value == "in_rx"] + mh$n_missing[mh$value == "in_rx"]) == nrow(fl))
      .check("E3 shares are in [0, 1]", all(mh$stat >= 0 & mh$stat <= 1, na.rm = TRUE))
      .check("E4 doy x hour smoothed: 365 x 24 cells", nrow(dh) == 365 * 24)
      .check("E5 both run in under 5 s", t1$secs < 5 && t2$secs < 5, paste(t1$secs, "s and", t2$secs, "s"))
      ir <- mh[mh$value == "in_rx", ]; best <- ir[which.max(ir$stat), ]
      .info("E6 example", sprintf("%s: best cell month %d hour %02d at %.0f%% of %d hours (%d years)",
                                  st$meta$name, best$month, best$hour, 100 * best$stat, best$n_valid, best$n_years))
    }
  }, error = function(e) {
    .T$abort <- paste("test aborted:", conditionMessage(e))
    .say(""); .say("ABORTED: ", conditionMessage(e))
  })
  .test_finish()
})
