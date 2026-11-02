# ==============================================================================
# tests/test_quality.R -- the Data quality report, no network
#
#   B  step detection on planted series
#   C  report structure on a planted station
#   D  every bundled station: general checks, plus known station-specific
#      findings (a table keyed by station id) for whichever of those stations
#      are bundled. The transcript prints each report in full.
#
#     source("tests/test_quality.R")
# ==============================================================================

local({
  source("tests/helpers.R", local = TRUE)
  .T <- .test_open("test_quality")

  tryCatch({
    .env_header("Data quality report (no network)")
    .section("A. Setup")
    .source_core("A")

    .section("B. Step detection")
    yrs <- 2005:2025
    set.seed(7)
    flat <- 14.7 + rnorm(21, 0, 0.05)
    s1 <- quality_step(yrs, flat + ifelse(yrs >= 2021, 0.5, 0))
    .check("B1 a +0.5 h step from 2021 is found, with the right year", isTRUE(s1$detected) && s1$year == 2021 && abs(s1$step - 0.5) < 0.1,
           sprintf("year %s, step %.2f, spread %.2f", s1$year, s1$step, s1$spread))
    s2 <- quality_step(yrs, flat)
    .check("B2 noise alone is not a step", !isTRUE(s2$detected), sprintf("best split %s, %.2f h", s2$year, s2$step))
    s3 <- quality_step(yrs, flat + ifelse(yrs >= 2015, 0.2, 0))
    .check("B3 a 0.2 h step is below the reporting threshold", !isTRUE(s3$detected))
    s4 <- quality_step(yrs, 14.7 + rnorm(21, 0, 0.4) + ifelse(yrs >= 2015, 0.5, 0))
    .check("B4 a 0.5 h step hidden in 0.4 h of noise is not reported", !isTRUE(s4$detected))
    .check("B5 too few years -> no step", !isTRUE(quality_step(2005:2008, c(1, 1, 2, 2))$detected))

    .section("C. Report structure (planted station)")
    wx <- .fake_wx(seq(as.Date("2005-01-01"), as.Date("2007-12-31"), by = "day"))
    i <- which(wx$date == as.Date("2006-07-12") & wx$hour == 8); wx$relativeHumidity[i:(i + 119)] <- 1
    st <- list(meta = hourly_station_meta("21202"),
               hourly = hourly_qc(hourly_add_calendar(hourly_from_fems(wx), "America/Phoenix")),
               grid_added = 0, source = list(type = "bundled", detail = "test data"))
    st$qc_log <- attr(st$hourly, "qc_log")
    rep <- quality_report(st)
    .check("C1 every item has found, action and affects text",
           all(nzchar(rep$found)) && all(nzchar(rep$action)) && all(nzchar(rep$affects)))
    .check("C2 severity is 'affects' or 'info'", all(rep$severity %in% c("affects", "info")))
    .check("C3 'affects' items come first", identical(rep$severity, sort(rep$severity)))
    .check("C4 the planted stuck RH run is reported", any(grepl("Relative humidity stuck at 1% for 120 h from 12 Jul 2006", rep$found, fixed = TRUE)))
    .check("C5 badge = number of 'affects' items", quality_badge(rep) == sum(rep$severity == "affects"))
    .check("C6 source and observation minute are always reported (info)",
           any(rep$section == "source") && any(grepl("8 minutes past", rep$found, fixed = TRUE)))

    .section("D. The bundled stations")
    # What we already know about particular stations. A row runs only when that
    # station is bundled (findings from the 2 Nov 2026 snapshots; a rebuilt
    # snapshot may move them a little -- the transcript prints every report).
    known <- data.frame(stringsAsFactors = FALSE,
      station_id = c("21202", "21202", "21202", "80802", "80802", "80802", "50507", "44102"),
      expect = c("longest 120 h at 1% from 12 Jul 2008",
                 "Solar radiation's timing shifts about 0.7 h later from 2020",
                 "Temperature's daily cycle shifts about 0.6 h later from 2021",
                 "Temperature went past physical limits",
                 "Solar radiation's timing shifts about 0.5 h later from 2020",
                 "Wind is calm (0 mph) in",
                 "Solar radiation's timing shifts about 0.5 h later from 2020",
                 "Measured precipitation is about 51%"),
      what = c("SAGUARO stuck RH in Jul 2008", "SAGUARO solar step 2020", "SAGUARO temperature step 2021",
               "BLOXHAM failed temperature sensor", "BLOXHAM solar step 2020", "BLOXHAM calm share",
               "ESTES PARK solar step 2020", "CRANE precipitation undercatch"))
    mf <- hourly_bundled_manifest()
    if (!nrow(mf)) {
      .info("D0 skipped", "no bundled snapshots")
    } else {
      reps <- list()
      for (sid in mf$station_id) {
        r <- .run(paste(sid, "load + report"), { s <- hourly_load_station(sid); list(st = s, rep = quality_report(s)) })
        if (!r$ok) { .check(paste("D", sid, "report builds"), FALSE); next }
        reps[[sid]] <- r$value
        rp <- r$value$rep
        .say(sprintf("    %s %s -- badge %d, %d items", sid, r$value$st$meta$name, quality_badge(rp), nrow(rp)))
        for (k in seq_len(nrow(rp))) .say(sprintf("      [%s] %s", rp$severity[k], rp$found[k]))
      }
      .check("D1 every bundled station's report builds", length(reps) == nrow(mf),
             paste(length(reps), "of", nrow(mf)))
      .check("D2 every report: one source item and one observation-minute item",
             all(vapply(reps, function(x) sum(x$rep$section == "source") == 1 &&
                          sum(grepl("minutes past", x$rep$found, fixed = TRUE)) == 1, logical(1))))
      .check("D3 every item has found, action and affects text",
             all(vapply(reps, function(x) all(nzchar(x$rep$found) & nzchar(x$rep$action) & nzchar(x$rep$affects)), logical(1))))
      .check("D4 every badge is at most 15 (grouped, not one item per run)",
             all(vapply(reps, function(x) quality_badge(x$rep) <= 15, logical(1))),
             paste(names(reps), vapply(reps, function(x) quality_badge(x$rep), numeric(1)), sep = "=", collapse = ", "))
      .check("D5 after screening, every station's temperatures are within -40..130 F",
             all(vapply(reps, function(x) all(x$st$hourly$temperature >= -40 & x$st$hourly$temperature <= 130, na.rm = TRUE), logical(1))))
      k <- known[known$station_id %in% names(reps), ]
      if (!nrow(k)) .info("D6 station-specific findings", "none recorded yet for the bundled stations")
      for (i in seq_len(nrow(k)))
        .check(paste0("D6.", i, " ", k$what[i]),
               any(grepl(k$expect[i], reps[[k$station_id[i]]]$rep$found, fixed = TRUE)))
    }
  }, error = function(e) {
    .T$abort <- paste("test aborted:", conditionMessage(e))
    .say(""); .say("ABORTED: ", conditionMessage(e))
  })
  .test_finish()
})
