# ==============================================================================
# tests/test_hourly.R -- the hourly layer, no network
#
#   B  grid completion (absent hours become NA rows)
#   C  calendar and clock time (Feb 29, week 52, DST changeovers)
#   D  screening: each rule fires on a planted fault and leaves real-looking
#      weather alone
#   E  the contract (hourly_problems / hourly_validate)
#   F  bundled snapshots: read, checksum guard, and the full pipeline on the
#      real files in data/stations_bundled (skipped if they're absent)
#
#     source("tests/test_hourly.R")
# ==============================================================================

local({
  source("tests/helpers.R", local = TRUE)
  .T <- .test_open("test_hourly")

  tryCatch({
    .env_header("Hourly layer (no network)")
    .section("A. Setup")
    .source_core("A")

    # ---- B. grid completion ------------------------------------------------------
    .section("B. Grid completion")
    wx <- .fake_wx(seq(as.Date("2024-03-01"), as.Date("2024-03-05"), by = "day"))
    full <- hourly_from_fems(wx)
    .check("B1 complete input is unchanged (120 rows, nothing added)",
           nrow(full) == 120 && attr(full, "grid_added") == 0)
    drop <- with(wx, (date == as.Date("2024-03-02") & hour == 8) |                    # one hour
                     (date == as.Date("2024-03-03") & hour %in% 6:17) |               # half a day
                     (date == as.Date("2024-03-04") & hour == 0))                      # a midnight
    g <- hourly_from_fems(wx[!drop, ])
    .check("B2 absent hours come back as rows (14 added, 120 rows)", nrow(g) == 120 && attr(g, "grid_added") == 14,
           paste(nrow(g), "rows,", attr(g, "grid_added"), "added"))
    added <- g[g$date == as.Date("2024-03-03") & g$hour %in% 6:17, ]
    .check("B3 added rows are NA for every value and obs_minute",
           all(is.na(added$temperature)) && all(is.na(added$windSpeed)) && all(is.na(added$obs_minute)))
    key <- as.integer(g$date) * 24L + g$hour
    .check("B4 rows are consecutive hours", all(diff(key) == 1L))
    f <- wx; f$record_type[1:3] <- "F"
    .check("B5 forecast (F) rows are dropped, then refilled as missing hours",
           nrow(hourly_from_fems(f)) == 120 && attr(hourly_from_fems(f), "grid_added") == 3)
    e1 <- tryCatch(hourly_from_fems(rbind(wx, wx[1, ])), error = function(e) conditionMessage(e))
    .check("B6 duplicate hours are an error", is.character(e1) && grepl("duplicate", e1, fixed = TRUE))
    w2 <- wx; w2$station_id[1] <- "99999"
    e2 <- tryCatch(hourly_from_fems(w2), error = function(e) conditionMessage(e))
    .check("B7 two stations in one table are an error", is.character(e2) && grepl("one station", e2, fixed = TRUE))

    # ---- C. calendar and clock -----------------------------------------------------
    .section("C. Calendar and clock time")
    dts <- as.Date(c("2023-03-01", "2024-02-28", "2024-02-29", "2024-03-01", "2024-12-31", "2023-12-31"))
    c1 <- hourly_add_calendar(hourly_from_fems(.fake_wx(dts[1])), "America/Phoenix")
    mk <- function(d) hourly_add_calendar(hourly_from_fems(.fake_wx(d)), "America/Phoenix")
    dv <- vapply(dts, function(d) mk(d)$doy[1], integer(1))
    .check("C1 doy on a 365-day calendar (Feb 29 -> Feb 28)", identical(dv, c(60L, 59L, 59L, 60L, 365L, 365L)),
           paste(dts, dv, sep = "=", collapse = ", "))
    .check("C2 week of 31 Dec is 52", mk(as.Date("2024-12-31"))$week[1] == 52L)
    az <- hourly_add_calendar(hourly_from_fems(.fake_wx(as.Date(c("2024-01-15", "2024-07-15")))), "America/Phoenix")
    .check("C3 Arizona: clock hour = LST hour all year", identical(az$hour_clock, az$hour))
    den <- hourly_add_calendar(hourly_from_fems(.fake_wx(seq(as.Date("2024-03-09"), as.Date("2024-03-11"), by = "day"),
                                                         "50507")), "America/Denver")
    at <- function(h, d, hr) h$hour_clock[h$date == as.Date(d) & h$hour == hr]
    .check("C4 Denver winter: 13:00 LST is 13 on the clock", at(den, "2024-03-09", 13) == 13)
    .check("C5 Denver, DST starts 10 Mar 2024: 01:00 LST -> 1, 02:00 LST -> 3 on the clock",
           at(den, "2024-03-10", 1) == 1 && at(den, "2024-03-10", 2) == 3)
    .check("C6 Denver summer side: 13:00 LST on 11 Mar is 14 on the clock", at(den, "2024-03-11", 13) == 14)
    nov <- hourly_add_calendar(hourly_from_fems(.fake_wx(as.Date(c("2024-11-02", "2024-11-03")), "50507")),
                               "America/Denver")
    .check("C7 Denver, DST ends 3 Nov 2024: 00:00 LST -> 1, 01:00 LST -> 1 on the clock",
           at(nov, "2024-11-03", 0) == 1 && at(nov, "2024-11-03", 1) == 1)
    .check("C8 LST date/hour are untouched by the clock columns",
           identical(den$hour, rep(0:23, 3)) && inherits(den$time_clock, "POSIXct"))

    # ---- D. screening -----------------------------------------------------------------
    .section("D. Screening rules")
    days <- seq(as.Date("2008-07-01"), as.Date("2008-07-31"), by = "day")
    base <- hourly_add_calendar(hourly_from_fems(.fake_wx(days)), "America/Phoenix")
    at_i <- function(h, d, hr) which(h$date == as.Date(d) & h$hour == hr)
    p <- base
    i <- at_i(p, "2008-07-02", 8); p$relativeHumidity[i:(i + 119)] <- 1          # stuck RH 120 h (SAGUARO 2008)
    i <- at_i(p, "2008-07-08", 0); p$relativeHumidity[i:(i + 29)] <- 100        # fog: 30 h at 100%
    i <- at_i(p, "2008-07-10", 0); p$temperature[i:(i + 22)] <- 55              # 23 h constant: kept
    i <- at_i(p, "2008-07-12", 14); p$temperature[i] <- p$temperature[i] + 25    # single spike
    i <- at_i(p, "2008-07-13", 15); p$temperature[i:(i + 5)] <- p$temperature[i:(i + 5)] - 25  # outflow: stays cool
    i <- at_i(p, "2008-07-15", 0); p$windSpeed[i:(i + 22)] <- 0                  # 23 h calm: kept
    i <- at_i(p, "2008-07-17", 0); p$windSpeed[i:(i + 29)] <- 0                  # 30 h calm: removed
    i <- at_i(p, "2008-07-20", 13); p$temperature[i] <- 140                      # past the limit
    i <- at_i(p, "2008-07-24", 9);  p$solarRadiation[i:(i + 5)] <- 640           # 6 identical daylight hours
    i <- at_i(p, "2008-07-26", 9);  p$solarRadiation[i:(i + 4)] <- 640           # 5: kept
    q <- hourly_qc(p); lg <- attr(q, "qc_log")
    .say("    screening log:"); .show(lg)
    has <- function(rule, var) any(lg$rule == rule & lg$variable == var)
    i <- at_i(q, "2008-07-02", 8)
    .check("D1 RH stuck at 1% for 120 h is removed and logged",
           all(is.na(q$relativeHumidity[i:(i + 119)])) &&
             any(lg$rule == "stuck" & lg$variable == "relativeHumidity" & lg$hours == 120 & lg$value == 1))
    i <- at_i(q, "2008-07-08", 0)
    .check("D2 fog (RH 100% for 30 h) is kept", all(q$relativeHumidity[i:(i + 29)] == 100))
    i <- at_i(q, "2008-07-10", 0)
    .check("D3 temperature constant for 23 h is kept", all(q$temperature[i:(i + 22)] == 55))
    .check("D4 a one-hour temperature spike is removed", is.na(q$temperature[at_i(q, "2008-07-12", 14)]) && has("spike", "temperature"))
    i <- at_i(q, "2008-07-13", 15)
    .check("D5 an outflow drop that persists is kept", all(!is.na(q$temperature[i:(i + 5)])))
    i <- at_i(q, "2008-07-15", 0)
    .check("D6 calm for 23 h is kept", all(q$windSpeed[i:(i + 22)] == 0))
    i <- at_i(q, "2008-07-17", 0)
    .check("D7 calm for 30 h removes wind speed, gust and direction",
           all(is.na(q$windSpeed[i:(i + 29)])) && all(is.na(q$gustSpeed[i:(i + 29)])) &&
             all(is.na(q$windDirection[i:(i + 29)])) && has("long calm", "windSpeed"))
    lim <- q$date %in% as.Date(c("2008-07-19", "2008-07-20", "2008-07-21"))
    .check("D8 140 F removes temperature for that day and one day either side (72 h)",
           all(is.na(q$temperature[lim])) && any(lg$rule == "limits" & lg$variable == "temperature" & lg$hours == 72),
           paste(sum(is.na(q$temperature[lim])), "of 72 removed"))
    .check("D9 the rest of the temperature record is intact", sum(!is.na(q$temperature[!lim])) >= sum(!lim) - 2)
    .check("D10 dewpoint follows removed temperature", all(is.na(q$dewpoint[lim])))
    i <- at_i(q, "2008-07-24", 9)
    .check("D11 6 identical daylight solar readings are removed", all(is.na(q$solarRadiation[i:(i + 5)])))
    i <- at_i(q, "2008-07-26", 9)
    .check("D12 5 identical daylight solar readings are kept", all(q$solarRadiation[i:(i + 4)] == 640))
    .check("D13 night-time zero solar is never 'stuck'", !any(lg$variable == "solarRadiation" & lg$value == 0))
    clean <- attr(hourly_qc(base), "qc_log")
    .check("D14 clean data: nothing screened", nrow(clean) == 0, paste(nrow(clean), "log rows"))

    # ---- E. contract ---------------------------------------------------------------------
    .section("E. Contract")
    .check("E1 a screened table passes hourly_problems()", length(hourly_problems(q)) == 0,
           paste(hourly_problems(q), collapse = "; "))
    br <- q[-50, ]
    .check("E2 a missing row is caught (hours not consecutive)",
           any(grepl("consecutive", hourly_problems(br), fixed = TRUE)))
    br2 <- q; br2$temperature[10] <- 200
    .check("E3 a value past the limits is caught", any(grepl("temperature", hourly_problems(br2), fixed = TRUE)))
    e3 <- tryCatch(hourly_validate(br), error = function(e) conditionMessage(e))
    .check("E4 hourly_validate() stops with the problem listed", is.character(e3) && grepl("consecutive", e3, fixed = TRUE))

    # ---- F. bundled snapshots --------------------------------------------------------------
    .section("F. Bundled snapshots and the full pipeline (real data)")
    mf <- hourly_bundled_manifest()
    if (!nrow(mf)) {
      .info("F0 skipped", "no data/stations_bundled/manifest.csv")
    } else {
      expect_added <- c("21202" = 4, "80802" = 59, "50507" = 5, "44102" = 163)
      for (i in seq_len(nrow(mf))) {
        sid <- mf$station_id[i]
        r <- .run(paste(sid, "load"), hourly_load_station(sid))
        st <- r$value
        ok <- r$ok && is.list(st) && is.data.frame(st$hourly)
        .check(paste0("F", i, " ", sid, " ", mf$name[i], ": loads from the bundle without FEMS"),
               ok && identical(st$source$type, "bundled"), if (ok) st$source$detail else "")
        if (!ok) next
        .check(paste0("F", i, "b passes the contract"), length(hourly_problems(st$hourly)) == 0)
        .check(paste0("F", i, "c loads in under 10 s"), r$secs < 10, paste(r$secs, "s"))
        if (sid %in% names(expect_added))
          .check(paste0("F", i, "d grid completion added ", expect_added[[sid]], " hours (as of the 2 Nov 2026 snapshot)"),
                 st$grid_added == expect_added[[sid]], paste("added", st$grid_added))
        .info(paste0("F", i, "e"), sprintf("%s rows, %d screening actions, %s to %s", format(nrow(st$hourly), big.mark = ","),
                                           nrow(st$qc_log), min(st$hourly$date), max(st$hourly$date)))
      }
      # checksum guard, on a copy so the real files are never touched
      tmp <- file.path(tempdir(), "bundle_test"); dir.create(tmp, showWarnings = FALSE)
      file.copy(file.path(BUNDLED_DIR, mf$file[1]), tmp, overwrite = TRUE)
      bad <- mf[1, ]; bad$md5 <- "00000000000000000000000000000000"
      utils::write.csv(bad, file.path(tmp, "manifest.csv"), row.names = FALSE)
      old <- list(BUNDLED_DIR = BUNDLED_DIR, BUNDLED_MANIFEST = BUNDLED_MANIFEST)
      assign("BUNDLED_DIR", tmp, envir = globalenv()); assign("BUNDLED_MANIFEST", file.path(tmp, "manifest.csv"), envir = globalenv())
      w <- NULL
      res <- withCallingHandlers(hourly_read_bundled(mf$station_id[1]),
                                 warning = function(x) { w <<- conditionMessage(x); invokeRestart("muffleWarning") })
      assign("BUNDLED_DIR", old$BUNDLED_DIR, envir = globalenv()); assign("BUNDLED_MANIFEST", old$BUNDLED_MANIFEST, envir = globalenv())
      .check("F9 a checksum mismatch returns NULL with a warning (the app then falls back to FEMS)",
             is.null(res) && !is.null(w) && grepl("checksum", w, fixed = TRUE), if (is.null(w)) "" else w)
      .check("F10 a station that isn't bundled returns NULL quietly", is.null(hourly_read_bundled("99999")))
    }
  }, error = function(e) {
    .T$abort <- paste("test aborted:", conditionMessage(e))
    .say(""); .say("ABORTED: ", conditionMessage(e))
  })
  .test_finish()
})
