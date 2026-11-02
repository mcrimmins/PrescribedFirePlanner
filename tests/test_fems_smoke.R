# ==============================================================================
# tests/test_fems_smoke.R -- smoke test for the FEMS download layer
#
# Checks that R/fems_api.R + R/fems_download.R (copied from NFDRSChartBuilder)
# still work in this repo: one station, a short date range, both feeds and the
# join, with the column contract and station-LOCAL hours verified.
#
# Run from the project root with PrescribedFirePlanner.Rproj open:
#     source("tests/test_fems_smoke.R")
#
# Needs network access and FEMS_USER / FEMS_KEY in .Renviron (restart R after
# editing .Renviron). Writes a transcript to tests/logs/ (gitignored) whose
# last line is "VERDICT: PASS" or "VERDICT: FAIL".
#
# Check levels:
#   PASS / FAIL  count toward the verdict
#   INFO         diagnostics only; read them, they do not change the verdict
#
# Station: SAGUARO (21202), Arizona -- MST all year (no DST), so every local
# timestamp should carry a -07:00 offset and afternoon should be hot and dry.
#
# 2026-11-02: C7, C8 and E5 are now PASS/FAIL checks for the weather-window
# fix in R/fems_download.R (they were INFO while the bug was open), and
# obs_minute joined the weather contract (C14).
#
# All test-local objects live inside local({ ... }); the only things this
# script leaves in the global environment are what R/fems_download.R defines,
# exactly as app.R would see them.
# ==============================================================================

local({

  # ---- settings --------------------------------------------------------------
  station_id <- "21202"                 # SAGUARO
  start_date <- as.Date("2025-06-01")   # hot, dry pre-monsoon week:
  end_date   <- as.Date("2025-06-07")   # strong, unambiguous diurnal cycle
  expected_offset <- "-07:00"

  nfdrs_cols <- c("station_id", "date", "hour", "record_type", "fuel_model",
                  "kbdi", "oneHR_TL_FuelMoisture", "tenHR_TL_FuelMoisture",
                  "hundredHR_TL_FuelMoisture", "thousandHR_TL_FuelMoisture",
                  "ignitionComponent", "spreadComponent",
                  "energyReleaseComponent", "burningIndex",
                  "herbaceousLFI_fuelMoisture", "woodyLFI_fuelMoisture", "gsi")
  wx_cols <- c("station_id", "date", "hour", "record_type", "obs_minute",
               "temperature", "relativeHumidity", "precipitation", "windSpeed",
               "windDirection", "gustSpeed", "gustDirection", "solarRadiation",
               "vpd", "hdw", "dewpoint")
  join_cols <- union(nfdrs_cols, wx_cols)

  # ---- transcript ------------------------------------------------------------
  log_dir <- file.path("tests", "logs")
  dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)
  log_file <- file.path(log_dir, paste0("test_fems_smoke_",
                                        format(Sys.time(), "%Y%m%d_%H%M%S"),
                                        ".txt"))
  con <- file(log_file, open = "wt", encoding = "UTF-8")

  .say <- function(...) {
    txt <- paste0(...)
    cat(txt, "\n", sep = "")
    cat(txt, "\n", sep = "", file = con)
  }
  .show <- function(x) for (ln in utils::capture.output(print(x))) .say("    ", ln)
  .section <- function(title) { .say(""); .say("---- ", title, " ", strrep("-", max(3, 70 - nchar(title)))) }

  results <- data.frame(id = character(0), status = character(0),
                        detail = character(0), stringsAsFactors = FALSE)
  .record <- function(id, status, detail) {
    results[nrow(results) + 1, ] <<- list(id, status, detail)
    .say(sprintf("[%s] %s%s", status, id,
                 if (nzchar(detail)) paste0(" -- ", detail) else ""))
  }
  .check <- function(id, ok, detail = "") .record(id, if (isTRUE(ok)) "PASS" else "FAIL", detail)
  .info  <- function(id, detail = "")     .record(id, "INFO", detail)

  # Run an expression, echoing its messages/warnings into the transcript and
  # turning an error into a value instead of aborting the test.
  .run <- function(label, expr) {
    warns <- character(0)
    t0 <- Sys.time()
    val <- withCallingHandlers(
      tryCatch(expr, error = function(e) {
        .say("  ERROR in ", label, ": ", conditionMessage(e))
        structure(list(msg = conditionMessage(e)), class = "smoke_error")
      }),
      message = function(m) {
        .say("  | ", sub("\n$", "", conditionMessage(m)))
        invokeRestart("muffleMessage")
      },
      warning = function(w) {
        warns <<- c(warns, conditionMessage(w))
        .say("  ! warning: ", conditionMessage(w))
        invokeRestart("muffleWarning")
      })
    secs <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
    .say("  (", label, ": ", secs, " s)")
    list(value = val, ok = !inherits(val, "smoke_error"),
         warnings = warns, secs = secs)
  }

  .offset_of <- function(x) {
    x <- as.character(x)
    ifelse(grepl("Z$", x), "Z", sub("^.*([+-][0-9]{2}:[0-9]{2})$", "\\1", x))
  }
  .hours_per_date <- function(df) {
    tab <- tapply(df$hour, df$date, function(h) length(unique(h)))
    data.frame(date = as.Date(names(tab)), n_hours = as.integer(tab))
  }
  .col_types <- function(df) vapply(df[c("station_id", "date", "hour", "record_type")],
                                    function(v) class(v)[1], character(1))
  .types_ok <- function(df) identical(unname(.col_types(df)),
                                      c("character", "Date", "integer", "character"))
  .dup_keys <- function(df) sum(duplicated(df[c("station_id", "date", "hour", "record_type")]))

  verdict_note <- ""

  tryCatch({

    .say("FEMS download-layer smoke test")
    .say("Run at:   ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"))
    .say("Station:  ", station_id, " (SAGUARO)   Range: ", start_date, " to ", end_date)
    .say("R:        ", R.version.string, " on ", R.version$platform)
    .say("WD:       ", normalizePath(getwd(), winslash = "/"))
    for (p in c("httr", "jsonlite", "dplyr", "tibble")) {
      v <- tryCatch(as.character(utils::packageVersion(p)), error = function(e) "NOT INSTALLED")
      .say(sprintf("          %-9s %s", p, v))
    }

    # ---- A. setup ------------------------------------------------------------
    .section("A. Setup")
    at_root <- file.exists("R/fems_download.R") && file.exists("R/fems_api.R") &&
               file.exists("PrescribedFirePlanner.Rproj")
    .check("A1 working directory is the project root", at_root,
           if (at_root) "" else "open PrescribedFirePlanner.Rproj (or setwd() to it) and re-run")
    if (!at_root) stop("not at project root; nothing else can run")

    user <- Sys.getenv("FEMS_USER", "")
    key  <- Sys.getenv("FEMS_KEY", "")
    .check("A2 FEMS_USER and FEMS_KEY are set", nzchar(user) && nzchar(key),
           paste0("user=", if (nzchar(user)) user else "<empty>",
                  ", key=", if (nzchar(key)) paste0("<", nchar(key), " chars>") else "<empty>",
                  if (nzchar(user) && nzchar(key)) "" else " (edit .Renviron, then restart R)"))

    src <- .run("source R/fems_download.R", source("R/fems_download.R"))
    fns <- c("fems_gql", "fems_gql_paged", "fems_download_nfdrs",
             "fems_download_weather", "fems_fetch_station_data")
    have <- vapply(fns, exists, logical(1), envir = globalenv())
    .check("A3 download layer sources and defines its functions", src$ok && all(have),
           if (all(have)) "" else paste("missing:", paste(fns[!have], collapse = ", ")))
    if (!(src$ok && all(have))) stop("download layer did not load")
    .info("A3b key mask (from fems_mask_key)", fems_mask_key(key))

    # Carryover sec. 2: attaching jsonlite masks shiny::validate().
    .check("A4 sourcing does not attach jsonlite (would mask shiny::validate)",
           !("package:jsonlite" %in% search()))
    .info("A4b packages attached by sourcing",
          paste(intersect(c("package:dplyr", "package:tibble", "package:httr",
                            "package:jsonlite"), search()), collapse = ", "))

    # ---- B. NFDRS feed -------------------------------------------------------
    .section("B. NFDRS feed (fems_download_nfdrs)")
    rb <- .run("NFDRS fetch", fems_download_nfdrs(station_id, start_date, end_date))
    nf <- rb$value
    nf_ok <- rb$ok && is.data.frame(nf) && nrow(nf) > 0
    .check("B1 returns rows", nf_ok,
           if (nf_ok) paste(nrow(nf), "rows") else "no data frame returned")
    if (nf_ok) {
      miss <- setdiff(nfdrs_cols, names(nf)); extra <- setdiff(names(nf), nfdrs_cols)
      .check("B2 column set matches the download contract", !length(miss) && !length(extra),
             paste0(if (length(miss)) paste0("missing: ", paste(miss, collapse = ", "), " ") else "",
                    if (length(extra)) paste0("extra: ", paste(extra, collapse = ", ")) else ""))
      .check("B3 key types chr/Date/int/chr", .types_ok(nf),
             paste(names(.col_types(nf)), .col_types(nf), sep = "=", collapse = ", "))
      .check("B4 station_id is \"21202\" (character)", all(nf$station_id == station_id))
      .check("B5 hour is in 0..23", all(nf$hour %in% 0:23),
             paste0("range ", min(nf$hour), "..", max(nf$hour)))
      .check("B6 no duplicate station/date/hour/record_type", .dup_keys(nf) == 0,
             paste(.dup_keys(nf), "duplicates"))
      .check("B7 all dates inside the requested range",
             all(nf$date >= start_date & nf$date <= end_date),
             paste0("dates ", min(nf$date), " to ", max(nf$date)))
      hpd <- .hours_per_date(nf)
      want_dates <- seq(start_date, end_date, by = "day")
      full <- all(want_dates %in% hpd$date) && all(hpd$n_hours[hpd$date %in% want_dates] == 24)
      .check("B8 every requested local date has 24 hours", full)
      .say("    hours per local date:"); .show(hpd)
      .info("B9 record_type counts", paste(names(table(nf$record_type)),
                                           table(nf$record_type), sep = "=", collapse = ", "))
      na_frac <- round(colMeans(is.na(nf[setdiff(nfdrs_cols, c("station_id", "date", "hour", "record_type", "fuel_model"))])), 3)
      .info("B10 NA fraction by NFDRS column",
            paste(names(na_frac), na_frac, sep = "=", collapse = ", "))
    }

    # ---- C. weather feed -----------------------------------------------------
    .section("C. Weather feed (fems_download_weather)")
    rc <- .run("weather fetch", fems_download_weather(station_id, start_date, end_date))
    wx <- rc$value
    wx_ok <- rc$ok && is.data.frame(wx) && nrow(wx) > 0
    .check("C1 returns rows", wx_ok,
           if (wx_ok) paste(nrow(wx), "rows") else "no data frame returned")
    if (wx_ok) {
      miss <- setdiff(wx_cols, names(wx)); extra <- setdiff(names(wx), wx_cols)
      .check("C2 column set matches the download contract", !length(miss) && !length(extra),
             paste0(if (length(miss)) paste0("missing: ", paste(miss, collapse = ", "), " ") else "",
                    if (length(extra)) paste0("extra: ", paste(extra, collapse = ", ")) else ""))
      .check("C3 key types chr/Date/int/chr", .types_ok(wx),
             paste(names(.col_types(wx)), .col_types(wx), sep = "=", collapse = ", "))
      .check("C4 hour is in 0..23", all(wx$hour %in% 0:23),
             paste0("range ", min(wx$hour), "..", max(wx$hour)))
      .check("C5 no duplicate station/date/hour/record_type", .dup_keys(wx) == 0,
             paste(.dup_keys(wx), "duplicates"))

      hpd <- .hours_per_date(wx)
      interior <- seq(start_date + 1, end_date - 1, by = "day")
      int_full <- all(interior %in% hpd$date) && all(hpd$n_hours[hpd$date %in% interior] == 24)
      .check("C6 interior local dates have 24 hours", int_full)
      .say("    hours per local date:"); .show(hpd)

      # Weather-window fix: the request is padded a day each side in UTC and
      # trimmed to local dates, so nothing may fall outside the requested
      # dates and both end days must be complete.
      outside <- wx[wx$date < start_date | wx$date > end_date, ]
      first  <- wx[wx$date == start_date, ]
      last   <- wx[wx$date == end_date, ]
      .check("C7 no weather rows labelled outside the requested local dates", nrow(outside) == 0,
             if (nrow(outside)) paste0(nrow(outside), " rows on ", paste(unique(outside$date), collapse = ","))
             else "none")
      .check("C8 start_date and end_date each have all 24 hours",
             length(unique(first$hour)) == 24 && length(unique(last$hour)) == 24,
             paste0(start_date, ": ", length(unique(first$hour)), " h; ",
                    end_date, ": ", length(unique(last$hour)), " h"))

      .check("C9 relativeHumidity within 0..100",
             all(wx$relativeHumidity >= 0 & wx$relativeHumidity <= 100, na.rm = TRUE),
             paste0("range ", paste(round(range(wx$relativeHumidity, na.rm = TRUE), 1), collapse = "..")))
      .check("C10 temperature within -40..130 F",
             all(wx$temperature >= -40 & wx$temperature <= 130, na.rm = TRUE),
             paste0("range ", paste(round(range(wx$temperature, na.rm = TRUE), 1), collapse = "..")))
      .check("C11 vpd within 0..15 kPa (i.e. Pa were divided by 1000)",
             all(wx$vpd >= 0 & wx$vpd <= 15, na.rm = TRUE),
             paste0("range ", paste(round(range(wx$vpd, na.rm = TRUE), 2), collapse = "..")))
      .info("C12 windSpeed range (mph)",
            paste(round(range(wx$windSpeed, na.rm = TRUE), 1), collapse = ".."))
      .info("C13 record_type counts", paste(names(table(wx$record_type)),
                                            table(wx$record_type), sep = "=", collapse = ", "))
      om <- wx$obs_minute
      .check("C14 obs_minute is an integer 0..59 on every row",
             is.integer(om) && !anyNA(om) && all(om >= 0 & om <= 59),
             paste0("class ", class(om)[1], ", NA ", sum(is.na(om))))
      .info("C14b observation minute(s) seen", paste(names(table(om)), table(om), sep = " x", collapse = ", "))
    }

    # ---- D. local time -------------------------------------------------------
    .section("D. Station-local time")
    # D1/D2 look at the raw display_hour_lst strings, before .lst_hour() strips
    # the offset, to confirm the feed is really in station-local time.
    rw <- .run("raw weather probe", fems_gql_paged(
      .Q_WEATHER,
      variables = list(startDateTimeRange = paste0(format(start_date), "T00:00:00Z"),
                       endDateTimeRange   = paste0(format(start_date + 1), "T23:59:59Z"),
                       stationIds = station_id, hasHistoricData = "ALL",
                       sortBy = "observation_time", sortOrder = "asc"),
      root = "weatherObs", per_page = 5000, verbose = FALSE))
    if (rw$ok && nrow(rw$value) > 0) {
      off <- table(.offset_of(rw$value$display_hour_lst))
      .check("D1 weather display_hour_lst offsets are all -07:00",
             identical(names(off), expected_offset),
             paste(names(off), off, sep = " x", collapse = ", "))
      .say("    first raw values: ", paste(head(rw$value$display_hour_lst, 3), collapse = " | "))
    } else .check("D1 weather display_hour_lst offsets are all -07:00", FALSE, "raw probe returned nothing")

    rn <- .run("raw NFDRS probe", fems_gql_paged(
      .Q_NFDRS,
      variables = list(fuelModels = "Y", stationIds = station_id,
                       startDateRange = format(start_date), endDateRange = format(start_date + 1),
                       hasHistoricData = "ALL", dateTimeFormat = "LocalStationTime",
                       sortBy = "observation_time", sortOrder = "asc"),
      root = "nfdrsObs", per_page = 5000, verbose = FALSE))
    if (rn$ok && nrow(rn$value) > 0) {
      off <- table(.offset_of(rn$value$display_hour_lst))
      .check("D2 NFDRS display_hour_lst offsets are all -07:00",
             identical(names(off), expected_offset),
             paste(names(off), off, sep = " x", collapse = ", "))
      .say("    first raw values: ", paste(head(rn$value$display_hour_lst, 3), collapse = " | "))
    } else .check("D2 NFDRS display_hour_lst offsets are all -07:00", FALSE, "raw probe returned nothing")

    # D3-D5: the diurnal cycle has to line up with local clock hours. Under the
    # old UTC-labelled data the afternoon peak landed near hour 20-23.
    if (wx_ok) {
      obs <- wx[wx$record_type == "O" | is.na(wx$record_type), ]
      by_hr <- data.frame(
        hour = 0:23,
        temp = vapply(0:23, function(h) mean(obs$temperature[obs$hour == h], na.rm = TRUE), numeric(1)),
        rh   = vapply(0:23, function(h) mean(obs$relativeHumidity[obs$hour == h], na.rm = TRUE), numeric(1)),
        wind = vapply(0:23, function(h) mean(obs$windSpeed[obs$hour == h], na.rm = TRUE), numeric(1)))
      by_hr[-1] <- round(by_hr[-1], 1)
      .say("    mean by local hour (observed records):"); .show(by_hr)
      h_tmax <- by_hr$hour[which.max(by_hr$temp)]
      h_tmin <- by_hr$hour[which.min(by_hr$temp)]
      h_rmin <- by_hr$hour[which.min(by_hr$rh)]
      .check("D3 warmest mean hour falls 11..17 local", h_tmax %in% 11:17, paste("hour", h_tmax))
      .check("D4 driest (min RH) mean hour falls 11..17 local", h_rmin %in% 11:17, paste("hour", h_rmin))
      .check("D5 coolest mean hour falls 2..9 local", h_tmin %in% 2:9, paste("hour", h_tmin))
      .info("D6 13:00 local means", paste0("temp ", by_hr$temp[by_hr$hour == 13], " F, RH ",
                                           by_hr$rh[by_hr$hour == 13], " %"))
    } else .check("D3-D5 diurnal checks", FALSE, "no weather data")

    # ---- E. combined fetch + join --------------------------------------------
    .section("E. Combined fetch (fems_fetch_station_data)")
    re <- .run("combined fetch", fems_fetch_station_data(station_id, start_date, end_date))
    jd <- re$value
    j_ok <- re$ok && is.data.frame(jd) && nrow(jd) > 0
    .check("E1 returns rows", j_ok, if (j_ok) paste(nrow(jd), "rows") else "NULL or error")
    if (j_ok) {
      .check("E2 column set is NFDRS + weather",
             setequal(names(jd), join_cols),
             if (setequal(names(jd), join_cols)) "" else
               paste0("missing: ", paste(setdiff(join_cols, names(jd)), collapse = ","),
                      " extra: ", paste(setdiff(names(jd), join_cols), collapse = ",")))
      if (nf_ok) .check("E3 left join did not multiply rows", nrow(jd) == nrow(nf),
                        paste(nrow(jd), "joined vs", nrow(nf), "NFDRS"))
      interior <- jd$date > start_date & jd$date < end_date
      cov <- mean(!is.na(jd$temperature[interior]))
      .check("E4 interior dates: >= 90% of rows have weather", cov >= 0.9,
             paste0(round(100 * cov, 1), "% non-NA temperature"))
      for (d in list(start_date, end_date)) {
        sub <- jd[jd$date == d, ]
        na_h <- sort(unique(sub$hour[is.na(sub$temperature)]))
        .check(paste0("E5 ", d, ": every hour has weather after the join"), length(na_h) == 0,
               if (length(na_h)) paste("missing hours", paste(na_h, collapse = ",")) else "")
      }
    }

  }, error = function(e) {
    verdict_note <<- paste("test aborted:", conditionMessage(e))
    .say(""); .say("ABORTED: ", conditionMessage(e))
  })

  # ---- verdict ---------------------------------------------------------------
  .section("Summary")
  n_pass <- sum(results$status == "PASS")
  n_fail <- sum(results$status == "FAIL")
  n_info <- sum(results$status == "INFO")
  .say(sprintf("%d PASS, %d FAIL, %d INFO", n_pass, n_fail, n_info))
  if (n_fail > 0) {
    .say("Failed checks:")
    for (i in which(results$status == "FAIL")) .say("  - ", results$id[i], "  ", results$detail[i])
  }
  if (nzchar(verdict_note)) .say(verdict_note)
  .say("Transcript: ", normalizePath(log_file, winslash = "/", mustWork = FALSE))
  ok <- n_fail == 0 && !nzchar(verdict_note) && n_pass > 0
  .say("VERDICT: ", if (ok) "PASS" else "FAIL")
  close(con)
  invisible(ok)
})
