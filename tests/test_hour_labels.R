# ==============================================================================
# tests/test_hour_labels.R -- did FEMS hour labelling change during the record?
#
# test_download_por.R found that SAGUARO's solar-radiation timing steps by
# about +0.65 h between 2005-2019 and 2020-2025. This test asks why:
#
#   Part 1  Do ALL variables shift, or only solar radiation?
#           Per year, the phase of the mean daily cycle (first harmonic) of
#           temperature, RH, wind speed and solar radiation. If every variable
#           steps by the same amount, the hour labels moved. If only solar
#           steps, the solar sensor or its averaging changed.
#   Part 2  What do the raw timestamps say?
#           One June week per year, requesting observation_time_lst (the real
#           observation time, with minutes) next to display_hour_lst (the hour
#           label the app uses). Shows the observation minute each year and
#           whether the label is the observation hour floored or rounded.
#
# Run from the project root with PrescribedFirePlanner.Rproj open:
#     source("tests/test_hour_labels.R")
# About 1-2 minutes. Diagnostic: almost everything is INFO; the verdict only
# says whether the test could run.
# ==============================================================================

local({
  source("tests/helpers.R", local = TRUE)

  station_id <- "80802"   # SAGUARO
  years      <- 2005:2025
  split_year <- 2020      # used only to compare raw times before/after in Part 2

  # Same as .Q_WEATHER in R/fems_download.R plus observation_time_lst.
  # Test-only; the app's query is unchanged.
  q_times <- '
    query WeatherTimes($startDateTimeRange: DateTime!, $endDateTimeRange: DateTime!,
                       $stationIds: String, $hasHistoricData: TriState,
                       $sortBy: WxObsSortBy, $sortOrder: SortOrder,
                       $page: Int, $perPage: Int) {
      weatherObs(
        startDateTimeRange: $startDateTimeRange
        endDateTimeRange: $endDateTimeRange
        stationIds: $stationIds
        hasHistoricData: $hasHistoricData
        sortBy: $sortBy
        sortOrder: $sortOrder
        page: $page
        per_page: $perPage
      ) {
        _metadata { page per_page total_count page_count }
        data { station_id display_hour_lst observation_time_lst observation_type }
      }
    }'

  .T <- .test_open(paste0("test_hour_labels_", station_id))

  tryCatch({
    .env_header("FEMS hour-label diagnostics",
                c(paste0("Station:  ", station_id),
                  paste0("Years:    ", min(years), "-", max(years), " (split at ", split_year, ")")))
    .section("A. Setup")
    .setup_layer("A")
    meta <- .metadata_row(station_id)
    .check("A5 station is in the metadata CSV", !is.null(meta))
    if (is.null(meta)) stop("station not in metadata")

    # ---- Part 1: phase of the daily cycle, per year and variable -------------
    .section("B. Daily-cycle phase by year (all variables)")
    rb <- .run("weather download", fems_download_weather(station_id, sprintf("%d-01-01", min(years)),
                                                         sprintf("%d-12-31", max(years))))
    wx <- rb$value
    got <- rb$ok && is.data.frame(wx) && nrow(wx) > 0
    .check("B1 download returns rows", got, if (got) paste(nrow(wx), "rows") else "nothing returned")
    if (!got) stop("no data")
    wx <- wx[wx$record_type == "O", ]
    wx$year <- as.integer(format(wx$date, "%Y"))
    wx <- wx[wx$year %in% years, ]

    # Hour (0-24) at which the first harmonic of the mean daily cycle peaks.
    .peak_hour <- function(hour, x) {
      m <- tapply(x, hour, mean, na.rm = TRUE)
      h <- as.numeric(names(m)); m <- as.numeric(m)
      ok <- is.finite(m)
      if (sum(ok) < 20) return(NA_real_)
      a <- sum(m[ok] * cos(2 * pi * h[ok] / 24)); b <- sum(m[ok] * sin(2 * pi * h[ok] / 24))
      (atan2(b, a) * 24 / (2 * pi)) %% 24
    }
    vars <- c(temperature = "temperature", RH = "relativeHumidity", wind = "windSpeed", solar = "solarRadiation")
    ph <- do.call(rbind, lapply(years, function(y) {
      d <- wx[wx$year == y, ]
      out <- data.frame(year = y)
      for (nm in names(vars)) out[[paste0(nm, "_peak")]] <- round(.peak_hour(d$hour, d[[vars[[nm]]]]), 2)
      out
    }))
    .say("    hour of peak of the daily cycle (first harmonic, LST):"); .show(ph)
    .write_csv(ph, "phase_by_year")

    # Each variable gets its own best split year (helpers.R .step_year); a step
    # counts when it is >= 0.3 h and >= 4 x the year-to-year spread.
    stp <- do.call(rbind, lapply(names(vars), function(nm) {
      r <- .step_year(ph$year, ph[[paste0(nm, "_peak")]])
      data.frame(variable = nm, step_year = r$year, step_h = round(r$step, 2), spread_h = round(r$spread, 2),
                 detected = is.finite(r$step) && abs(r$step) >= 0.3 && abs(r$step) >= 4 * r$spread)
    }))
    .say("    best single step per variable (peak hour, later minus earlier):"); .show(stp)
    .write_csv(stp, "steps")
    for (i in seq_len(nrow(stp)))
      .info(paste0("B2 ", stp$variable[i]),
            if (stp$detected[i]) sprintf("step of %+.2f h from %d (spread %.2f h)", stp$step_h[i], stp$step_year[i], stp$spread_h[i])
            else sprintf("no clear step (best split %s: %+.2f h, spread %.2f h)", stp$step_year[i], stp$step_h[i], stp$spread_h[i]))
    det <- stp[stp$detected, ]
    .info("B3 reading",
          if (!nrow(det)) "no variable steps clearly: timing looks steady"
          else if (nrow(det) == nrow(stp) && length(unique(det$step_year)) == 1 && diff(range(det$step_h)) <= 0.25)
            paste0("ALL variables step together in ", det$step_year[1], ": consistent with the hour labels moving")
          else paste0("variables step in different years (",
                      paste(det$variable, det$step_year, sep = " ", collapse = ", "),
                      "): consistent with sensor averaging changes, not a label shift"))

    # ---- Part 2: raw observation times ---------------------------------------
    .section("C. Raw observation times, one June week per year")
    probe <- function(y) fems_gql_paged(
      q_times,
      variables = list(startDateTimeRange = sprintf("%d-06-10T07:00:00Z", y),
                       endDateTimeRange   = sprintf("%d-06-17T06:59:59Z", y),
                       stationIds = station_id, hasHistoricData = "ALL",
                       sortBy = "observation_time", sortOrder = "asc"),
      root = "weatherObs", per_page = 5000, verbose = FALSE)

    first <- .run(paste("raw times", years[1]), probe(years[1]))
    .check("C1 weatherObs accepts observation_time_lst", first$ok,
           if (first$ok) "" else "field rejected; Part 2 skipped (Part 1 still stands)")
    if (first$ok) {
      rows <- list()
      for (y in years) {
        r <- if (y == years[1]) first else .run(paste("raw times", y), probe(y))
        if (!r$ok || !nrow(r$value)) { .info(paste("C2", y), "no rows"); next }
        x <- r$value
        x <- x[toupper(x$observation_type) == "O" & !is.na(x$observation_time_lst), ]
        if (!nrow(x)) { .info(paste("C2", y), "no observed rows with a time"); next }
        ot <- as.character(x$observation_time_lst); dl <- as.character(x$display_hour_lst)
        o_hr  <- as.integer(substr(ot, 12, 13)); o_min <- as.integer(substr(ot, 15, 16))
        d_hr  <- as.integer(substr(dl, 12, 13))
        same_day <- substr(ot, 1, 10) == substr(dl, 1, 10)
        floor_match <- mean(same_day & d_hr == o_hr)
        round_hr <- (o_hr + (o_min >= 30)) %% 24
        round_match <- mean(d_hr == round_hr)
        mins <- sort(table(o_min), decreasing = TRUE)
        rows[[length(rows) + 1]] <- data.frame(
          year = y, n = nrow(x),
          top_minute = as.integer(names(mins)[1]), top_minute_share = round(100 * mins[[1]] / nrow(x), 1),
          median_minute = stats::median(o_min),
          label_is_floor_pct = round(100 * floor_match, 1), label_is_rounded_pct = round(100 * round_match, 1),
          example_obs = ot[1], example_label = dl[1], stringsAsFactors = FALSE)
      }
      if (length(rows)) {
        rt <- do.call(rbind, rows)
        .say("    observation minute and how the hour label relates to it:")
        .show(rt[c("year", "n", "top_minute", "top_minute_share", "median_minute",
                   "label_is_floor_pct", "label_is_rounded_pct")])
        .say("    examples:"); .show(rt[c("year", "example_obs", "example_label")])
        .write_csv(rt, "raw_times_by_year")
        e <- rt[rt$year < split_year, ]; l <- rt[rt$year >= split_year, ]
        .info("C3 typical observation minute, earlier vs later",
              paste0(stats::median(e$median_minute), " vs ", stats::median(l$median_minute)))
        .info("C4 label = observation hour floored, earlier vs later (%)",
              paste0(round(mean(e$label_is_floor_pct), 1), " vs ", round(mean(l$label_is_floor_pct), 1)))
        .info("C5 label = observation hour rounded, earlier vs later (%)",
              paste0(round(mean(e$label_is_rounded_pct), 1), " vs ", round(mean(l$label_is_rounded_pct), 1)))
      }
    }

  }, error = function(e) {
    .T$abort <- paste("test aborted:", conditionMessage(e))
    .say(""); .say("ABORTED: ", conditionMessage(e))
  })

  .test_finish()
})
