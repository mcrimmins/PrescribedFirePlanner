# ==============================================================================
# tests/test_download_stations.R -- download checks across time zones
#
# Runs the same short checks on four stations in different time zones, two
# weeks in January and two weeks in July, to confirm that:
#   * the download contract holds for every station, not just SAGUARO
#   * "local station time" means the same thing everywhere, and settles the
#     open DST question: does FEMS stay on standard time in summer?
#   * hour labels line up with the sun (solar centroid vs computed solar noon)
#
# Run from the project root with PrescribedFirePlanner.Rproj open:
#     source("tests/test_download_stations.R")
# Takes about 1-2 minutes. Edit `panel` to try other stations.
#
# Output in tests/logs/: transcript (ends in VERDICT) and a _summary.csv with
# one row per station x window.
# ==============================================================================

local({
  source("tests/helpers.R", local = TRUE)

  # ---- settings --------------------------------------------------------------
  panel <- data.frame(
    station_id = c("21202", "80802", "50507", "44102"),
    why = c("AZ Pima Co., MST, no DST",
            "FL Leon Co. (near Tallahassee), Eastern with DST",
            "CO Larimer Co., Mountain with DST",
            "CA Mariposa Co., Pacific with DST"),
    stringsAsFactors = FALSE)
  windows <- list(Jan = as.Date(c("2024-01-08", "2024-01-21")),
                  Jul = as.Date(c("2024-07-08", "2024-07-21")))

  .T <- .test_open("test_download_stations")
  summary_rows <- list()

  tryCatch({
    .env_header("FEMS download checks across time zones",
                c(paste0("Stations: ", paste(panel$station_id, collapse = ", ")),
                  paste0("Windows:  ", paste(names(windows), vapply(windows, function(w) paste(w, collapse = " to "), ""),
                                             collapse = "; "))))
    .section("A. Setup")
    .setup_layer("A")

    for (i in seq_len(nrow(panel))) {
      sid <- panel$station_id[i]
      .section(paste0("S", i, ". Station ", sid, " (", panel$why[i], ")"))
      meta <- .metadata_row(sid)
      .check(paste0("S", i, ".0 station is in the metadata CSV"), !is.null(meta))
      if (is.null(meta)) next
      tzo <- .tz_offsets(meta$tz, 2024); std_h <- .offset_hours(tzo$std)
      .info(paste0("S", i, ".0b metadata"),
            sprintf("%s, %s; lat %.3f lon %.3f; %s ft; tz %s; expected offsets Jan %s / Jul %s",
                    meta$name, meta$state, meta$lat, meta$lon, meta$elev_ft, meta$tz, tzo$jan, tzo$jul))

      for (w in names(windows)) {
        id <- paste0("S", i, ".", w)
        d0 <- windows[[w]][1]; d1 <- windows[[w]][2]
        r <- .run(paste(sid, w, "download"), fems_download_weather(sid, d0, d1, verbose = FALSE))
        wx <- r$value
        got <- r$ok && is.data.frame(wx) && nrow(wx) > 0
        .check(paste0(id, " download returns rows"), got, if (got) paste(nrow(wx), "rows") else "nothing returned")
        if (!got) next
        .check_wx_contract(wx, id, sid)
        wx <- wx[wx$date >= d0 & wx$date <= d1 & wx$record_type == "O", ]

        p <- .run(paste(sid, w, "raw probe"), fems_gql_paged(
          .Q_WEATHER,
          variables = list(startDateTimeRange = paste0(format(d0), "T00:00:00Z"),
                           endDateTimeRange   = paste0(format(d0 + 2), "T23:59:59Z"),
                           stationIds = sid, hasHistoricData = "ALL",
                           sortBy = "observation_time", sortOrder = "asc"),
          root = "weatherObs", per_page = 5000, verbose = FALSE))
        offs <- if (p$ok && nrow(p$value)) unique(.offset_of(p$value$display_hour_lst)) else character(0)
        .check(paste0(id, " one UTC offset in the window"), length(offs) == 1,
               paste("seen", paste(offs, collapse = ",")))
        if (w == "Jan") .check(paste0(id, " January offset is the standard offset"),
                               length(offs) == 1 && offs == tzo$std,
                               paste0("seen ", paste(offs, collapse = ","), ", expected ", tzo$std))

        doy <- as.integer(format(d0 + 7, "%j"))
        noon <- .solar_noon_lst(meta$lon, std_h, doy)
        cen <- .solar_centroid(wx$hour, wx$solarRadiation)
        diffh <- cen - noon
        .check(paste0(id, " solar centroid within 1.5 h of solar noon"), is.finite(diffh) && abs(diffh) <= 1.5,
               paste0("centroid ", round(cen, 2), ", solar noon (LST) ", round(noon, 2), ", diff ", round(diffh, 2), " h"))
        tm <- tapply(wx$temperature, wx$hour, mean, na.rm = TRUE)
        warm <- as.integer(names(tm))[which.max(tm)]
        .qc(paste0(id, " warmest mean hour 11..17"), warm %in% 11:17, paste("hour", warm))

        summary_rows[[length(summary_rows) + 1]] <- data.frame(
          station_id = sid, name = meta$name, tz = meta$tz, window = w,
          expected_std = tzo$std, expected_civil = if (w == "Jan") tzo$jan else tzo$jul,
          offset_seen = paste(offs, collapse = ","), zone_has_dst = tzo$dst,
          rows = nrow(wx), pct_temp = .pct(mean(!is.na(wx$temperature))),
          pct_solar = .pct(mean(!is.na(wx$solarRadiation))),
          solar_noon_lst = round(noon, 2), solar_centroid = round(cen, 2), centroid_minus_noon = round(diffh, 2),
          warmest_hour = warm, stringsAsFactors = FALSE)
      }
    }

    # ---- cross-station conclusions --------------------------------------------
    .section("X. Across stations")
    if (length(summary_rows)) {
      sm <- do.call(rbind, summary_rows)
      .show(sm[c("station_id", "name", "window", "expected_std", "expected_civil", "offset_seen",
                 "pct_temp", "centroid_minus_noon", "warmest_hour")])
      .write_csv(sm, "summary")

      jul <- sm[sm$window == "Jul" & sm$zone_has_dst, ]
      if (nrow(jul)) {
        conv <- ifelse(jul$offset_seen == jul$expected_std, "standard",
                       ifelse(jul$offset_seen == jul$expected_civil, "daylight", "other"))
        .check("X1 all DST-zone stations use the same July convention", length(unique(conv)) == 1,
               paste(jul$station_id, conv, sep = "=", collapse = ", "))
        .info("X2 ANSWER to the DST open item",
              if (all(conv == "standard")) "FEMS local station time is STANDARD time all year; summer hour labels are 1 h behind the clock"
              else if (all(conv == "daylight")) "FEMS local station time FOLLOWS daylight saving time; hour labels are clock time"
              else "mixed or unclear -- see the table")
        js <- merge(sm[sm$window == "Jan", c("station_id", "centroid_minus_noon")],
                    sm[sm$window == "Jul", c("station_id", "centroid_minus_noon")],
                    by = "station_id", suffixes = c("_jan", "_jul"))
        js$jul_minus_jan <- round(js$centroid_minus_noon_jul - js$centroid_minus_noon_jan, 2)
        .info("X3 July minus January solar centroid shift (h; ~+1 at DST stations means labels follow DST)",
              paste(js$station_id, js$jul_minus_jan, sep = "=", collapse = ", "))
      }
    } else .check("X0 at least one station downloaded", FALSE)

  }, error = function(e) {
    .T$abort <- paste("test aborted:", conditionMessage(e))
    .say(""); .say("ABORTED: ", conditionMessage(e))
  })

  .test_finish()
})
