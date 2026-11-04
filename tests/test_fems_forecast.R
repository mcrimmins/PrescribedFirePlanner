# ==============================================================================
# tests/test_fems_forecast.R -- PROBE: what forecast data does FEMS have?
#
# Scoping for the Season tracker tab (current year + FEMS forecast). Nothing in
# the app uses forecasts yet; this finds out what exists before we design for
# it. Mostly INFO lines: read the transcript and paste it back.
#
#   A  the schema (GraphQL introspection, if FEMS allows it): weatherObs
#      arguments and fields, and any query whose name mentions forecasts
#   B  weatherObs from 3 days ago to 10 days ahead for the bundled stations:
#      counts by observation_type, hours per day, how far ahead "F" runs,
#      which variables are filled, and whether F and O overlap
#   C  nfdrsObs over the same window: NFDRS forecast records (nfdr_type "F")
#   D  the current-year top-up: how long one station's snapshot-end-to-today
#      request takes
#
# Needs network access and FEMS_USER / FEMS_KEY in .Renviron.
#     source("tests/test_fems_forecast.R")
# ==============================================================================

local({
  source("tests/helpers.R", local = TRUE)
  .T <- .test_open("test_fems_forecast")

  tryCatch({
    .env_header("FEMS forecast probe (network)")
    .section("A. Setup and schema")
    r <- .run("source R/fems_download.R", source("R/fems_download.R"))
    .check("A1 R/fems_download.R loads", r$ok)
    if (!r$ok) stop("could not source R/fems_download.R")
    .check("A2 FEMS credentials are set", nzchar(Sys.getenv("FEMS_USER")) && nzchar(Sys.getenv("FEMS_KEY")))

    intro <- .run("introspection", fems_gql('{ __schema { queryType { fields { name args { name } } } } }', simplify = TRUE))
    if (intro$ok && !is.null(intro$value$`__schema`)) {
      qf <- intro$value$`__schema`$queryType$fields
      .info("A3 queries FEMS offers", paste(qf$name, collapse = ", "))
      fc <- qf$name[grepl("forecast|fcst|ndfd|point", qf$name, ignore.case = TRUE)]
      .info("A4 queries mentioning forecasts", if (length(fc)) paste(fc, collapse = ", ") else "none")
      wa <- qf$args[[which(qf$name == "weatherObs")]]
      .info("A5 weatherObs arguments", if (length(wa)) paste(wa$name, collapse = ", ") else "?")
      ty <- .run("weatherObs fields", fems_gql('{ __type(name: "WeatherObs") { fields { name } } }', simplify = TRUE))
      if (ty$ok && !is.null(ty$value$`__type`)) .info("A6 WeatherObs fields", paste(ty$value$`__type`$fields$name, collapse = ", "))
      else .info("A6 WeatherObs fields", "type name not WeatherObs; see A3/A5")
    } else {
      .info("A3 introspection", "not allowed or failed; sections B-D still tell us what the data look like")
    }

    .section("B. weatherObs, 3 days back to 10 days ahead")
    mf <- utils::read.csv("data/stations_featured.csv", colClasses = c(station_id = "character"), stringsAsFactors = FALSE)
    today <- Sys.Date()
    for (sid in mf$station_id) {
      .say(""); .say("  Station ", sid, " (", mf$name[mf$station_id == sid], ")")
      w <- .run(paste("weatherObs", sid), fems_gql_paged(.Q_WEATHER,
        variables = list(startDateTimeRange = paste0(format(today - 3), "T00:00:00Z"),
                         endDateTimeRange   = paste0(format(today + 10), "T23:59:59Z"),
                         stationIds = sid, hasHistoricData = "ALL",
                         sortBy = "observation_time", sortOrder = "asc"),
        root = "weatherObs", per_page = 5000, verbose = FALSE))
      if (!w$ok) { .check(paste("B", sid, "request works"), FALSE); next }
      d <- w$value
      .check(paste0("B1 ", sid, " request works"), TRUE, paste(nrow(d), "rows"))
      if (!nrow(d)) next
      d$date <- as.Date(substr(d$display_hour_lst, 1, 10)); d$type <- toupper(d$observation_type)
      tab <- table(d$type)
      .info(paste0("B2 ", sid, " rows by observation_type"), paste(names(tab), tab, sep = " = ", collapse = ", "))
      for (ty in names(tab)) {
        x <- d[d$type == ty, ]
        hpd <- table(x$date)
        filled <- vapply(c("temperature", "relative_humidity", "wind_speed", "wind_direction", "hourly_precip", "sol_rad"),
                         function(v) round(100 * mean(!is.na(x[[v]]))), numeric(1))
        .info(paste0("B3 ", sid, " type ", ty),
              sprintf("%s to %s; %s rows/day (min-max); %s days after today; %% filled: %s",
                      min(x$date), max(x$date), paste(range(hpd), collapse = "-"), as.integer(max(x$date) - today),
                      paste(names(filled), filled, sep = " ", collapse = ", ")))
        .info(paste0("B4 ", sid, " type ", ty, " first rows"),
              paste(utils::head(paste(x$display_hour_lst, x$observation_time_lst, round(x$temperature), round(x$relative_humidity)), 4),
                    collapse = " | "))
      }
      if (all(c("O", "F") %in% names(tab))) {
        ov <- intersect(d$display_hour_lst[d$type == "O"], d$display_hour_lst[d$type == "F"])
        .info(paste0("B5 ", sid, " hours with both O and F"), length(ov))
      }
    }

    .section("C. nfdrsObs forecast records, same window")
    sid <- mf$station_id[1]
    n <- .run(paste("nfdrsObs", sid), fems_download_nfdrs(sid, today - 3, today + 10, verbose = FALSE))
    if (n$ok && !is.null(n$value) && nrow(n$value)) {
      v <- n$value
      tab <- table(v$record_type)
      .info("C1 NFDRS rows by type", paste(names(tab), tab, sep = " = ", collapse = ", "))
      if ("F" %in% v$record_type) {
        f <- v[v$record_type == "F", ]
        .info("C2 NFDRS forecast dates and hours", paste(unique(paste(f$date, sprintf("%02d", f$hour))), collapse = ", "))
      }
    } else .info("C1 NFDRS", "no rows or request failed")

    .section("D. Current-year top-up")
    man <- tryCatch(utils::read.csv("data/stations_bundled/manifest.csv", stringsAsFactors = FALSE,
                                    colClasses = c(station_id = "character")), error = function(e) NULL)
    end <- if (!is.null(man) && "end" %in% names(man)) as.Date(man$end[man$station_id == sid][1]) else today - 30
    if (is.na(end)) end <- today - 30
    t <- .run("top-up request", fems_download_weather(sid, end + 1, today, verbose = FALSE))
    .check("D1 a snapshot-end-to-today request works", t$ok,
           sprintf("%s: %s to %s, %s rows in %s s", sid, end + 1, today,
                   if (t$ok && !is.null(t$value)) nrow(t$value) else 0, t$secs))
  }, error = function(e) {
    .T$abort <- paste("test aborted:", conditionMessage(e))
    .say(""); .say("ABORTED: ", conditionMessage(e))
  })
  .test_finish()
})
