# ==============================================================================
# tests/test_download_por.R -- full-record download diagnostics, one station
#
# Downloads the station's whole baseline record (2005-2025 weather) through the
# app's download layer and checks that the data are what the app assumes:
# structure, completeness, units, time of day, and sensor behaviour.
#
# Run from the project root with PrescribedFirePlanner.Rproj open:
#     source("tests/test_download_por.R")
# Change `station_id` below to test another station. Takes about a minute.
#
# Output in tests/logs/ (gitignored):
#   test_download_por_<id>_<time>.txt                 transcript, ends in VERDICT
#   test_download_por_<id>_<time>_by_year.csv         rows and % reporting per year
#   test_download_por_<id>_<time>_variables.csv       quantiles per variable
#   test_download_por_<id>_<time>_month_hour.csv      mean T/RH/wind/solar by month x hour
#   test_download_por_<id>_<time>_solar_timing.csv    solar centroid vs solar noon by month
#   test_download_por_<id>_<time>_solar_timing_by_year.csv   the same, one row per year
#   test_download_por_<id>_<time>_morning_jump.csv    largest hourly T rise vs sunrise by month
#
# PASS/FAIL = the data break an assumption the app relies on (verdict).
# OK/WARN   = a data-quality finding about this station (listed, not failed).
# INFO      = diagnostics to read.
# ==============================================================================

local({
  source("tests/helpers.R", local = TRUE)

  # ---- settings --------------------------------------------------------------
  station_id <- "21202"   # SAGUARO
  years <- if (exists("RX_BASELINE_YEARS")) RX_BASELINE_YEARS else 2005:2025
  start_date <- as.Date(sprintf("%d-01-01", min(years)))
  end_date   <- as.Date(sprintf("%d-12-31", max(years)))
  probe_year <- max(years)   # raw-offset probes in Jan and Jul of this year

  vars <- c("temperature", "relativeHumidity", "windSpeed", "windDirection",
            "gustSpeed", "gustDirection", "precipitation", "solarRadiation", "vpd")
  units <- c(temperature = "F", relativeHumidity = "%", windSpeed = "mph",
             windDirection = "deg", gustSpeed = "mph", gustDirection = "deg",
             precipitation = "in/hr", solarRadiation = "W/m2", vpd = "kPa")

  .T <- .test_open(paste0("test_download_por_", station_id))

  tryCatch({
    .env_header("FEMS full-record download diagnostics",
                c(paste0("Station:  ", station_id),
                  paste0("Range:    ", start_date, " to ", end_date)))

    # ---- A. setup and metadata -----------------------------------------------
    .section("A. Setup and station metadata")
    .setup_layer("A")
    meta <- .metadata_row(station_id)
    .check("A5 station is in data/station_metadata_FEMS3_042225.csv", !is.null(meta))
    if (is.null(meta)) stop("station not in metadata")
    tzo <- .tz_offsets(meta$tz, probe_year)
    std_h <- .offset_hours(tzo$std)
    .info("A6 metadata", sprintf("%s, %s; lat %.4f lon %.4f; %s ft; tz %s (%s); status %s",
                                 meta$name, meta$state, meta$lat, meta$lon, meta$elev_ft,
                                 meta$tz, meta$tz_abbrev, meta$status))
    .info("A7 expected UTC offsets from tz", paste0("Jan ", tzo$jan, ", Jul ", tzo$jul,
                                                    if (tzo$dst) " (zone observes DST)" else " (no DST)"))
    .info("A8 metadata period of record start / avg annual precip",
          paste0(meta$por_start, " / ", meta$avg_precip_in, " in"))

    # ---- B. download and structure -------------------------------------------
    .section("B. Download and structure")
    rb <- .run("weather download", fems_download_weather(station_id, start_date, end_date))
    raw <- rb$value
    got <- rb$ok && is.data.frame(raw) && nrow(raw) > 0
    .check("B1 download returns rows", got, if (got) paste(nrow(raw), "rows") else "nothing returned")
    if (!got) stop("no data")
    .info("B1b download speed", paste0(rb$secs, " s, ", round(nrow(raw) / max(rb$secs, 0.1)), " rows/s"))
    if (!.check_wx_contract(raw, "B2", station_id)) stop("download contract broken")

    rt <- table(raw$record_type)
    .info("B3 record_type counts", paste(names(rt), rt, sep = "=", collapse = ", "))
    .check("B4 record_type values are O or F only", all(raw$record_type %in% c("O", "F")))

    outside <- raw[raw$date < start_date | raw$date > end_date, ]
    .info("B5 rows labelled outside the requested dates (UTC request window)",
          if (nrow(outside)) paste0(nrow(outside), " rows, dates ",
                                    paste(unique(outside$date), collapse = ", ")) else "none")
    wx <- raw[raw$date >= start_date & raw$date <= end_date & raw$record_type == "O", ]
    wx <- wx[order(wx$date, wx$hour), ]
    wx$year  <- as.integer(format(wx$date, "%Y"))
    wx$month <- as.integer(format(wx$date, "%m"))
    .info("B6 rows analysed (observed, inside range)", nrow(wx))

    hpd <- tapply(wx$hour, wx$date, length)
    short <- names(hpd)[hpd != 24]
    short <- setdiff(short, as.character(c(end_date)))   # last day affected by the UTC window bug
    .qc("B7 every local date has 24 rows (gap-filled grid)", length(short) == 0,
        if (length(short)) paste0(length(short), " dates differ, first: ",
                                  paste(head(short, 5), collapse = ", ")) else "")
    yrs_present <- sort(unique(wx$year))
    .qc("B8 every baseline year is present", setequal(yrs_present, years),
        if (setequal(yrs_present, years)) "" else
          paste0("missing: ", paste(setdiff(years, yrs_present), collapse = ",")))

    # ---- C. completeness -----------------------------------------------------
    .section("C. Completeness (rows exist for every hour; values may be NA)")
    by_year <- do.call(rbind, lapply(split(wx, wx$year), function(d) {
      out <- data.frame(year = d$year[1], rows = nrow(d),
                        expected = 24 * (365 + ((d$year[1] %% 4 == 0 & d$year[1] %% 100 != 0) | d$year[1] %% 400 == 0)))
      for (v in vars) out[[paste0("pct_", v)]] <- .pct(mean(!is.na(d[[v]])))
      out$precip_total_in <- round(sum(d$precipitation, na.rm = TRUE), 2)
      out
    }))
    .say("    % of hours with a value, by year:")
    .show(by_year[c("year", "rows", "expected", "pct_temperature", "pct_relativeHumidity",
                    "pct_windSpeed", "pct_solarRadiation", "pct_precipitation", "precip_total_in")])
    .write_csv(by_year, "by_year")

    overall <- vapply(vars, function(v) .pct(mean(!is.na(wx[[v]]))), numeric(1))
    .info("C1 % of hours with a value, whole record", paste(names(overall), overall, sep = "=", collapse = ", "))
    low_years <- by_year$year[by_year$pct_temperature < 80]
    .qc("C2 every year has temperature for >= 80% of hours", length(low_years) == 0,
        if (length(low_years)) paste("low years:", paste(low_years, collapse = ",")) else "")
    ym <- tapply(!is.na(wx$temperature), format(wx$date, "%Y-%m"), mean)
    bad_ym <- names(ym)[ym < 0.5]
    .qc("C3 no month has temperature for < 50% of hours", length(bad_ym) == 0,
        if (length(bad_ym)) paste0(length(bad_ym), " months, e.g. ", paste(head(bad_ym, 6), collapse = ", ")) else "")
    for (v in c("temperature", "relativeHumidity", "windSpeed", "solarRadiation", "precipitation")) {
      d <- wx$date[!is.na(wx[[v]])]
      .info(paste0("C4 first/last value: ", v),
            if (length(d)) paste(min(d), "to", max(d)) else "never reported")
    }
    gaps <- rle(is.na(wx$temperature))
    ends <- cumsum(gaps$lengths); starts <- ends - gaps$lengths + 1
    gi <- which(gaps$values)
    if (length(gi)) {
      top <- gi[order(-gaps$lengths[gi])][seq_len(min(5, length(gi)))]
      gtab <- data.frame(start = paste(wx$date[starts[top]], sprintf("%02d:00", wx$hour[starts[top]])),
                         hours = gaps$lengths[top], days = round(gaps$lengths[top] / 24, 1))
      .info("C5 longest temperature gaps", ""); .show(gtab)
    } else .info("C5 longest temperature gaps", "none")

    # ---- D. units and physical plausibility ---------------------------------
    .section("D. Units and plausibility")
    vs <- do.call(rbind, lapply(vars, function(v) {
      q <- stats::quantile(wx[[v]], c(0, 0.01, 0.25, 0.5, 0.75, 0.99, 1), na.rm = TRUE, names = FALSE)
      data.frame(variable = v, units = units[[v]], n = sum(!is.na(wx[[v]])),
                 min = q[1], p01 = q[2], p25 = q[3], p50 = q[4], p75 = q[5], p99 = q[6], max = q[7])
    }))
    vs[-(1:3)] <- round(vs[-(1:3)], 2)
    .show(vs); .write_csv(vs, "variables")

    t <- wx$temperature
    .check("D1 temperature within -40..130", all(t >= -40 & t <= 130, na.rm = TRUE), .fmt_q(t))
    p99t <- stats::quantile(t, 0.99, na.rm = TRUE)
    .check("D2 temperature is deg F (p99 between 50 and 125; deg C would be below 50)",
           p99t >= 50 && p99t <= 125, paste("p99 =", round(p99t, 1)))

    rh <- wx$relativeHumidity
    .check("D3 RH within 0..100 %", all(rh >= 0 & rh <= 100, na.rm = TRUE), .fmt_q(rh))
    .info("D3b RH at the ends of the scale", paste0(.pct(mean(rh >= 100, na.rm = TRUE), 2), "% at 100, ",
                                                    .pct(mean(rh <= 2, na.rm = TRUE), 2), "% at or below 2"))

    ws <- wx$windSpeed; gs <- wx$gustSpeed
    .check("D4 wind speed >= 0 and < 120 mph", all(ws >= 0 & ws < 120, na.rm = TRUE), .fmt_q(ws))
    both <- !is.na(ws) & !is.na(gs)
    g_ok <- mean(gs[both] >= ws[both])
    .qc("D5 gust >= wind speed in >= 95% of hours", g_ok >= 0.95, paste0(.pct(g_ok), "%"))
    .info("D5b median gust / wind ratio (wind > 2 mph)",
          round(stats::median(gs[both & ws > 2] / ws[both & ws > 2]), 2))

    wd <- wx$windDirection
    .check("D6 wind direction within 0..360 deg", all(wd >= 0 & wd <= 360, na.rm = TRUE), .fmt_q(wd, digits = 0))
    calm <- !is.na(ws) & ws == 0
    .info("D6b direction coding", paste0(.pct(mean(wd == 0, na.rm = TRUE), 2), "% at 0, ",
                                         .pct(mean(wd == 360, na.rm = TRUE), 2), "% at 360; calm hours ",
                                         .pct(mean(calm)), "%, their directions: ", .fmt_q(wd[calm], digits = 0)))
    oct <- cut((wd + 22.5) %% 360, seq(0, 360, 45), labels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW"),
               include.lowest = TRUE)
    ot <- round(100 * prop.table(table(oct[!calm])), 1)
    .info("D6c direction share by octant (non-calm, %)", paste(names(ot), ot, sep = "=", collapse = " "))

    pr <- wx$precipitation
    .check("D7 precipitation has no negative values", all(pr >= 0, na.rm = TRUE), .fmt_q(pr, digits = 2))
    .check("D8 hourly precipitation < 6 in (larger means mm or an accumulating counter)",
           all(pr < 6, na.rm = TRUE), paste("max =", round(max(pr, na.rm = TRUE), 2)))
    .info("D8b hours with precipitation > 0", paste0(.pct(mean(pr > 0, na.rm = TRUE), 2), "%"))
    good_p <- by_year$pct_precipitation >= 80
    if (any(good_p) && is.finite(meta$avg_precip_in)) {
      ratio <- stats::median(by_year$precip_total_in[good_p]) / meta$avg_precip_in
      .check("D9 annual precip totals match metadata average (ratio 0.4..2.5 => inches)",
             ratio >= 0.4 && ratio <= 2.5,
             paste0("median annual ", round(stats::median(by_year$precip_total_in[good_p]), 2),
                    " in vs metadata ", meta$avg_precip_in, " in (ratio ", round(ratio, 2), ")"))
    } else .info("D9 annual precip vs metadata", "not enough complete years or no metadata average")

    sr <- wx$solarRadiation
    .check("D10 solar radiation within 0..1500 W/m2", all(sr >= 0 & sr <= 1500, na.rm = TRUE), .fmt_q(sr, digits = 0))
    night <- wx$hour %in% c(0, 1, 2, 23)
    nmed <- stats::median(sr[night], na.rm = TRUE)
    .check("D11 solar radiation ~0 at night (median of 23-02 LST <= 10 W/m2)", is.finite(nmed) && nmed <= 10,
           paste("night median =", round(nmed, 1), "; night p99 =",
                 round(stats::quantile(sr[night], 0.99, na.rm = TRUE), 1)))

    .check("D12 vpd within 0..15 kPa", all(wx$vpd >= 0 & wx$vpd <= 15, na.rm = TRUE), .fmt_q(wx$vpd, digits = 2))
    dp_ok <- mean(wx$dewpoint <= wx$temperature + 0.5, na.rm = TRUE)
    .qc("D13 dewpoint <= temperature in >= 99% of hours", dp_ok >= 0.99, paste0(.pct(dp_ok, 2), "%"))

    # ---- E. time of day ------------------------------------------------------
    .section("E. Time of day")
    probe <- function(m) {
      d0 <- as.Date(sprintf("%d-%02d-10", probe_year, m))
      fems_gql_paged(.Q_WEATHER,
                     variables = list(startDateTimeRange = paste0(format(d0), "T00:00:00Z"),
                                      endDateTimeRange   = paste0(format(d0 + 6), "T23:59:59Z"),
                                      stationIds = station_id, hasHistoricData = "ALL",
                                      sortBy = "observation_time", sortOrder = "asc"),
                     root = "weatherObs", per_page = 5000, verbose = FALSE)
    }
    pj <- .run("raw probe January", probe(1)); pl <- .run("raw probe July", probe(7))
    oj <- if (pj$ok && nrow(pj$value)) unique(.offset_of(pj$value$display_hour_lst)) else character(0)
    ol <- if (pl$ok && nrow(pl$value)) unique(.offset_of(pl$value$display_hour_lst)) else character(0)
    .check("E1 January offsets are the zone's standard offset",
           length(oj) == 1 && oj == tzo$std, paste0("seen ", paste(oj, collapse = ","), ", expected ", tzo$std))
    .check("E2 July offsets are a single value", length(ol) == 1, paste("seen", paste(ol, collapse = ",")))
    .info("E3 daylight-saving convention",
          if (!tzo$dst) "zone has no DST; nothing to learn here"
          else if (length(ol) == 1 && ol == tzo$std) "FEMS local station time stays on STANDARD time in summer"
          else if (length(ol) == 1 && ol == tzo$jul) "FEMS local station time FOLLOWS daylight saving time"
          else "unclear")

    # Each variable averaged on its own, so a gap in one doesn't drop the others.
    mh <- expand.grid(hour = 0:23, month = 1:12)[c("month", "hour")]
    key <- paste(wx$month, wx$hour)
    for (v in c("temperature", "relativeHumidity", "windSpeed", "solarRadiation")) {
      m <- tapply(wx[[v]], key, mean, na.rm = TRUE)
      mh[[v]] <- round(as.numeric(m[paste(mh$month, mh$hour)]), 2)
    }
    .write_csv(mh, "month_hour")

    mid_doy <- c(15, 46, 74, 105, 135, 166, 196, 227, 258, 288, 319, 349)
    st <- do.call(rbind, lapply(1:12, function(m) {
      d <- wx[wx$month == m, ]
      cen <- .solar_centroid(d$hour, d$solarRadiation)
      noon <- .solar_noon_lst(meta$lon, std_h, mid_doy[m])
      data.frame(month = m, solar_noon_lst = round(noon, 2), centroid_hour = round(cen, 2),
                 centroid_minus_noon = round(cen - noon, 2))
    }))
    .say("    solar-radiation centroid vs computed solar noon (local standard time):")
    .show(st); .write_csv(st, "solar_timing")
    .check("E4 solar centroid within 1.5 h of solar noon in every month (catches shifted clocks)",
           all(abs(st$centroid_minus_noon) <= 1.5, na.rm = TRUE) && sum(is.finite(st$centroid_minus_noon)) >= 10,
           paste("range", paste(round(range(st$centroid_minus_noon, na.rm = TRUE), 2), collapse = " to "), "h"))
    .info("E5 how the hour label relates to the sun",
          paste0("median centroid - noon = ", round(stats::median(st$centroid_minus_noon, na.rm = TRUE), 2),
                 " h (~0: label is mid-period or instantaneous; ~+0.5: label marks the END of an hourly average)"))
    .info("E6 summer minus winter shift (Jun-Aug vs Dec-Feb)",
          paste0(round(mean(st$centroid_minus_noon[6:8]) - mean(st$centroid_minus_noon[c(12, 1, 2)]), 2),
                 " h (~+1 would mean hour labels follow DST)"))

    # Same timing, one value per year: a change in how FEMS labels hours at
    # some point in the record would show up as a step here.
    sty <- do.call(rbind, lapply(sort(unique(wx$year)), function(y) {
      dy <- wx[wx$year == y, ]
      d <- vapply(1:12, function(m) {
        dm <- dy[dy$month == m, ]
        if (sum(!is.na(dm$solarRadiation)) < 24 * 10) return(NA_real_)
        .solar_centroid(dm$hour, dm$solarRadiation) - .solar_noon_lst(meta$lon, std_h, mid_doy[m])
      }, numeric(1))
      ok <- is.finite(d)
      data.frame(year = y, months = sum(ok),
                 mean_centroid_minus_noon = if (any(ok)) round(mean(d[ok]), 2) else NA_real_,
                 min_month = if (any(ok)) round(min(d[ok]), 2) else NA_real_,
                 max_month = if (any(ok)) round(max(d[ok]), 2) else NA_real_)
    }))
    .say("    solar centroid minus solar noon, by year (h):"); .show(sty)
    .write_csv(sty, "solar_timing_by_year")
    dev <- abs(sty$mean_centroid_minus_noon - stats::median(sty$mean_centroid_minus_noon, na.rm = TRUE))
    .check("E6b hour-label timing is steady across years (each year within 0.5 h of the median)",
           sum(is.finite(dev)) >= 1 && all(dev <= 0.5, na.rm = TRUE),
           paste0("largest deviation ", round(max(dev, na.rm = TRUE), 2), " h in ",
                  sty$year[which.max(dev)]))

    peak <- do.call(rbind, lapply(1:12, function(m) {
      d <- mh[mh$month == m, ]
      data.frame(month = m, warmest = d$hour[which.max(d$temperature)],
                 driest = d$hour[which.min(d$relativeHumidity)], coolest = d$hour[which.min(d$temperature)])
    }))
    .say("    hour of warmest / driest / coolest monthly mean:"); .show(peak)
    .check("E7 warmest hour falls 11..17 LST in every month", all(peak$warmest %in% 11:17))
    .check("E8 driest hour falls 11..18 LST in every month", all(peak$driest %in% 11:18))

    # Morning temperature jump (open item): does the largest hourly warming
    # follow sunrise through the year (physical) or sit at a fixed hour (artifact)?
    wx$dT  <- c(NA, diff(wx$temperature))
    wx$dRH <- c(NA, diff(wx$relativeHumidity))
    consecutive <- c(FALSE, diff(as.numeric(wx$date) * 24 + wx$hour) == 1)
    wx$dT[!consecutive] <- NA; wx$dRH[!consecutive] <- NA
    jump <- do.call(rbind, lapply(1:12, function(m) {
      d <- wx[wx$month == m, ]
      mdT  <- tapply(d$dT, d$hour, mean, na.rm = TRUE)
      mdRH <- tapply(d$dRH, d$hour, mean, na.rm = TRUE)
      h <- as.integer(names(mdT))[which.max(mdT)]
      data.frame(month = m, sunrise_lst = round(.sunrise_lst(meta$lat, meta$lon, std_h, mid_doy[m]), 2),
                 hour_of_max_rise = h, mean_dT_F = round(max(mdT, na.rm = TRUE), 2),
                 mean_dRH_pct = round(mdRH[as.character(h)], 2),
                 hours_after_sunrise = round(h - .sunrise_lst(meta$lat, meta$lon, std_h, mid_doy[m]), 2))
    }))
    .say("    largest mean hour-to-hour warming by month (hour h = change from h-1 to h):"); .show(jump)
    .write_csv(jump, "morning_jump")
    spread <- diff(range(jump$hours_after_sunrise))
    .info("E9 morning jump follows sunrise?",
          paste0("hours-after-sunrise ranges over ", round(spread, 2),
                 " h across months (small = tracks sunrise, physical; large while hour_of_max_rise stays fixed = clock artifact)"))

    # ---- F. sensor behaviour -------------------------------------------------
    .section("F. Sensor behaviour (stuck values, spikes)")
    run_msg <- function(r) if (r$len > 0) paste0(r$len, " h at ", round(r$value, 2), " starting ",
                                                 wx$date[r$start], sprintf(" %02d:00", wx$hour[r$start])) else "none"
    rt_ <- .longest_run(wx$temperature)
    .qc("F1 temperature never stuck >= 12 h", rt_$len < 12, run_msg(rt_))
    rh_x <- wx$relativeHumidity; rh_x[rh_x >= 100] <- NA   # saturation can persist legitimately
    rr <- .longest_run(rh_x)
    .qc("F2 RH (below 100) never stuck >= 12 h", rr$len < 12, run_msg(rr))
    ws_x <- wx$windSpeed; ws_x[ws_x == 0] <- NA
    rw <- .longest_run(ws_x)
    .qc("F3 non-calm wind speed never stuck >= 12 h", rw$len < 12, run_msg(rw))
    rc <- .longest_run(ifelse(wx$windSpeed == 0, 0, NA))
    .info("F4 longest calm spell", run_msg(rc))
    sr_x <- wx$solarRadiation; sr_x[!(wx$hour %in% 9:15)] <- NA
    rs <- .longest_run(sr_x)
    .qc("F5 midday solar never stuck >= 6 h", rs$len < 6, run_msg(rs))
    big <- which(abs(wx$dT) > 25)
    .qc("F6 fewer than 10 hour-to-hour temperature changes > 25 F", length(big) < 10,
        paste0(length(big), " found",
               if (length(big)) paste0(", e.g. ", paste(head(paste(wx$date[big], sprintf("%02d", wx$hour[big])), 3),
                                                         collapse = "; ")) else ""))
    bigrh <- which(abs(wx$dRH) > 50)
    .qc("F7 fewer than 10 hour-to-hour RH changes > 50 %", length(bigrh) < 10, paste(length(bigrh), "found"))

  }, error = function(e) {
    .T$abort <- paste("test aborted:", conditionMessage(e))
    .say(""); .say("ABORTED: ", conditionMessage(e))
  })

  .test_finish()
})
