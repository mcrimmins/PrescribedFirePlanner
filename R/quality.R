# ==============================================================================
# R/quality.R -- the Data quality report for a loaded station
#
# Written once; used by the Data quality tab and by the tests. Every item says
# what was FOUND, what the app DID about it (action), and what it AFFECTS,
# with a severity:
#   "affects"  can change results (amber; counted in the sidebar badge)
#   "info"     worth knowing, doesn't change results (grey)
#
#   rep <- quality_report(st)       # st from hourly_load_station()
#   quality_badge(rep)              # number of "affects" items
#
# Building blocks (also used directly by tests and the tab's tables):
#   quality_coverage(h)   % of hours with a value, per year x variable
#   quality_gaps(h)       longest runs of missing temperature
#   quality_timing(h, meta)  per-year daily-cycle timing for each variable
#   quality_steps(timing)    the step year/size for each variable, if any
#   quality_precip(h, meta)  annual precipitation vs the metadata average
#
# Requires R/constants.R and R/sun.R.
# ==============================================================================

.q_label <- function(v) {
  lab <- RX_VARS$label[match(v, RX_VARS$var)]
  extra <- c(gustSpeed = "Gust speed", gustDirection = "Gust direction", dewpoint = "Dewpoint",
             vpd = "VPD", hdw = "Hot-dry-windy index")
  ifelse(is.na(lab), ifelse(v %in% names(extra), extra[v], v), lab)
}
.q_units <- function(v) {
  u <- RX_VARS$units[match(v, RX_VARS$var)]
  ifelse(is.na(u), ifelse(v == "gustSpeed", "mph", ""), u)
}
# "12 Jul 2008 08:00" without depending on the locale's month names
.q_when <- function(date, hour = NULL) {
  lt <- as.POSIXlt(date)
  s <- sprintf("%d %s %d", lt$mday, month.abb[lt$mon + 1L], lt$year + 1900L)
  if (!is.null(hour)) s <- paste0(s, sprintf(" %02d:00", as.integer(hour)))
  s
}
.q_item <- function(section, severity, variable, found, action, affects, start = as.Date(NA), end = as.Date(NA)) {
  data.frame(section = section, severity = severity, variable = variable, start = start, end = end,
             found = found, action = action, affects = affects, stringsAsFactors = FALSE)
}

# ---- coverage -----------------------------------------------------------------------
QUALITY_COVERAGE_VARS <- c("temperature", "relativeHumidity", "windSpeed", "windDirection",
                           "precipitation", "solarRadiation")

quality_coverage <- function(h, vars = QUALITY_COVERAGE_VARS) {
  yrs <- sort(unique(h$year))
  do.call(rbind, lapply(vars, function(v) {
    nv <- tapply(!is.na(h[[v]]), h$year, sum); nh <- tapply(h[[v]], h$year, length)
    data.frame(year = as.integer(names(nh)), variable = v, hours = as.integer(nh),
               n_valid = as.integer(nv), pct = round(100 * as.numeric(nv) / as.numeric(nh), 1),
               stringsAsFactors = FALSE)
  }))
}

quality_gaps <- function(h, var = "temperature", n = 5) {
  r <- rle(is.na(h[[var]])); ends <- cumsum(r$lengths); starts <- ends - r$lengths + 1L
  g <- which(r$values)
  if (!length(g)) return(data.frame(start_date = as.Date(character(0)), start_hour = integer(0), hours = integer(0)))
  g <- g[order(-r$lengths[g])][seq_len(min(n, length(g)))]
  data.frame(start_date = h$date[starts[g]], start_hour = h$hour[starts[g]], hours = r$lengths[g])
}

# ---- timing ---------------------------------------------------------------------------
# Hour (0-24) at which the first harmonic of the mean daily cycle peaks.
.q_peak_hour <- function(hour, x) {
  m <- tapply(x, hour, mean, na.rm = TRUE)
  hh <- as.numeric(names(m)); m <- as.numeric(m); ok <- is.finite(m)
  if (sum(ok) < 20) return(NA_real_)
  a <- sum(m[ok] * cos(2 * pi * hh[ok] / 24)); b <- sum(m[ok] * sin(2 * pi * hh[ok] / 24))
  (atan2(b, a) * 24 / (2 * pi)) %% 24
}
# Solar-weighted mean hour minus solar noon, averaged over months with data.
.q_solar_offset <- function(d, meta) {
  mid <- c(15, 46, 74, 105, 135, 166, 196, 227, 258, 288, 319, 349)
  vals <- vapply(1:12, function(m) {
    dm <- d[d$month == m, ]
    if (sum(!is.na(dm$solarRadiation)) < 24 * 10) return(NA_real_)
    mh <- tapply(dm$solarRadiation, dm$hour, mean, na.rm = TRUE)
    hh <- as.numeric(names(mh)); mh <- as.numeric(mh); ok <- is.finite(mh)
    if (sum(ok) < 12) return(NA_real_)
    w <- pmax(mh[ok] - min(mh[ok]), 0)
    if (sum(w) <= 0) return(NA_real_)
    sum(hh[ok] * w) / sum(w) - sun_noon_lst(meta$lon, meta$std_offset_h, mid[m])
  }, numeric(1))
  if (sum(is.finite(vals)) < 6) NA_real_ else mean(vals, na.rm = TRUE)
}

# Per year and variable: when the daily cycle peaks (temperature, RH, wind:
# peak hour) or where solar radiation sits relative to solar noon (h).
# Years where the variable reports < QUALITY_COVERAGE_MIN % are left out.
quality_timing <- function(h, meta, vars = QUALITY_TIMING_VARS) {
  cov <- quality_coverage(h, vars)
  out <- list()
  for (y in sort(unique(h$year))) {
    d <- h[h$year == y, ]
    for (v in vars) {
      pc <- cov$pct[cov$year == y & cov$variable == v]
      val <- if (!length(pc) || pc < QUALITY_COVERAGE_MIN) NA_real_
             else if (v == "solarRadiation") .q_solar_offset(d, meta)
             else .q_peak_hour(d$hour, d[[v]])
      out[[length(out) + 1L]] <- data.frame(year = y, variable = v, value = val, stringsAsFactors = FALSE)
    }
  }
  do.call(rbind, out)
}

# Best single step in a yearly series (the split that minimises squared
# error around two flat levels). Returns the first year of the later level.
quality_step <- function(years, x, min_side = QUALITY_STEP_MIN_SIDE) {
  ok <- is.finite(x); years <- years[ok]; x <- x[ok]; n <- length(x)
  none <- list(year = NA_integer_, step = NA_real_, spread = NA_real_, detected = FALSE)
  if (n < 2 * min_side) return(none)
  best <- NULL
  for (k in seq(min_side, n - min_side)) {
    a <- x[seq_len(k)]; b <- x[(k + 1):n]
    sse <- sum((a - mean(a))^2) + sum((b - mean(b))^2)
    if (is.null(best) || sse < best$sse) best <- list(k = k, sse = sse, step = mean(b) - mean(a))
  }
  spread <- sqrt(best$sse / max(n - 2, 1))
  list(year = as.integer(years[best$k + 1]), step = best$step, spread = spread,
       detected = abs(best$step) >= QUALITY_STEP_H && abs(best$step) >= QUALITY_STEP_SPREAD * spread)
}

quality_steps <- function(timing) {
  do.call(rbind, lapply(unique(timing$variable), function(v) {
    t <- timing[timing$variable == v, ]
    s <- quality_step(t$year, t$value)
    data.frame(variable = v, step_year = s$year, step_h = round(s$step, 2), spread_h = round(s$spread, 2),
               detected = s$detected, stringsAsFactors = FALSE)
  }))
}

# ---- precipitation ------------------------------------------------------------------------
quality_precip <- function(h, meta) {
  cov <- quality_coverage(h, "precipitation")
  tot <- tapply(h$precipitation, h$year, sum, na.rm = TRUE)
  d <- data.frame(year = as.integer(names(tot)), total_in = round(as.numeric(tot), 2))
  d$pct_reporting <- cov$pct[match(d$year, cov$year)]
  d$complete <- d$pct_reporting >= QUALITY_COVERAGE_MIN & d$year %in% RX_BASELINE_YEARS
  d$vs_avg <- if (is.finite(meta$avg_precip_in) && meta$avg_precip_in > 0) round(d$total_in / meta$avg_precip_in, 2) else NA_real_
  d
}

# ---- the report -----------------------------------------------------------------------------
quality_report <- function(st) {
  h <- st$hourly; meta <- st$meta; items <- list()
  add <- function(...) items[[length(items) + 1L]] <<- .q_item(...)
  base <- h[h$year %in% RX_BASELINE_YEARS, ]

  # source
  add("source", "info", NA_character_,
      paste0("Data: ", st$source$detail, "; FEMS weather from ", .q_when(min(h$date)), " to ", .q_when(max(h$date)), "."),
      if (identical(st$source$type, "bundled")) "Use Refresh from FEMS to load the live record." else "Downloaded live from FEMS.",
      "Nothing, unless FEMS has revised its data since the snapshot.")

  # observation minute
  om <- table(h$obs_minute)
  if (length(om)) {
    top <- as.integer(names(om)[which.max(om)])
    add("timing", "info", NA_character_,
        if (length(om) == 1) sprintf("Observations are taken at %d minutes past each hour (the 13:00 row is the 13:%02d reading).", top, top)
        else sprintf("Observations are mostly taken at %d minutes past the hour (also: %s).", top,
                     paste(setdiff(names(om), as.character(top)), collapse = ", ")),
        "Hours are labelled with the hour of the observation.",
        "Comparing stations: the same labelled hour can be up to an hour apart in real time.")
  }

  # grid completion
  if (isTRUE(st$grid_added > 0)) {
    add("coverage", "info", NA_character_,
        sprintf("FEMS sent nothing for %s hours (it leaves gaps out of recent data instead of marking them missing).",
                format(st$grid_added, big.mark = ",")),
        "Those hours were added as missing.",
        "They count as excluded hours; nothing else.")
  }

  # screening log, grouped: one item per rule x variable (the full run-by-run
  # log is st$qc_log, shown as a table on the tab)
  ql <- st$qc_log
  if (nrow(ql)) {
    ql$end_date <- ql$start_date + floor((ql$start_hour + ql$hours - 1) / 24)
    grp <- unique(ql[c("rule", "variable")])
    for (g in seq_len(nrow(grp))) {
      r <- ql[ql$rule == grp$rule[g] & ql$variable == grp$variable[g], ]
      v <- grp$variable[g]; lab <- .q_label(v); u <- .q_units(v)
      k <- which.max(r$hours); n <- nrow(r); tot <- sum(r$hours)
      span <- if (n > 1) sprintf(" between %s and %s", .q_when(min(r$start_date)), .q_when(max(r$end_date))) else ""
      if (grp$rule[g] == "stuck") {
        val <- paste0(format(r$value[k]), if (u == "%") "%" else paste0(" ", u))
        found <- if (n == 1) sprintf("%s stuck at %s for %d h from %s.", lab, val, r$hours[k],
                                     .q_when(r$start_date[k], r$start_hour[k]))
                 else sprintf("%s stuck at one value %d times%s (%s h in all); longest %d h at %s from %s.",
                              lab, n, span, format(tot, big.mark = ","), r$hours[k], val,
                              .q_when(r$start_date[k], r$start_hour[k]))
        aff <- if (v == "relativeHumidity")
          "Otherwise those hours would count as in prescription for any RH maximum (or out, for an RH minimum)."
          else "Otherwise those hours would be judged on a false value; they're now excluded hours."
        add("screening", "affects", v, found, "Removed (treated as missing).", aff,
            start = min(r$start_date), end = max(r$end_date))
      } else if (grp$rule[g] == "long calm") {
        found <- if (n == 1) sprintf("Wind reported calm for %d h from %s, longer than a real calm usually lasts.",
                                     r$hours[k], .q_when(r$start_date[k], r$start_hour[k]))
                 else sprintf("Wind reported calm for more than %d h %d times%s (%s h in all); longest %d h from %s.",
                              QC_CALM_HOURS, n, span, format(tot, big.mark = ","), r$hours[k],
                              .q_when(r$start_date[k], r$start_hour[k]))
        add("screening", "affects", "windSpeed", found,
            "Wind speed, gust and direction removed for those hours.",
            "Otherwise those hours would pass any wind-speed maximum.",
            start = min(r$start_date), end = max(r$end_date))
      } else if (grp$rule[g] == "spike") {
        add("screening", "affects", v,
            sprintf("%d one-hour %s spike%s (first %s).", tot, tolower(lab), if (tot > 1) "s" else "",
                    .q_when(r$start_date[1], r$start_hour[1])),
            "Removed (treated as missing).", "Single bad readings no longer count either way.",
            start = r$start_date[1])
      } else {
        dd <- sum(r$days, na.rm = TRUE)
        add("screening", "affects", v,
            sprintf("%s went past physical limits on %d day%s (first %s), a failing sensor.", lab, dd,
                    if (dd > 1) "s" else "", .q_when(r$start_date[1], r$start_hour[1])),
            sprintf("Removed %s on those days and %d day%s either side (%s h).", tolower(lab), QC_LIMITS_PAD_DAYS,
                    if (QC_LIMITS_PAD_DAYS > 1) "s" else "", format(tot, big.mark = ",")),
            "Readings from a failing sensor no longer count either way.", start = r$start_date[1])
      }
    }
  }

  # calm share: a site or anemometer that rarely registers light wind
  if (nrow(base)) {
    ws <- base$windSpeed
    share <- 100 * mean(ws == 0, na.rm = TRUE)
    if (is.finite(share) && share > QUALITY_CALM_SHARE) {
      by_y <- tapply(ws == 0, base$year, mean, na.rm = TRUE)
      yy <- as.integer(names(by_y)); first3 <- 100 * mean(by_y[yy <= min(yy) + 2]); last3 <- 100 * mean(by_y[yy >= max(yy) - 2])
      trend <- if (abs(last3 - first3) >= 10) sprintf(", %s from about %.0f%% in %d-%d to %.0f%% in %d-%d",
                                                    if (last3 > first3) "rising" else "falling", first3, min(yy), min(yy) + 2,
                                                    last3, max(yy) - 2, max(yy)) else ""
      add("plausibility", "affects", "windSpeed",
          sprintf("Wind is calm (0 mph) in %.0f%% of baseline hours%s.", share, trend),
          "Not changed; calm hours are kept as calm.",
          "A wind-speed minimum will exclude many hours here. A sheltered site or an anemometer that misses light wind could cause this.")
    }
  }

  # coverage in the baseline
  if (nrow(base)) {
    cov <- quality_coverage(base, c("temperature", "relativeHumidity", "windSpeed"))
    low <- cov[cov$pct < QUALITY_COVERAGE_MIN, ]
    for (v in unique(low$variable)) {
      l <- low[low$variable == v, ]
      add("coverage", "affects", v,
          sprintf("%s reported for under %d%% of hours in %s (%s).", .q_label(v), QUALITY_COVERAGE_MIN,
                  paste(l$year, collapse = ", "), paste0(l$pct, "%", collapse = ", ")),
          "% in prescription is computed only over hours with data.",
          "Those years count for less in the climatology.")
    }
    first <- min(h$date)
    if (first > as.Date(sprintf("%d-01-31", min(RX_BASELINE_YEARS))))
      add("coverage", "info", NA_character_,
          sprintf("The record starts %s, after the baseline start (%d).", .q_when(first), min(RX_BASELINE_YEARS)),
          "The climatology uses the years available.", "Fewer years behind each percentage.")
  }

  # timing steps
  tm <- quality_timing(base, meta)
  stp <- quality_steps(tm)
  for (i in which(stp$detected)) {
    s <- stp[i, ]; lab <- .q_label(s$variable)
    dir <- if (s$step_h > 0) "later" else "earlier"
    what <- if (s$variable == "solarRadiation") "Solar radiation's timing" else paste0(lab, "'s daily cycle")
    add("timing", "affects", s$variable,
        sprintf("%s shifts about %.1f h %s from %d, %s.", what, abs(s$step_h), dir, s$step_year,
                if (s$step_h > 0) "consistent with a switch from a single reading to an hourly average"
                else "probably a change in the sensor, its siting or its processing"),
        "Not corrected.",
        sprintf("Hour-of-day results for %s mix two conventions about %.1f h apart.", tolower(lab), abs(s$step_h)),
        start = as.Date(sprintf("%d-01-01", s$step_year)))
  }

  # precipitation
  pr <- quality_precip(base, meta)
  ok <- pr[pr$complete, ]
  if (nrow(ok) >= 3 && is.finite(meta$avg_precip_in)) {
    ratio <- stats::median(ok$total_in) / meta$avg_precip_in
    med <- stats::median(ok$total_in)
    if (ratio < QUALITY_PRECIP_RATIO[1] || ratio > QUALITY_PRECIP_RATIO[2]) {
      add("plausibility", "affects", "precipitation",
          sprintf("Typical annual precipitation is %.1f in, against %.1f in in the station metadata (ratio %.2f).",
                  med, meta$avg_precip_in, ratio),
          "Not corrected.", "Precipitation limits may not mean what they say (units or sensor problem).")
    } else if (ratio < QUALITY_PRECIP_UNDERCATCH) {
      add("plausibility", "affects", "precipitation",
          sprintf("Measured precipitation is about %.0f%% of the station's listed average (%.1f vs %.1f in a year).",
                  100 * ratio, med, meta$avg_precip_in),
          "Not corrected.",
          "The gauge probably misses snowfall, so snowy hours count as dry under a no-rain limit.")
    }
    ok$vs_med <- if (med > 0) ok$total_in / med else NA_real_
    odd <- ok[which(ok$vs_med < QUALITY_PRECIP_YEAR[1] | ok$vs_med > QUALITY_PRECIP_YEAR[2]), ]
    for (i in seq_len(nrow(odd)))
      add("plausibility", "info", "precipitation",
          sprintf("%d: %.1f in, against a typical %.1f in at this station (%s year).", odd$year[i], odd$total_in[i], med,
                  if (odd$vs_med[i] < 1) "dry" else "wet"),
          "Included.", "A real extreme year, as far as the data show.")
  }

  rep <- do.call(rbind, items)
  rep$severity <- factor(rep$severity, levels = c("affects", "info"))
  sec <- factor(rep$section, levels = c("screening", "timing", "coverage", "plausibility", "source"))
  rep <- rep[order(rep$severity, sec), ]
  rep$severity <- as.character(rep$severity)
  rownames(rep) <- NULL
  rep
}

quality_badge <- function(rep) sum(rep$severity == "affects")
