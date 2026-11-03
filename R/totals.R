# ==============================================================================
# R/totals.R -- hour totals for reporting out (counts, not shares)
#
# Presentation of counts only: the counting itself is clim_summarise()
# (n_true) on columns made by rx_flag() (in_rx, pass_<var>, only_<var>). No
# flagging and no second aggregation here.
#
#   totals_per_year(flagged, rx_vars, period = "month")
#       one row per year x period x measure: hours TRUE, valid, missing
#   totals_summary(per_year)
#       per period x measure: median year, lowest and highest year (named),
#       total over all years, years used / left out, hours excluded.
#       Includes a whole-year row (period "Year").
#   totals_display(summary, rx_vars)   -> list(main, by_limit) of display tables
#       (main: months down; by_limit: a met row and an only-one-failing row per limit, months across)
#   totals_csv_lines(x, meta, rx, years, hour_label, what)  -> lines for a CSV
#   totals_event(flagged, rx_vars)      Past event: per day x measure + Total,
#       with the longest in-prescription run (clim_runs)
#   totals_event_display(), totals_event_csv_lines()  (daily or hourly CSV)
#
# Measures: in_rx (all limits met), pass_<var> (that limit met on its own),
# only_<var> (misses the prescription only because of that limit).
#
# Coverage rule: a year x period with less than TOTALS_COVERAGE_MIN % of its
# hours usable for in_rx is left out of the median and range of EVERY measure
# (so the columns describe the same years); it still counts toward the total.
# Requires R/constants.R and R/climatology.R.
# ==============================================================================

.TOTALS_PERIOD_LABEL <- c(month.abb, "Year")

totals_measures <- function(rx_vars) c("in_rx", paste0("pass_", rx_vars), paste0("only_", rx_vars))

totals_per_year <- function(flagged, rx_vars, period = "month") {
  vals <- totals_measures(rx_vars)
  vals <- vals[vals %in% names(flagged)]
  per <- clim_summarise(flagged, vals, by = c("year", period))
  yr <- clim_summarise(flagged, vals, by = "year")
  per$period <- .TOTALS_PERIOD_LABEL[per[[period]]]
  yr$period <- "Year"
  cols <- c("year", "period", "value", "n_true", "n_valid", "n_missing")
  out <- rbind(per[, cols], yr[, cols])
  out$hours <- out$n_valid + out$n_missing
  # coverage is judged on in_rx (the strictest) and applied to every measure
  ir <- out[out$value == "in_rx", ]
  cov <- stats::setNames(100 * ir$n_valid / ir$hours, paste(ir$year, ir$period))
  out$pct_usable <- unname(cov[paste(out$year, out$period)])
  out$used <- !is.na(out$pct_usable) & out$pct_usable >= TOTALS_COVERAGE_MIN
  out$period <- factor(out$period, levels = .TOTALS_PERIOD_LABEL)
  out <- out[order(out$period, match(out$value, vals), out$year), ]
  rownames(out) <- NULL
  out
}

totals_summary <- function(per_year) {
  key <- paste(per_year$period, per_year$value)
  parts <- split(per_year, factor(key, levels = unique(key)))
  rows <- lapply(parts, function(d) {
    u <- d[d$used, ]
    pick <- function(f) if (nrow(u)) u[f(u$n_true), ] else NULL
    lo <- pick(which.min); hi <- pick(which.max)        # ties: the earliest year (rows are in year order)
    data.frame(period = as.character(d$period[1]), value = d$value[1],
               median = if (nrow(u)) stats::median(u$n_true) else NA_real_,
               low = if (nrow(u)) lo$n_true else NA_real_, low_year = if (nrow(u)) lo$year else NA_integer_,
               high = if (nrow(u)) hi$n_true else NA_real_, high_year = if (nrow(u)) hi$year else NA_integer_,
               total = sum(d$n_true), years_used = nrow(u),
               years_left_out = paste(d$year[!d$used], collapse = ", "),
               hours_excluded = sum(d$n_missing),
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows); rownames(out) <- NULL
  out
}

.totals_h <- function(x) ifelse(is.na(x), "", format(round(x), big.mark = ",", trim = TRUE))

totals_display <- function(summary, rx_vars) {
  ir <- summary[summary$value == "in_rx", ]
  rng <- ifelse(is.na(ir$low), "",
                sprintf("%s (%d) to %s (%d)", .totals_h(ir$low), ir$low_year, .totals_h(ir$high), ir$high_year))
  note <- ifelse(nzchar(ir$years_left_out), paste0(ir$years_used, "*"), as.character(ir$years_used))
  main <- data.frame(ir$period, .totals_h(ir$median), rng, .totals_h(ir$total), note, .totals_h(ir$hours_excluded),
                     stringsAsFactors = FALSE)
  names(main) <- c("Month", "Typical year (median h)", "Lowest to highest year (h)", "Total, all years (h)",
                   "Years used", "Hours excluded")
  # by limit: one row per limit and measure, months across (short numbers, so it stays narrow)
  rows <- list()
  for (v in rx_vars) {
    lab <- RX_VARS$label[RX_VARS$var == v]
    for (m in c("pass", "only")) {
      r <- summary[summary$value == paste0(m, "_", v), ]
      rows[[length(rows) + 1]] <- c(Limit = if (m == "pass") lab else "",
                                    Hours = if (m == "pass") "met" else "only one failing",
                                    stats::setNames(.totals_h(r$median[match(ir$period, r$period)]), ir$period))
    }
  }
  by_limit <- if (length(rows)) as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
              else data.frame(Limit = character(0), Hours = character(0))
  list(main = main, by_limit = by_limit, any_left_out = any(nzchar(ir$years_left_out)))
}

# ---- CSV ------------------------------------------------------------------------
# Text of the prescription, e.g. "Temperature 40 to 85 degF; Relative humidity up to 55 %".
totals_rx_text <- function(rx) {
  vars <- rx_constrained(rx)
  if (!length(vars)) return("none (every hour counts)")
  parts <- vapply(vars, function(v) {
    i <- which(rx$var == v); m <- RX_VARS[RX_VARS$var == v, ]; lo <- rx$min[i]; hi <- rx$max[i]
    rng <- if (v == "precipitation" && is.na(lo) && identical(hi, 0)) "none (0"
           else if (m$kind == "circular") sprintf("from %g to %g, clockwise (calm passes)", lo, hi)
           else if (is.na(lo)) sprintf("up to %g", hi) else if (is.na(hi)) sprintf("at least %g", lo)
           else sprintf("%g to %g", lo, hi)
    if (startsWith(rng, "none (")) paste0(m$label, " ", rng, " ", m$units, ")") else paste(m$label, rng, m$units)
  }, character(1))
  paste(parts, collapse = "; ")
}

totals_measure_label <- function(value) {
  v <- sub("^(pass|only)_", "", value)
  lab <- RX_VARS$label[match(v, RX_VARS$var)]
  ifelse(value == "in_rx", "In prescription (all limits met)",
         ifelse(startsWith(value, "pass_"), paste0(lab, ": limit met on its own"),
                paste0(lab, ": misses only because of this limit")))
}

# Header lines shared by every CSV: they make a saved file explain itself.
.totals_csv_header <- function(title, meta, source_detail, rx, period_line, hour_label, extra = character(0)) {
  c(paste0("# Prescribed Fire Weather Explorer (RxWx) ", APP_VERSION, " -- ", title),
    paste0("# Station: ", meta$name, " (", meta$station_id, ")",
           if (!is.null(meta$state) && !is.na(meta$state)) paste0(", ", meta$state) else ""),
    paste0("# Data: ", source_detail, "; hourly RAWS weather from USDA Forest Service FEMS"),
    paste0("# ", period_line),
    paste0("# Prescription: ", totals_rx_text(rx)),
    paste0("# Hours: ", hour_label, ". Counts are hours. An hour is excluded when a limited variable is missing or was screened out."),
    if (length(extra)) paste0("# ", extra),
    paste0("# Exported: ", format(Sys.time(), "%Y-%m-%d %H:%M %Z"), ". Training tool, not an operational planning product."))
}
.totals_csv_body <- function(d) utils::capture.output(utils::write.csv(d, row.names = FALSE, na = ""))

# x: totals_summary() (what = "summary") or totals_per_year() (what = "per_year").
totals_csv_lines <- function(x, meta, source_detail, rx, years, hour_label, what = c("summary", "per_year")) {
  what <- match.arg(what)
  hdr <- .totals_csv_header(
    paste0("hour totals, ", if (what == "summary") "by month (typical year, range, total)" else "by year and month"),
    meta, source_detail, rx, paste0("Years: ", years[1], "-", years[2]), hour_label,
    paste0("Median year and range leave out a year-month with under ", TOTALS_COVERAGE_MIN,
           "% of its hours usable (listed in years_left_out); those still count toward total_h."))
  if (what == "summary") {
    d <- data.frame(period = x$period, measure = x$value, description = totals_measure_label(x$value),
                    median_year_h = x$median, lowest_h = x$low, lowest_year = x$low_year,
                    highest_h = x$high, highest_year = x$high_year, total_h = x$total,
                    years_used = x$years_used, years_left_out = x$years_left_out, hours_excluded = x$hours_excluded,
                    stringsAsFactors = FALSE)
  } else {
    d <- data.frame(year = x$year, period = as.character(x$period), measure = x$value,
                    description = totals_measure_label(x$value), hours_true = x$n_true,
                    hours_usable = x$n_valid, hours_excluded = x$n_missing, hours_total = x$hours,
                    pct_usable = round(x$pct_usable, 1), in_median = x$used, stringsAsFactors = FALSE)
  }
  c(hdr, .totals_csv_body(d))
}

# ---- Past event: by day --------------------------------------------------------------
# flagged: rx_flag() output for the event's hours. Returns one row per day x
# measure (plus "Total" rows), with the longest in-prescription run per day.
totals_event <- function(flagged, rx_vars, hour_col = "hour") {
  vals <- totals_measures(rx_vars); vals <- vals[vals %in% names(flagged)]
  d <- clim_summarise(flagged, vals, by = "date")
  runs <- clim_runs(flagged, "in_rx", by = "date", hour_col = hour_col)
  out <- data.frame(day = format(d$date, "%Y-%m-%d"), value = d$value, n_true = d$n_true,
                    n_valid = d$n_valid, n_missing = d$n_missing, stringsAsFactors = FALSE)
  i <- match(out$day, format(runs$date, "%Y-%m-%d"))
  ir <- out$value == "in_rx"
  out$longest_run <- ifelse(ir, runs$longest[i], NA_integer_)
  out$run_start <- ifelse(ir, runs$start_hour[i], NA_integer_)
  tot <- stats::aggregate(cbind(n_true, n_valid, n_missing) ~ value, data = out, FUN = sum)
  tot <- data.frame(day = "Total", value = tot$value, n_true = tot$n_true, n_valid = tot$n_valid,
                    n_missing = tot$n_missing, longest_run = NA_integer_, run_start = NA_integer_)
  # the event's longest run may span midnight; the Total row gives the longest single-day run
  tot$longest_run[tot$value == "in_rx"] <- max(c(0L, runs$longest))
  out <- rbind(out, tot[match(vals, tot$value), ])
  rownames(out) <- NULL
  out
}

.totals_day_label <- function(day) ifelse(day == "Total", "Total", format(as.Date(day, optional = TRUE), "%a %d %b"))

totals_event_display <- function(ev, rx_vars, clock = FALSE) {
  ir <- ev[ev$value == "in_rx", ]
  run <- ifelse(is.na(ir$longest_run) | ir$longest_run == 0, "none",
                ifelse(is.na(ir$run_start), paste0(ir$longest_run, " h"),
                       sprintf("%d h, %02d:00-%02d:00", ir$longest_run, ir$run_start, (ir$run_start + ir$longest_run) %% 24)))
  run[ir$day == "Total"] <- paste0(ir$longest_run[ir$day == "Total"], " h (longest day)")
  main <- data.frame(.totals_day_label(ir$day), .totals_h(ir$n_true), run, .totals_h(ir$n_valid),
                     .totals_h(ir$n_missing), stringsAsFactors = FALSE)
  names(main) <- c("Day", "In prescription (h)", paste0("Longest run (", if (clock) "clock time" else "LST", ")"),
                   "Hours with data", "Hours excluded")
  rows <- list()
  for (v in rx_vars) {
    lab <- RX_VARS$label[RX_VARS$var == v]
    for (m in c("pass", "only")) {
      r <- ev[ev$value == paste0(m, "_", v), ]
      rows[[length(rows) + 1]] <- c(Limit = if (m == "pass") lab else "",
                                    Hours = if (m == "pass") "met" else "only one failing",
                                    stats::setNames(.totals_h(r$n_true[match(ir$day, r$day)]),
                                                    ifelse(ir$day == "Total", "Total", format(as.Date(ir$day, optional = TRUE), "%d %b"))))
    }
  }
  by_limit <- if (length(rows)) as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE, check.names = FALSE)
              else data.frame(Limit = character(0), Hours = character(0))
  list(main = main, by_limit = by_limit)
}

# what = "daily": totals_event() rows; what = "hourly": the flagged hours themselves.
totals_event_csv_lines <- function(x, meta, source_detail, rx, dates, hour_label, what = c("daily", "hourly")) {
  what <- match.arg(what)
  hdr <- .totals_csv_header(paste0("past event, ", if (what == "daily") "hour totals by day" else "every hour"),
                            meta, source_detail, rx,
                            paste0("Dates: ", format(dates[1]), " to ", format(dates[2]), " (local standard time days)"),
                            hour_label,
                            if (what == "daily") "longest_run_h: the longest stretch of consecutive in-prescription hours within the day; run_start_hour: its first hour, in the hour convention above."
                            else "pass_<var>: that limit met; only_<var>: misses the prescription only because of that limit; blank = not known (missing or screened).")
  if (what == "daily") {
    d <- data.frame(day = x$day, measure = x$value, description = totals_measure_label(x$value),
                    hours_true = x$n_true, hours_usable = x$n_valid, hours_excluded = x$n_missing,
                    longest_run_h = x$longest_run, run_start_hour = x$run_start, stringsAsFactors = FALSE)
  } else {
    keep <- c("date", "hour", "hour_clock", "obs_minute", "in_rx", grep("^(pass|only)_", names(x), value = TRUE),
              intersect(c("temperature", "relativeHumidity", "windSpeed", "windDirection", "gustSpeed",
                          "precipitation", "solarRadiation"), names(x)))
    d <- as.data.frame(x)[, intersect(keep, names(x)), drop = FALSE]
    names(d)[names(d) == "hour"] <- "hour_lst"
    d$date <- format(d$date, "%Y-%m-%d")
  }
  c(hdr, .totals_csv_body(d))
}

totals_csv_name <- function(meta, what = "summary", tab = "climatology") {
  part <- switch(what, summary = "totals", per_year = "per-year", daily = "daily", hourly = "hourly", what)
  paste0("rxwx_", gsub("[^A-Za-z0-9]+", "-", tolower(meta$name)), "_", tab, "_", part, "_", format(Sys.Date(), "%Y%m%d"), ".csv")
}

# ---- Same dates: one window, every year ------------------------------------------------
# flagged: rx_flag() output of hourly_window() rows (has wyear, wday, date).
# ndays: named integer, days in the window per wyear (hourly_window_ndays).
# min_run: hours of consecutive in-prescription time that make a "usable day".
# Returns list(days, per_year, summary):
#   days      one row per day: wyear, date, wday, n_true, n_valid, n_missing,
#             longest, start_hour, usable, held (the limit that alone held out the
#             most hours that day) and held_h
#   per_year  totals_summary() input: one row per wyear x measure (period "Window"),
#             measures in_rx, pass_/only_<var>, plus usable_days and longest_run
#   summary   totals_summary(per_year)
totals_window <- function(flagged, rx_vars, ndays, min_run = SAME_MIN_RUN_H, hour_col = "hour") {
  vals <- totals_measures(rx_vars); vals <- vals[vals %in% names(flagged)]
  dl <- clim_summarise(flagged, vals, by = c("wyear", "date"))
  runs <- clim_runs(flagged, "in_rx", by = c("wyear", "date"), hour_col = hour_col)
  days <- dl[dl$value == "in_rx", c("wyear", "date", "n_true", "n_valid", "n_missing")]
  days$wday <- format(days$date, "%d %b")
  k <- match(paste(days$wyear, days$date), paste(runs$wyear, runs$date))
  days$longest <- runs$longest[k]; days$start_hour <- runs$start_hour[k]
  days$usable <- days$longest >= min_run
  days$held <- NA_character_; days$held_h <- NA_real_
  only <- dl[startsWith(dl$value, "only_"), ]
  if (nrow(only)) {
    only <- only[order(paste(only$wyear, only$date), -only$n_true), ]
    top <- only[!duplicated(paste(only$wyear, only$date)) & only$n_true > 0, ]
    j <- match(paste(days$wyear, days$date), paste(top$wyear, top$date))
    days$held <- sub("^only_", "", top$value[j]); days$held_h <- top$n_true[j]
  }
  # per year: sums over the window's days
  key <- paste(dl$wyear, dl$value)
  py <- data.frame(year = as.integer(tapply(dl$wyear, key, `[`, 1)), period = "Window",
                   value = tapply(dl$value, key, `[`, 1),
                   n_true = as.numeric(tapply(dl$n_true, key, sum)), n_valid = as.numeric(tapply(dl$n_valid, key, sum)),
                   n_missing = as.numeric(tapply(dl$n_missing, key, sum)), stringsAsFactors = FALSE)
  ud <- tapply(days$usable, days$wyear, sum); lr <- tapply(days$longest, days$wyear, max)
  yrs <- as.integer(names(ud))
  extra <- rbind(data.frame(year = yrs, period = "Window", value = "usable_days", n_true = as.numeric(ud),
                            n_valid = NA_real_, n_missing = NA_real_, stringsAsFactors = FALSE),
                 data.frame(year = yrs, period = "Window", value = "longest_run", n_true = as.numeric(lr),
                            n_valid = NA_real_, n_missing = NA_real_, stringsAsFactors = FALSE))
  py <- rbind(py, extra)
  # coverage: usable in_rx hours over the hours the window has that year (missing rows count as missing)
  ir <- py[py$value == "in_rx", ]
  expected <- 24 * ndays[as.character(ir$year)]
  cov <- stats::setNames(100 * ir$n_valid / expected, ir$year)
  py$hours <- ifelse(py$value == "in_rx", 24 * ndays[as.character(py$year)], NA_real_)
  py$pct_usable <- unname(cov[as.character(py$year)])
  py$used <- !is.na(py$pct_usable) & py$pct_usable >= TOTALS_COVERAGE_MIN
  py$period <- factor(py$period, levels = "Window")
  ord <- c(vals, "usable_days", "longest_run")
  py <- py[order(match(py$value, ord), py$year), ]
  rownames(py) <- NULL
  list(days = days, per_year = py, summary = totals_summary(py))
}

totals_window_display <- function(w, rx_vars, min_run = SAME_MIN_RUN_H) {
  py <- w$per_year; sm <- w$summary
  get <- function(v) { d <- py[py$value == v, ]; d[order(d$year), ] }
  ir <- get("in_rx"); ud <- get("usable_days"); lr <- get("longest_run")
  yr <- paste0(ir$year, ifelse(ir$used, "", "*"))
  main <- data.frame(yr, .totals_h(ir$n_true), .totals_h(ud$n_true[match(ir$year, ud$year)]),
                     paste0(.totals_h(lr$n_true[match(ir$year, lr$year)]), " h"),
                     paste0(round(ir$pct_usable), "%"), .totals_h(ir$n_missing), stringsAsFactors = FALSE)
  nm <- c("Year", "In prescription (h)", sprintf("Usable days (%d+ h run)", min_run), "Longest run",
          "Data coverage", "Hours excluded")
  names(main) <- nm
  # summary rows: typical year, lowest, highest (years named), for hours, usable days and longest run
  srow <- function(lab, f) {
    g <- function(v, unit = "") { r <- sm[sm$value == v, ]; if (!nrow(r)) "" else f(r, unit) }
    stats::setNames(data.frame(lab, g("in_rx"), g("usable_days"), g("longest_run", " h"), "", "",
                               stringsAsFactors = FALSE), nm)
  }
  main <- rbind(main,
                srow("Typical year", function(r, u) paste0(.totals_h(r$median), u)),
                srow("Lowest", function(r, u) sprintf("%s%s (%d)", .totals_h(r$low), u, r$low_year)),
                srow("Highest", function(r, u) sprintf("%s%s (%d)", .totals_h(r$high), u, r$high_year)))
  rows <- list()
  for (v in rx_vars) {
    lab <- RX_VARS$label[RX_VARS$var == v]
    for (m in c("pass", "only")) {
      r <- sm[sm$value == paste0(m, "_", v), ]
      if (!nrow(r)) next
      rows[[length(rows) + 1]] <- c(Limit = if (m == "pass") lab else "", Hours = if (m == "pass") "met" else "only one failing",
                                    `Typical year` = .totals_h(r$median),
                                    Lowest = if (is.na(r$low)) "" else sprintf("%s (%d)", .totals_h(r$low), r$low_year),
                                    Highest = if (is.na(r$high)) "" else sprintf("%s (%d)", .totals_h(r$high), r$high_year),
                                    `All years` = .totals_h(r$total))
    }
  }
  by_limit <- if (length(rows)) as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE, check.names = FALSE)
              else data.frame(Limit = character(0), Hours = character(0))
  list(main = main, by_limit = by_limit, any_left_out = any(!ir$used))
}

# what = "per_year": totals_window()$per_year; what = "daily": $days.
totals_window_csv_lines <- function(w, meta, source_detail, rx, window_label, min_run, hour_label,
                                    what = c("per_year", "daily")) {
  what <- match.arg(what)
  hdr <- .totals_csv_header(paste0("same dates, many years, ", if (what == "per_year") "hour totals by year" else "every day"),
                            meta, source_detail, rx, paste0("Window: ", window_label, ", every year in the record"), hour_label,
                            c(sprintf("A usable day has at least %d consecutive hours in prescription (the run stops at midnight).", min_run),
                              paste0("Typical year, lowest and highest leave out a year with under ", TOTALS_COVERAGE_MIN,
                                     "% of the window's hours usable (in_median = FALSE).")))
  if (what == "per_year") {
    x <- w$per_year
    d <- data.frame(year = x$year, measure = x$value,
                    description = ifelse(x$value == "usable_days", sprintf("Days with a run of %d+ h in prescription", min_run),
                                         ifelse(x$value == "longest_run", "Longest run in prescription (h)", totals_measure_label(x$value))),
                    value = x$n_true, hours_usable = x$n_valid, hours_excluded = x$n_missing,
                    pct_usable = round(x$pct_usable, 1), in_median = x$used, stringsAsFactors = FALSE)
  } else {
    x <- w$days
    d <- data.frame(year = x$wyear, date = format(x$date, "%Y-%m-%d"), hours_in_rx = x$n_true, hours_usable = x$n_valid,
                    hours_excluded = x$n_missing, longest_run_h = x$longest, run_start_hour = x$start_hour,
                    usable_day = x$usable, held_out_most_by = RX_VARS$label[match(x$held, RX_VARS$var)],
                    held_out_hours = x$held_h, stringsAsFactors = FALSE)
  }
  c(hdr, .totals_csv_body(d))
}
