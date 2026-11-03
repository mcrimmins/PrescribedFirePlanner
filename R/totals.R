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

# x: totals_summary() (what = "summary") or totals_per_year() (what = "per_year").
totals_csv_lines <- function(x, meta, source_detail, rx, years, hour_label, what = c("summary", "per_year")) {
  what <- match.arg(what)
  hdr <- c(
    paste0("# Prescribed Fire Weather Explorer (RxWx) ", APP_VERSION, " -- hour totals, ",
           if (what == "summary") "by month (typical year, range, total)" else "by year and month"),
    paste0("# Station: ", meta$name, " (", meta$station_id, ")",
           if (!is.null(meta$state) && !is.na(meta$state)) paste0(", ", meta$state) else ""),
    paste0("# Data: ", source_detail, "; hourly RAWS weather from USDA Forest Service FEMS"),
    paste0("# Years: ", years[1], "-", years[2]),
    paste0("# Prescription: ", totals_rx_text(rx)),
    paste0("# Hours: ", hour_label, ". Counts are hours. An hour is excluded when a limited variable is missing or was screened out."),
    paste0("# Median year and range leave out a year-month with under ", TOTALS_COVERAGE_MIN,
           "% of its hours usable (listed in years_left_out); those still count toward total_h."),
    paste0("# Exported: ", format(Sys.time(), "%Y-%m-%d %H:%M %Z"), ". Training tool, not an operational planning product."))
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
  body <- utils::capture.output(utils::write.csv(d, row.names = FALSE, na = ""))
  c(hdr, body)
}

totals_csv_name <- function(meta, what = "summary")
  paste0("rxwx_", gsub("[^A-Za-z0-9]+", "-", tolower(meta$name)), "_climatology_",
         if (what == "summary") "totals" else "per-year", "_", format(Sys.Date(), "%Y%m%d"), ".csv")
