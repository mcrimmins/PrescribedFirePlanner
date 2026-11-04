# ==============================================================================
# R/tooltips.R -- every hover text in the app, with one set of formatters
#
# Rule (CLAUDE.md): tooltips come from here only. Each one says what the mark
# is, the number behind it, and how many hours it's based on, with the same
# wording, units and time label ("LST", or "clock time" with the toggle on).
#
# Inputs are summaries (clim_summarise() output, metadata rows); nothing here
# aggregates data. Text uses <br> line breaks for plotly and leaflet.
# Requires R/constants.R.
# ==============================================================================

# ---- formatters ----------------------------------------------------------------------
tip_pct <- function(x) {
  ifelse(is.na(x), "n/a",
         ifelse(x > 0 & x < 0.005, "<1%", ifelse(x < 1 & x > 0.995, ">99%", paste0(round(100 * x), "%"))))
}

tip_hours <- function(n) {
  ifelse(is.na(n), "n/a", paste0(format(n, big.mark = ",", trim = TRUE), ifelse(n == 1, " hour", " hours")))
}

tip_hour_label <- function(h, clock = FALSE) {
  sprintf("%02d:00 %s", as.integer(h), if (clock) "clock time" else "LST")
}

# A value with its units, rounded for reading: "78 \u00b0F", "22%", "6 mph".
tip_value <- function(var, x) {
  if (is.na(x)) return("n/a")
  switch(var,
         temperature      = paste0(round(x), " \u00b0F"),
         relativeHumidity = paste0(round(x), "%"),
         windSpeed        = paste0(round(x), " mph"),
         gustSpeed        = paste0(round(x), " mph"),
         windDirection    = paste0(round(x), "\u00b0"),
         precipitation    = paste0(formatC(x, format = "f", digits = 2), " in/hr"),
         solarRadiation   = paste0(round(x), " W/m\u00b2"),
         format(signif(x, 3)))
}

.tip_short <- c(temperature = "temperature", relativeHumidity = "RH", windSpeed = "wind speed", gustSpeed = "gusts",
                windDirection = "wind direction", precipitation = "precipitation", solarRadiation = "solar radiation")

# ---- heatmap cells -----------------------------------------------------------------------
# `long` is clim_summarise() output for values in_rx, pass_<var> for each
# constrained variable, and (optionally) the variables themselves (means).
# Returns one row per cell: the by-columns plus `text`.
#   x_col     "month" (v0.1); "doy" or "week" later
#   hour_col  "hour" (LST) or "hour_clock"
#   obs_minute  station's observation minute, shown with LST labels
#   threshold   outline threshold in % (NA = none); cells at or above it say so
#   sun         optional data frame (doy, sunrise, sunset) in the chart's hours, for x_col "doy"
tip_cells <- function(long, x_col = "month", hour_col = "hour", rx_vars = character(0),
                      obs_minute = NA_integer_, threshold = NA_real_, sun = NULL) {
  clock <- hour_col == "hour_clock"
  key <- paste(long[[x_col]], long[[hour_col]])
  ir <- long[long$value == "in_rx", ]
  ir$key <- paste(ir[[x_col]], ir[[hour_col]])
  get_stat <- function(val) {
    s <- long[long$value == val, ]
    s$stat[match(ir$key, paste(s[[x_col]], s[[hour_col]]))]
  }
  when_x <- if (x_col == "month") month.name[ir[[x_col]]]
            else if (x_col == "doy") paste0(format(as.Date("2001-01-01") + ir[[x_col]] - 1, "%d %b"),
                                            " (\u00b1", CLIM_SMOOTH_DAYS, " days)")
            else paste(x_col, ir[[x_col]])
  hr <- tip_hour_label(ir[[hour_col]], clock)
  if (!clock && !is.na(obs_minute) && obs_minute > 0)
    hr <- paste0(hr, sprintf(" (read at :%02d)", as.integer(obs_minute)))

  lines <- paste0("<b>", when_x, ", ", hr, "</b><br>",
                  "In prescription: <b>", tip_pct(ir$stat), "</b> of ", tip_hours(ir$n_valid),
                  " (", ir$n_years, .tip_years(ir$n_years), ")")
  if (!is.na(threshold))
    lines <- paste0(lines, ifelse(!is.na(ir$stat) & 100 * ir$stat >= threshold,
                                  sprintf("<br>Outlined: at or above your %d%% threshold", round(threshold)), ""))
  if (!is.null(sun) && x_col == "doy") {
    k <- match(ir$doy, sun$doy)
    lines <- paste0(lines, "<br>Sunrise ", sun_fmt_hm(sun$sunrise[k]), ", sunset ", sun_fmt_hm(sun$sunset[k]),
                    if (clock) " clock time" else " LST")
  }
  lines <- paste0(lines, ifelse(ir$n_missing > 0,
                                paste0("<br>Excluded: ", tip_hours(ir$n_missing), " (missing or screened)"), ""))

  if (length(rx_vars)) {
    fails <- vapply(rx_vars, function(v) 1 - get_stat(paste0("pass_", v)), numeric(nrow(ir)))
    if (is.null(dim(fails))) fails <- matrix(fails, nrow = nrow(ir))
    worst <- apply(fails, 1, function(r) if (all(is.na(r))) NA_integer_ else which.max(r))
    worst_txt <- ifelse(is.na(worst) | ir$stat >= 0.995, "",
                        paste0("<br>Fails most often on: ", .tip_short[rx_vars[pmax(worst, 1)]], " (",
                               tip_pct(fails[cbind(seq_len(nrow(ir)), pmax(worst, 1))]), " of hours)"))
    lines <- paste0(lines, worst_txt)
    # precipitation is left out: a mean hourly rate of ~0.00 in/hr tells a reader nothing
    typ_vars <- intersect(rx_vars, c("temperature", "relativeHumidity", "windSpeed", "gustSpeed", "solarRadiation"))
    typ_vars <- typ_vars[typ_vars %in% long$value]
    if (length(typ_vars)) {
      typ_lab <- c(temperature = "", relativeHumidity = "RH ", windSpeed = "wind ", gustSpeed = "gusts ", solarRadiation = "solar ")
      stats_by <- lapply(typ_vars, get_stat)
      typ <- vapply(seq_len(nrow(ir)), function(i)
        paste(vapply(seq_along(typ_vars), function(k) paste0(typ_lab[[typ_vars[k]]], tip_value(typ_vars[k], stats_by[[k]][i])),
                     character(1)), collapse = ", "),
        character(1))
      lines <- paste0(lines, "<br>Typical: ", typ)
    }
  }
  out <- ir[, c(x_col, hour_col)]
  out$text <- lines
  rownames(out) <- NULL
  out
}

.tip_years <- function(n) ifelse(n == 1, " year", " years")

# ---- map markers ------------------------------------------------------------------------------
# `st` rows need station_id, station_name, elevation; flags are optional.
tip_station <- function(station_id, name, elevation, state = NA, featured = FALSE, bundled = FALSE, note = NA) {
  paste0("<b>", name, "</b> (", station_id, ")",
         ifelse(is.na(state), "", paste0(", ", state)),
         "<br>Elevation: ", format(round(elevation), big.mark = ","), " ft",
         ifelse(bundled, "<br>Bundled with the app: loads instantly",
                "<br>Downloads from FEMS (about 30 s)"),
         ifelse(featured & !is.na(note), paste0("<br><i>", note, "</i>"), ""))
}

# ---- Past event: one tooltip per hour ----------------------------------------------
# 16-point compass name for a direction in degrees: 225 -> "SW".
tip_compass <- function(deg) {
  pts <- c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW")
  ifelse(is.na(deg), NA_character_, pts[(round((deg %% 360) / 22.5) %% 16) + 1])
}

# A limit in words, for "fails RH 12% (limit 20-55%)".
.tip_limit <- function(var, lo, hi) {
  f <- function(x) sub("^n/a$", "", tip_value(var, x))
  if (var == "windDirection") return(sprintf("%s to %s", tip_compass(lo), tip_compass(hi)))
  if (var == "precipitation" && is.na(lo) && isTRUE(hi == 0)) return("no rain")
  if (is.na(lo)) paste("up to", f(hi)) else if (is.na(hi)) paste("at least", f(lo)) else paste(f(lo), "to", f(hi))
}

# flagged: rx_flag() output for the event's hours (date, hour, hour_clock,
# time_clock, the variables, pass_<var>, in_rx). Returns one string per row.
tip_event_hours <- function(flagged, rx, clock = FALSE, obs_minute = NA_integer_) {
  vars <- rx_constrained(rx)
  n <- nrow(flagged)
  when <- if (clock) format(flagged$time_clock, "%a %d %b %Y, %H:00 clock time")
          else paste0(format(flagged$date, "%a %d %b %Y, "), sprintf("%02d:00 LST", as.integer(flagged$hour)),
                      if (!is.na(obs_minute) && obs_minute > 0) sprintf(" (read at :%02d)", as.integer(obs_minute)) else "")
  status <- character(n)
  for (i in seq_len(n)) {
    ir <- flagged$in_rx[i]
    if (isTRUE(ir)) {
      status[i] <- if (!is.null(flagged$in_ign) && isFALSE(flagged$in_ign[i])) "<b>In prescription</b> (outside the ignition period)"
                   else "<b>In prescription</b>"
      next
    }
    fails <- character(0); unknown <- character(0)
    for (v in vars) {
      p <- flagged[[paste0("pass_", v)]][i]
      j <- which(rx$var == v)
      if (is.na(p)) unknown <- c(unknown, .tip_short[[v]])
      else if (!p) fails <- c(fails, sprintf("%s %s (limit %s)", .tip_short[[v]],
                                             if (v == "windDirection") tip_compass(flagged$windDirection[i]) else tip_value(v, flagged[[v]][i]),
                                             .tip_limit(v, rx$min[j], rx$max[j])))
    }
    status[i] <- paste0(if (length(fails)) paste0("<b>Out</b>: ", paste(fails, collapse = "; ")) else "<b>Not known</b>",
                        if (length(unknown)) paste0(if (length(fails)) "<br>" else ": ", "no data for ", paste(unknown, collapse = ", ")) else "")
  }
  wx <- paste0("Temp ", vapply(flagged$temperature, function(x) tip_value("temperature", x), ""),
               " \u00b7 RH ", vapply(flagged$relativeHumidity, function(x) tip_value("relativeHumidity", x), ""),
               "<br>Wind ", vapply(flagged$windSpeed, function(x) tip_value("windSpeed", x), ""),
               if (!is.null(flagged$gustSpeed)) ifelse(is.na(flagged$gustSpeed), "",
                                                       paste0(", gusts ", vapply(flagged$gustSpeed, function(x) tip_value("gustSpeed", x), ""))) else "",
               ifelse(!is.na(flagged$windSpeed) & flagged$windSpeed == 0, " (calm)",
                      ifelse(is.na(flagged$windDirection), "", paste0(" from ", tip_compass(flagged$windDirection),
                                                                      " (", round(flagged$windDirection), "\u00b0)"))),
               ifelse(is.na(flagged$precipitation) | flagged$precipitation == 0, "",
                      paste0(" \u00b7 Rain ", vapply(flagged$precipitation, function(x) tip_value("precipitation", x), ""))))
  paste0(when, "<br>", status, "<br>", wx)
}

# ---- Same dates: one tooltip per day ---------------------------------------------------
# days: totals_window()$days. Returns one string per row.
tip_window_days <- function(days, min_run, clock = FALSE) {
  run <- ifelse(days$longest == 0, "No hours in prescription",
                sprintf("Longest run %d h, %02d:00-%02d:00%s", days$longest, days$start_hour,
                        (days$start_hour + days$longest) %% 24, if (clock) " clock time" else " LST"))
  use <- ifelse(days$usable, sprintf(" \u2014 <b>a usable day</b> (%d+ h)", min_run), "")
  held <- ifelse(is.na(days$held), "",
                 paste0("<br>Held out most by ", .tip_short[days$held], " alone: ", tip_hours(days$held_h)))
  miss <- ifelse(days$n_missing > 0, paste0("<br>", tip_hours(days$n_missing), " not known (missing data)"), "")
  paste0("<b>", format(days$date, "%a %d %b %Y"), "</b>",
         "<br>In prescription: <b>", days$n_true, "</b> of ", tip_hours(days$n_valid), " with data",
         "<br>", run, use, held, miss)
}

# ---- Climatology by variable ----------------------------------------------------------
# A limit in words for captions and tooltips, e.g. "20% to 45%" or "at least 4 mph".
tip_limit_text <- function(var, lo, hi) .tip_limit(var, lo, hi)

# long: clim_summarise(by = c("doy", hour_col), smooth) with the variable (mean)
# and pass_<var>. One tooltip per cell of that variable's panel.
tip_var_cells <- function(long, var, hour_col = "hour", lo = NA, hi = NA, sun = NULL) {
  clock <- hour_col == "hour_clock"
  kind <- RX_VARS$kind[RX_VARS$var == var]
  pv <- long[long$value == paste0("pass_", var), ]
  base <- if (var %in% long$value) long[long$value == var, ] else pv
  key <- paste(base$doy, base[[hour_col]])
  ps <- pv$stat[match(key, paste(pv$doy, pv[[hour_col]]))]
  when <- paste0("<b>", format(as.Date("2001-01-01") + base$doy - 1, "%d %b"), " (\u00b1", CLIM_SMOOTH_DAYS, " days), ",
                 tip_hour_label(base[[hour_col]], clock), "</b>")
  val <- if (var %in% long$value)
    paste0("<br>Mean ", .tip_short[[var]], ": <b>", vapply(base$stat, function(x) tip_value(var, x), ""), "</b>") else ""
  lim <- if (var == "windDirection") paste0("in the sector (", .tip_limit(var, lo, hi), ")")
         else paste0("within your limit (", .tip_limit(var, lo, hi), ")")
  pass <- if (nrow(pv)) paste0("<br>Hours ", lim, ": <b>", tip_pct(ps), "</b> of ", tip_hours(base$n_valid)) else ""
  sunl <- if (!is.null(sun)) { k <- match(base$doy, sun$doy)
    paste0("<br>Sunrise ", sun_fmt_hm(sun$sunrise[k]), ", sunset ", sun_fmt_hm(sun$sunset[k])) } else ""
  out <- base[, c("doy", hour_col)]
  out$stat <- base$stat; out$pass <- ps
  out$text <- paste0(when, val, pass, sunl)
  rownames(out) <- NULL
  out
}

# q: clim_quantiles() rows for one variable; pass: share of those hours within the limit by doy.
tip_daily_range <- function(q, var, pass = NULL, hours_label = "all hours") {
  f <- function(x) vapply(x, function(v) tip_value(var, v), "")
  paste0("<b>", format(as.Date("2001-01-01") + q$doy - 1, "%d %b"), " (\u00b1", CLIM_SMOOTH_DAYS, " days, ", hours_label, ")</b>",
         "<br>Median ", .tip_short[[var]], ": <b>", f(q$q50), "</b>",
         "<br>Middle half of hours: ", f(q$q25), " to ", f(q$q75),
         "<br>8 in 10 hours: ", f(q$q10), " to ", f(q$q90),
         if (!is.null(pass)) paste0("<br>Within your limit: <b>", tip_pct(pass), "</b> of ", tip_hours(q$n))
         else paste0("<br>From ", tip_hours(q$n)))
}

# ---- Season tracker: one tooltip per day (shared by every panel) ------------------------
# days: totals_season_days(); bands: totals_season_reference()$bands.
tip_season_days <- function(days, bands, rx_vars, min_run, clock = FALSE) {
  b <- bands[bands$value == "in_rx", ]; k <- match(days$doy, b$doy)
  tot <- days$in_rx_obs + days$in_rx_fc
  part <- ifelse(days$has_fc & days$obs_valid > 0, sprintf(" (%d observed + %d forecast)", days$in_rx_obs, days$in_rx_fc),
                 ifelse(days$has_fc, " (FEMS forecast)", ""))
  run <- ifelse(is.na(days$longest) | days$longest == 0, "No hours in prescription",
                sprintf("Longest run %d h, %02d:00-%02d:00 %s", days$longest, days$start_hour,
                        (days$start_hour + days$longest) %% 24, if (clock) "clock time" else "LST"))
  use <- ifelse(days$usable, sprintf(" \u2014 <b>a usable day</b> (%d+ h)", min_run), "")
  ref <- ifelse(is.na(k), "", sprintf("<br>Typical for the date: %s h (middle half %s-%s h, record %s h)",
                                      round(b$q50[k]), round(b$q25[k]), round(b$q75[k]), round(b$q100[k])))
  held <- ifelse(is.na(days$held), "", paste0("<br>Held out most by ", .tip_short[days$held], " alone: ", tip_hours(days$held_h)))
  lim <- if (length(rx_vars)) paste0("<br>Limit met: ", vapply(seq_len(nrow(days)), function(i)
    paste(vapply(rx_vars, function(v) paste0(.tip_short[[v]], " ", days[[paste0("pass_", v, "_obs")]][i] + days[[paste0("pass_", v, "_fc")]][i], " h"), ""),
          collapse = ", "), "")) else ""
  miss <- ifelse(days$missing > 0, paste0("<br>", tip_hours(days$missing), " not known"), "")
  paste0("<b>", format(days$date, "%a %d %b %Y"), "</b>",
         "<br>In prescription: <b>", tot, " h</b>", part,
         "<br>", run, use, ref, held, lim, miss,
         "<br><i>Click to open this week on Past event</i>")
}
