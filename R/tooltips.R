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

.tip_short <- c(temperature = "temperature", relativeHumidity = "RH", windSpeed = "wind speed",
                windDirection = "wind direction", precipitation = "precipitation", solarRadiation = "solar radiation")

# ---- heatmap cells -----------------------------------------------------------------------
# `long` is clim_summarise() output for values in_rx, pass_<var> for each
# constrained variable, and (optionally) the variables themselves (means).
# Returns one row per cell: the by-columns plus `text`.
#   x_col     "month" (v0.1); "doy" or "week" later
#   hour_col  "hour" (LST) or "hour_clock"
#   obs_minute  station's observation minute, shown with LST labels
#   threshold   outline threshold in % (NA = none); cells at or above it say so
tip_cells <- function(long, x_col = "month", hour_col = "hour", rx_vars = character(0),
                      obs_minute = NA_integer_, threshold = NA_real_) {
  clock <- hour_col == "hour_clock"
  key <- paste(long[[x_col]], long[[hour_col]])
  ir <- long[long$value == "in_rx", ]
  ir$key <- paste(ir[[x_col]], ir[[hour_col]])
  get_stat <- function(val) {
    s <- long[long$value == val, ]
    s$stat[match(ir$key, paste(s[[x_col]], s[[hour_col]]))]
  }
  when_x <- if (x_col == "month") month.name[ir[[x_col]]] else paste(x_col, ir[[x_col]])
  hr <- tip_hour_label(ir[[hour_col]], clock)
  if (!clock && !is.na(obs_minute) && obs_minute > 0)
    hr <- paste0(hr, sprintf(" (read at :%02d)", as.integer(obs_minute)))

  lines <- paste0("<b>", when_x, ", ", hr, "</b><br>",
                  "In prescription: <b>", tip_pct(ir$stat), "</b> of ", tip_hours(ir$n_valid),
                  " (", ir$n_years, .tip_years(ir$n_years), ")")
  if (!is.na(threshold))
    lines <- paste0(lines, ifelse(!is.na(ir$stat) & 100 * ir$stat >= threshold,
                                  sprintf("<br>Outlined: at or above your %d%% threshold", round(threshold)), ""))
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
    typ_vars <- intersect(rx_vars, c("temperature", "relativeHumidity", "windSpeed", "solarRadiation"))
    typ_vars <- typ_vars[typ_vars %in% long$value]
    if (length(typ_vars)) {
      typ_lab <- c(temperature = "", relativeHumidity = "RH ", windSpeed = "wind ", solarRadiation = "solar ")
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
