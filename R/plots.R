# ==============================================================================
# R/plots.R -- charts only draw (no calculation here)
#
# Each function takes summaries (clim_summarise / quality_* output) plus
# tooltip text from R/tooltips.R, and returns a plotly widget.
# Colour: one sequential ramp (CHART_SEQ, chosen by CHART_RAMP), always 0-100%,
# so a colour means the same share for every prescription and station. 2 px gaps
# between cells; tooltips on every cell; recessive axes.
# Requires R/constants.R; uses plotly (namespaced, never attached).
# ==============================================================================

.plot_colorscale <- function(colors = CHART_SEQ) {
  n <- length(colors)
  lapply(seq_len(n), function(i) list((i - 1) / (n - 1), colors[i]))
}

.plot_axis <- function(...) {
  utils::modifyList(list(showgrid = FALSE, zeroline = FALSE, ticks = "", fixedrange = TRUE,
                         tickfont = list(size = 12, color = "#555555"), titlefont = list(size = 12, color = "#555555")),
                    list(...))
}

# Month x hour heatmap of a share (0-1). `cells` has the month and hour column,
# `stat` (share), and `text` (from tip_cells). Hours run top to bottom like a
# timetable: midnight at the top, late evening at the bottom.
# `threshold` (0-100, or NA for none): cells at or above it get an outline in
# `highlight`, so the window shows by outline as well as by colour.
plot_hour_heatmap <- function(cells, hour_col = "hour", x_col = "month", clock = FALSE,
                              colorbar_title = "% of hours<br>in prescription",
                              threshold = NA, colors = CHART_SEQ, highlight = CHART_HIGHLIGHT, outside = NULL) {
  xs <- sort(unique(cells[[x_col]])); hs <- 0:23
  z <- matrix(NA_real_, nrow = 24, ncol = length(xs)); tx <- matrix("", nrow = 24, ncol = length(xs))
  i <- match(cells[[hour_col]], hs); j <- match(cells[[x_col]], xs)
  z[cbind(i, j)] <- 100 * cells$stat
  tx[cbind(i, j)] <- cells$text
  xlab <- if (x_col == "month") month.abb[xs] else as.character(xs)
  shapes <- list()
  if (!is.na(threshold)) {
    hit <- which(!is.na(z) & z >= threshold, arr.ind = TRUE)
    shapes <- lapply(seq_len(nrow(hit)), function(k) {
      r <- hit[k, 1] - 1; cc <- hit[k, 2] - 1          # 0-based category positions
      list(type = "rect", xref = "x", yref = "y", x0 = cc - 0.5, x1 = cc + 0.5, y0 = r - 0.5, y1 = r + 0.5,
           line = list(color = highlight, width = 2.5), fillcolor = "rgba(0,0,0,0)", layer = "above")
    })
  }
  # `outside`: data frame of x and hour (same columns as cells) for cells outside the
  # ignition period; they are washed out so the ignition hours stand out
  if (!is.null(outside) && nrow(outside)) {
    oi <- match(outside[[hour_col]], hs) - 1; oj <- match(outside[[x_col]], xs) - 1
    shapes <- c(lapply(which(!is.na(oi) & !is.na(oj)), function(k)
      list(type = "rect", xref = "x", yref = "y", x0 = oj[k] - 0.5, x1 = oj[k] + 0.5, y0 = oi[k] - 0.5, y1 = oi[k] + 0.5,
           line = list(width = 0), fillcolor = CHART_OUTSIDE_IGN, layer = "above")), shapes)
  }
  plotly::plot_ly(x = xlab, y = sprintf("%02d:00", hs), z = z, text = tx, type = "heatmap",
                  colorscale = .plot_colorscale(colors), zmin = 0, zmax = 100,
                  xgap = CHART_CELL_GAP, ygap = CHART_CELL_GAP,
                  hovertemplate = "%{text}<extra></extra>",
                  colorbar = list(title = list(text = colorbar_title, font = list(size = 12)),
                                  ticksuffix = "%", tickvals = c(0, 25, 50, 75, 100), len = 0.9,
                                  outlinewidth = 0, thickness = 14)) |>
    plotly::layout(xaxis = .plot_axis(title = "", type = "category", side = "top"),
                   yaxis = .plot_axis(title = if (clock) "Hour (clock time)" else "Hour (LST)",
                                      type = "category", autorange = "reversed", dtick = 3),
                   shapes = shapes,
                   margin = list(l = 70, r = 20, t = 40, b = 20),
                   plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)",
                   hoverlabel = list(bgcolor = "white", bordercolor = "#cccccc", font = list(size = 13, color = "#222222"),
                                     align = "left")) |>
    plotly::config(displaylogo = FALSE,
                   modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d", "autoScale2d",
                                              "zoomIn2d", "zoomOut2d", "resetScale2d"),
                   toImageButtonOptions = list(format = "png", filename = "rx_month_hour", scale = 2))
}

# Year x variable grid of % of hours reporting (quality_coverage output).
plot_coverage_grid <- function(cov, labels) {
  yrs <- sort(unique(cov$year)); vars <- unique(cov$variable)
  z <- matrix(NA_real_, length(vars), length(yrs)); tx <- matrix("", length(vars), length(yrs))
  i <- match(cov$variable, vars); j <- match(cov$year, yrs)
  z[cbind(i, j)] <- cov$pct
  tx[cbind(i, j)] <- paste0("<b>", labels[cov$variable], ", ", cov$year, "</b><br>",
                            "Reporting: <b>", round(cov$pct), "%</b> of ", tip_hours(cov$hours))
  plotly::plot_ly(x = as.character(yrs), y = unname(labels[vars]), z = z, text = tx, type = "heatmap",
                  colorscale = .plot_colorscale(), zmin = 0, zmax = 100,
                  xgap = CHART_CELL_GAP, ygap = CHART_CELL_GAP, hovertemplate = "%{text}<extra></extra>",
                  colorbar = list(title = list(text = "% of hours<br>reporting", font = list(size = 12)),
                                  ticksuffix = "%", tickvals = c(0, 50, 100), outlinewidth = 0, thickness = 12)) |>
    plotly::layout(xaxis = .plot_axis(title = "", type = "category", dtick = 2),
                   yaxis = .plot_axis(title = "", type = "category", autorange = "reversed"),
                   margin = list(l = 130, r = 20, t = 10, b = 30),
                   plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)",
                   hoverlabel = list(bgcolor = "white", bordercolor = "#cccccc", font = list(size = 13, color = "#222222"),
                                     align = "left")) |>
    plotly::config(displaylogo = FALSE, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d",
                                                                    "autoScale2d", "zoomIn2d", "zoomOut2d", "resetScale2d"))
}

# ---- Past event: stacked hourly traces ----------------------------------------------
# flagged: rx_flag() output for the event, in time order. text: one tooltip per
# row (tip_event_hours). nights: sun_nights() spans (hours from the first
# date's midnight, LST). clock_shift: hours to add for clock time (0 for LST).
# Top strip: each hour coloured in / out / not known. Below: one panel per
# variable; the prescription range shaded, markers blue when the hour is in
# prescription, orange when this limit failed, grey otherwise.
plot_event <- function(flagged, rx, text, nights, vars_show, clock = FALSE, clock_shift = 0) {
  n <- nrow(flagged)
  base <- as.POSIXct(format(min(flagged$date)), tz = "UTC")
  fmt <- function(t) format(t, "%Y-%m-%d %H:%M:%S", tz = "UTC")
  x <- if (clock) format(flagged$time_clock, "%Y-%m-%d %H:%M:%S")
       else fmt(as.POSIXct(format(flagged$date), tz = "UTC") + flagged$hour * 3600)
  vars <- rx_constrained(rx)
  k <- length(vars_show)
  # vertical layout: strip (0.45 units) then one unit per variable, top to bottom
  units <- c(0.45, rep(1, k)); gap <- 0.06
  tot <- sum(units) + gap * k
  top <- 1 - c(0, cumsum(units + gap))[seq_along(units)] / tot
  bottom <- top - units / tot
  yname <- function(i) if (i == 1) "y" else paste0("y", i)
  ylay  <- function(i) if (i == 1) "yaxis" else paste0("yaxis", i)

  st <- ifelse(is.na(flagged$in_rx), "unknown", ifelse(flagged$in_rx, "in", "out"))
  if (!is.null(flagged$in_ign)) st[st == "in" & !flagged$in_ign] <- "in_out_ign"   # in prescription, outside ignition hours
  p <- plotly::plot_ly()
  p <- plotly::add_bars(p, x = x, y = rep(1, n), yaxis = "y", text = text, hoverinfo = "text", textposition = "none",
                        marker = list(color = c(`in` = CHART_IN_RX, in_out_ign = CHART_IN_RX_OUT_IGN, out = CHART_OUT, unknown = "#ffffff")[st],
                                      line = list(color = ifelse(st == "unknown", "#c3ccd3", "rgba(0,0,0,0)"), width = 1)),
                        showlegend = FALSE)
  shapes <- list()
  lay <- list()
  lay[["yaxis"]] <- .plot_axis(domain = c(bottom[1], top[1]), range = c(0, 1), showticklabels = FALSE,
                               title = list(text = "In Rx", font = list(size = 11, color = "#555555")))
  for (i in seq_len(k)) {
    v <- vars_show[i]; ax <- i + 1
    m <- RX_VARS[RX_VARS$var == v, ]
    y <- flagged[[v]]
    pass <- if (v %in% vars) flagged[[paste0("pass_", v)]] else rep(NA, n)
    col <- ifelse(!is.na(flagged$in_rx) & flagged$in_rx, CHART_IN_RX,
                  ifelse(!is.na(pass) & !pass, CHART_FAIL, CHART_OUT))
    circ <- m$kind == "circular"
    if (!circ) p <- plotly::add_lines(p, x = x, y = y, yaxis = yname(ax), line = list(color = "#9aa5ad", width = 1.2),
                                      hoverinfo = "skip", showlegend = FALSE, connectgaps = FALSE)
    p <- plotly::add_markers(p, x = x, y = y, yaxis = yname(ax), text = text, hoverinfo = "text",
                             marker = list(color = col, size = if (circ) 6 else 5), showlegend = FALSE)
    rng <- if (circ) c(0, 360) else {
      r <- range(c(y, if (v %in% vars) c(rx$min[rx$var == v], rx$max[rx$var == v])), na.rm = TRUE)
      if (!all(is.finite(r))) r <- c(m$slider_min, m$slider_max)
      pad <- max(diff(r) * 0.08, m$step); c(max(m$slider_min, r[1] - pad), r[2] + pad)
    }
    if (v %in% vars) {
      lo <- rx$min[rx$var == v]; hi <- rx$max[rx$var == v]
      band <- function(a, b) list(type = "rect", xref = "paper", x0 = 0, x1 = 1, yref = yname(ax), y0 = a, y1 = b,
                                  fillcolor = CHART_BAND, line = list(width = 0), layer = "below")
      edge <- function(a) list(type = "line", xref = "paper", x0 = 0, x1 = 1, yref = yname(ax), y0 = a, y1 = a,
                               line = list(color = CHART_IN_RX, width = 1, dash = "dot"), layer = "below")
      if (circ) {
        shapes <- c(shapes, if (lo <= hi) list(band(lo, hi)) else list(band(lo, 360), band(0, hi)), list(edge(lo), edge(hi)))
      } else {
        a <- if (is.na(lo)) rng[1] else lo; b <- if (is.na(hi)) rng[2] else hi
        shapes <- c(shapes, list(band(a, b)), if (!is.na(lo)) list(edge(lo)), if (!is.na(hi)) list(edge(hi)))
      }
    }
    lab <- if (v == "windSpeed") "Wind (mph)" else if (v == "relativeHumidity") "RH (%)" else
           if (v == "windDirection") "Wind dir" else paste0(sub(" .*", "", m$label), " (", m$units, ")")
    lay[[ylay(ax)]] <- .plot_axis(domain = c(bottom[ax], top[ax]), range = rng,
                                  title = list(text = lab, font = list(size = 11, color = "#555555")),
                                  showgrid = TRUE, gridcolor = "#eeeeee",
                                  tickvals = if (circ) c(0, 90, 180, 270, 360) else NULL,
                                  ticktext = if (circ) c("N", "E", "S", "W", "N") else NULL)
  }
  night <- lapply(seq_len(nrow(nights)), function(r)
    list(type = "rect", xref = "x", yref = "paper", y0 = 0, y1 = 1,
         x0 = fmt(base + (nights$from[r] + clock_shift) * 3600), x1 = fmt(base + (nights$to[r] + clock_shift) * 3600),
         fillcolor = CHART_NIGHT, line = list(width = 0), layer = "below"))
  xr <- c(fmt(base + (clock_shift - 0.5) * 3600),
          fmt(base + (as.numeric(max(flagged$date) - min(flagged$date)) * 24 + 23.5 + clock_shift) * 3600))
  lay$xaxis <- .plot_axis(type = "date", range = xr, fixedrange = FALSE, showgrid = TRUE, gridcolor = "#e6e6e6",
                          dtick = 86400000, tickformat = "%a %d %b", ticklabelmode = "period", anchor = yname(k + 1),
                          title = list(text = if (clock) "Clock time" else "Local standard time (LST); shaded: night",
                                       font = list(size = 11, color = "#555555")))
  lay$shapes <- c(night, shapes)
  lay$bargap <- 0.08
  lay$hovermode <- "closest"
  lay$margin <- list(l = 70, r = 20, t = 10, b = 50)
  lay$plot_bgcolor <- "rgba(0,0,0,0)"; lay$paper_bgcolor <- "rgba(0,0,0,0)"
  lay$hoverlabel <- list(bgcolor = "white", bordercolor = "#cccccc", font = list(size = 13, color = "#222222"), align = "left")
  p <- do.call(plotly::layout, c(list(p), lay))
  plotly::config(p, displaylogo = FALSE,
                 modeBarButtonsToRemove = c("select2d", "lasso2d", "autoScale2d", "zoomIn2d", "zoomOut2d"),
                 toImageButtonOptions = list(format = "png", filename = "rx_past_event", scale = 2))
}

# ---- Same dates: year x day heatmap ---------------------------------------------------
# days: totals_window()$days with `text` (tip_window_days). labels: every day
# of the window in order (attr "window_days"). Colour = hours in prescription
# that day (0 to zmax: 24, or the ignition period's hours; same ramp as the climatology); usable days outlined.
plot_year_day <- function(days, labels, zmax = 24, colors = CHART_SEQ, highlight = CHART_HIGHLIGHT) {
  yrs <- sort(unique(days$wyear))
  z <- matrix(NA_real_, length(yrs), length(labels)); tx <- matrix("", length(yrs), length(labels))
  i <- match(days$wyear, yrs); j <- match(days$wday, labels)
  ok <- !is.na(j)
  z[cbind(i, j)[ok, , drop = FALSE]] <- ifelse(days$n_valid[ok] > 0, days$n_true[ok], NA)
  tx[cbind(i, j)[ok, , drop = FALSE]] <- days$text[ok]
  u <- which(ok & days$usable)
  shapes <- lapply(u, function(k)
    list(type = "rect", xref = "x", yref = "y", x0 = j[k] - 1.5, x1 = j[k] - 0.5, y0 = i[k] - 1.5, y1 = i[k] - 0.5,
         line = list(color = highlight, width = 1.1), fillcolor = "rgba(0,0,0,0)", layer = "above"))
  step <- if (length(labels) > 45) 7 else if (length(labels) > 20) 3 else 1
  tv <- labels[seq(1, length(labels), by = step)]
  plotly::plot_ly(x = labels, y = as.character(yrs), z = z, text = tx, type = "heatmap",
                  colorscale = .plot_colorscale(colors), zmin = 0, zmax = zmax,
                  xgap = 1, ygap = 1, hovertemplate = "%{text}<extra></extra>",
                  colorbar = list(title = list(text = paste0("Hours in<br>prescription<br>(of ", zmax, ")"), font = list(size = 12)),
                                  tickvals = unique(round(seq(0, zmax, length.out = 5))), len = 0.9, outlinewidth = 0, thickness = 14)) |>
    plotly::layout(xaxis = .plot_axis(title = "", type = "category", tickvals = tv, side = "top",
                                      categoryorder = "array", categoryarray = labels),
                   yaxis = .plot_axis(title = "", type = "category", autorange = "reversed",
                                      categoryorder = "array", categoryarray = as.character(yrs)),
                   shapes = shapes, margin = list(l = 50, r = 20, t = 40, b = 10),
                   plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)",
                   hoverlabel = list(bgcolor = "white", bordercolor = "#cccccc", font = list(size = 13, color = "#222222"),
                                     align = "left")) |>
    plotly::config(displaylogo = FALSE,
                   modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d", "autoScale2d",
                                              "zoomIn2d", "zoomOut2d", "resetScale2d"),
                   toImageButtonOptions = list(format = "png", filename = "rx_same_dates", scale = 2))
}

# ---- Climatology: hour x day-of-year chart (WeatherSpark style) -----------------------
# cells: doy, hour column, stat (share, smoothed), text (tip_cells). Hours run
# down the chart (midnight at the top) on a numeric axis, so a cell for hour h
# spans h to h+1 and the sun lines sit at their real times.
# sun: doy, sunrise, sunset, civil_dawn, civil_dusk in the chart's hours.
# ign: NULL or doy, start, end in the chart's hours (the ignition period).
# threshold (%): a contour line where the share crosses it (NA = none).
plot_doy_heatmap <- function(cells, hour_col = "hour", clock = FALSE, sun = NULL, ign = NULL, threshold = NA,
                             colors = CHART_SEQ, highlight = CHART_HIGHLIGHT) {
  N <- CLIM_DOY_N
  x <- sort(unique(cells$doy)); y <- 0:23 + 0.5      # x: the days drawn (every CLIM_DOY_STEP-th)
  z <- matrix(NA_real_, 24, length(x)); tx <- matrix("", 24, length(x))
  j <- match(cells$doy, x)
  z[cbind(cells[[hour_col]] + 1, j)] <- 100 * cells$stat
  tx[cbind(cells[[hour_col]] + 1, j)] <- cells$text
  first <- as.integer(format(as.Date(sprintf("2001-%02d-01", 1:12)), "%j"))
  p <- plotly::plot_ly() |>
    plotly::add_heatmap(x = x, y = y, z = z, text = tx, hovertemplate = "%{text}<extra></extra>",
                        colorscale = .plot_colorscale(colors), zmin = 0, zmax = 100,
                        colorbar = list(title = list(text = "% of hours<br>in prescription", font = list(size = 12)),
                                        ticksuffix = "%", tickvals = c(0, 25, 50, 75, 100), len = 0.9,
                                        outlinewidth = 0, thickness = 14))
  if (!is.na(threshold))
    p <- plotly::add_contour(p, x = x, y = y, z = z, hoverinfo = "skip", showscale = FALSE,
                             contours = list(start = threshold, end = threshold, size = 1, coloring = "none"),
                             line = list(color = highlight, width = 2), showlegend = FALSE)
  line <- function(p, yv, col, dash, name, width = 1.4)
    plotly::add_lines(p, x = sun$doy, y = yv, line = list(color = col, width = width, dash = dash),
                      hoverinfo = "skip", name = name, showlegend = TRUE)
  if (!is.null(sun)) {
    p <- line(p, sun$sunrise, "#e0a100", "solid", "Sunrise / sunset")
    p <- plotly::add_lines(p, x = sun$doy, y = sun$sunset, line = list(color = "#e0a100", width = 1.4),
                           hoverinfo = "skip", showlegend = FALSE)
    p <- line(p, sun$civil_dawn, "#7a6a9a", "dot", "Civil twilight", 1.1)
    p <- plotly::add_lines(p, x = sun$doy, y = sun$civil_dusk, line = list(color = "#7a6a9a", width = 1.1, dash = "dot"),
                           hoverinfo = "skip", showlegend = FALSE)
  }
  if (!is.null(ign)) {
    p <- plotly::add_lines(p, x = ign$doy, y = ign$start, line = list(color = "#222222", width = 1.3, dash = "dash"),
                           hoverinfo = "skip", name = "Ignition period", showlegend = TRUE)
    p <- plotly::add_lines(p, x = ign$doy, y = ign$end, line = list(color = "#222222", width = 1.3, dash = "dash"),
                           hoverinfo = "skip", showlegend = FALSE)
  }
  if (!is.na(threshold))
    p <- plotly::add_lines(p, x = c(NA_real_, NA_real_), y = c(NA_real_, NA_real_), line = list(color = highlight, width = 2),
                           name = sprintf("%d%% of hours in prescription", round(threshold)), showlegend = TRUE)
  p |>
    plotly::layout(xaxis = .plot_axis(title = "", range = c(0.5, N + 0.5), tickvals = first, ticktext = month.abb,
                                      side = "top", ticklabelmode = "period"),
                   yaxis = .plot_axis(title = if (clock) "Hour (clock time)" else "Hour (LST)", range = c(24, 0),
                                      tickvals = seq(0, 21, 3), ticktext = sprintf("%02d:00", seq(0, 21, 3))),
                   legend = list(orientation = "h", x = 0, y = -0.04, yanchor = "top", font = list(size = 11)),
                   margin = list(l = 70, r = 20, t = 40, b = 40),
                   plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)",
                   hoverlabel = list(bgcolor = "white", bordercolor = "#cccccc", font = list(size = 13, color = "#222222"),
                                     align = "left")) |>
    plotly::config(displaylogo = FALSE,
                   modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d", "autoScale2d",
                                              "zoomIn2d", "zoomOut2d", "resetScale2d"),
                   toImageButtonOptions = list(format = "png", filename = "rx_day_of_year", scale = 2))
}

# ---- Climatology by variable: one hour x day-of-year panel ---------------------------------
# cells: tip_var_cells() output (doy, hour column, stat = mean value, pass = share passing, text).
# Colour = the share of hours that pass THIS limit, on the same ramp and scale as
# the main chart, so the panels show which limit closes the window, when. The
# mean value is in the tooltip. threshold: the same orange contour as the main chart.
plot_var_doy <- function(cells, var, hour_col = "hour", sun = NULL, clock = FALSE, threshold = NA,
                         highlight = CHART_HIGHLIGHT) {
  x <- sort(unique(cells$doy)); y <- 0:23 + 0.5
  z <- matrix(NA_real_, 24, length(x)); tx <- matrix("", 24, length(x))
  ij <- cbind(cells[[hour_col]] + 1, match(cells$doy, x))
  z[ij] <- 100 * cells$pass
  tx[ij] <- cells$text
  p <- plotly::plot_ly() |>
    plotly::add_heatmap(x = x, y = y, z = z, text = tx, hovertemplate = "%{text}<extra></extra>",
                        colorscale = .plot_colorscale(CHART_SEQ), zmin = 0, zmax = 100,
                        colorbar = list(title = list(text = "% of hours<br>within this limit", font = list(size = 11)),
                                        len = 0.95, thickness = 12, outlinewidth = 0, ticksuffix = "%",
                                        tickvals = c(0, 50, 100)))
  if (!is.na(threshold))
    p <- plotly::add_contour(p, x = x, y = y, z = z, hoverinfo = "skip", showscale = FALSE, showlegend = FALSE,
                             contours = list(start = threshold, end = threshold, size = 1, coloring = "none"),
                             line = list(color = highlight, width = 1.6))
  if (!is.null(sun)) for (col in c("sunrise", "sunset"))
    p <- plotly::add_lines(p, x = sun$doy, y = sun[[col]], line = list(color = "#e0a100", width = 1.1),
                           hoverinfo = "skip", showlegend = FALSE)
  first <- as.integer(format(as.Date(sprintf("2001-%02d-01", 1:12)), "%j"))
  p |>
    plotly::layout(xaxis = .plot_axis(title = "", range = c(0.5, CLIM_DOY_N + 0.5), tickvals = first, ticktext = month.abb),
                   yaxis = .plot_axis(title = if (clock) "Hour (clock)" else "Hour (LST)", range = c(24, 0),
                                      tickvals = seq(0, 18, 6), ticktext = sprintf("%02d:00", seq(0, 18, 6))),
                   margin = list(l = 60, r = 10, t = 10, b = 25),
                   plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)",
                   hoverlabel = list(bgcolor = "white", bordercolor = "#cccccc", font = list(size = 13, color = "#222222"),
                                     align = "left")) |>
    plotly::config(displaylogo = FALSE, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d", "autoScale2d",
                                                                    "zoomIn2d", "zoomOut2d", "resetScale2d"),
                   toImageButtonOptions = list(format = "png", filename = paste0("rx_", var, "_by_day"), scale = 2))
}

# ---- Climatology: daily range of a variable through the year -------------------------------
# q: clim_quantiles() rows for one variable; text: tip_daily_range(). The
# prescription range is shaded across the year; 10-90 and 25-75 bands, median line.
plot_daily_range <- function(q, var, lo = NA, hi = NA, text = NULL, band = CHART_IN_RX) {
  m <- RX_VARS[RX_VARS$var == var, ]
  rng <- range(c(q$q10, q$q90, lo, hi), na.rm = TRUE); pad <- diff(rng) * 0.06
  yr <- c(max(m$slider_min, rng[1] - pad), rng[2] + pad)
  shapes <- list()
  if (!is.na(lo) || !is.na(hi))
    shapes <- list(list(type = "rect", xref = "paper", x0 = 0, x1 = 1, yref = "y",
                        y0 = if (is.na(lo)) yr[1] else lo, y1 = if (is.na(hi)) yr[2] else hi,
                        fillcolor = CHART_BAND, line = list(width = 0), layer = "below"))
  for (b in c(lo, hi)) if (!is.na(b))
    shapes <- c(shapes, list(list(type = "line", xref = "paper", x0 = 0, x1 = 1, yref = "y", y0 = b, y1 = b,
                                  line = list(color = band, width = 1, dash = "dot"), layer = "below")))
  first <- as.integer(format(as.Date(sprintf("2001-%02d-01", 1:12)), "%j"))
  plotly::plot_ly(q, x = ~doy) |>
    plotly::add_ribbons(ymin = ~q10, ymax = ~q90, fillcolor = "rgba(120,130,140,0.18)", line = list(width = 0),
                        hoverinfo = "skip", name = "8 in 10 hours") |>
    plotly::add_ribbons(ymin = ~q25, ymax = ~q75, fillcolor = "rgba(120,130,140,0.35)", line = list(width = 0),
                        hoverinfo = "skip", name = "Middle half") |>
    plotly::add_lines(y = ~q50, line = list(color = "#333333", width = 2), text = text, hoverinfo = "text", name = "Median") |>
    plotly::layout(xaxis = .plot_axis(title = "", range = c(1, CLIM_DOY_N), tickvals = first, ticktext = month.abb),
                   yaxis = .plot_axis(title = paste0(c(temperature = "Temperature", relativeHumidity = "RH", windSpeed = "Wind",
                                                       solarRadiation = "Solar")[[var]], " (", m$units, ")"), range = yr,
                                      showgrid = TRUE, gridcolor = "#eeeeee"),
                   shapes = shapes, hovermode = "x", showlegend = FALSE,
                   margin = list(l = 60, r = 10, t = 10, b = 25),
                   plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)",
                   hoverlabel = list(bgcolor = "white", bordercolor = "#cccccc", font = list(size = 13, color = "#222222"),
                                     align = "left")) |>
    plotly::config(displaylogo = FALSE, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d", "autoScale2d",
                                                                    "zoomIn2d", "zoomOut2d", "resetScale2d"),
                   toImageButtonOptions = list(format = "png", filename = paste0("rx_", var, "_daily_range"), scale = 2))
}
