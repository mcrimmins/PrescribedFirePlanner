# ==============================================================================
# R/plots.R -- charts only draw (no calculation here)
#
# Each function takes summaries (clim_summarise / quality_* output) plus
# tooltip text from R/tooltips.R, and returns a plotly widget.
# Colour: prescription heatmaps use .rx_scale() -- warm below the threshold,
# blue above, always darker as the share rises, in steps (docs/SPEC.md
# "Colour"); lines over them are haloed (.plot_halo_line / _contour). The data
# quality grid keeps the single-hue CHART_SEQ. 2 px gaps between cells;
# tooltips on every cell; recessive axes.
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

# ---- prescription colour scale and haloed lines (docs/SPEC.md "Colour") ---------------------
# A stepped warm/cool colourscale from 0 to zmax, split at `split`: warm below,
# blue above, always darker as the value rises; one colour per step, and the
# split is always a step edge. Returns list(colorscale, tickvals, ticktext).
.rx_scale <- function(split, zmax = 100, step = CHART_RX_STEP, unit = "%", split_label = "threshold") {
  split <- min(max(split, 0), zmax)
  br <- sort(unique(c(seq(0, zmax, by = step), split, zmax)))
  cw <- grDevices::colorRamp(CHART_RX_WARM); cc <- grDevices::colorRamp(CHART_RX_COOL)
  col_at <- function(z) {
    v <- if (z < split) cw(if (split > 0) z / split else 0) else cc(if (zmax > split) (z - split) / (zmax - split) else 1)
    grDevices::rgb(v[1], v[2], v[3], maxColorValue = 255)
  }
  cs <- list()
  for (k in seq_len(length(br) - 1)) {
    col <- col_at((br[k] + br[k + 1]) / 2)
    cs <- c(cs, list(list(br[k] / zmax, col), list(br[k + 1] / zmax, col)))
  }
  lab <- paste0(format(br, trim = TRUE), unit)
  lab[br == split] <- paste0("<b>", lab[br == split], " ", split_label, "</b>")
  list(colorscale = cs, tickvals = br, ticktext = lab, colour_at = col_at)
}

# A line drawn twice: a wider halo underneath, so it shows on light and dark cells.
# legend = "line" puts the visible line in the legend; "halo" the wider one underneath (for white lines,
# which would vanish on the white legend).
.plot_halo_line <- function(p, x, y, color = CHART_LINE, halo = CHART_LINE_HALO, width = 1.4, dash = "solid",
                            name = NULL, showlegend = FALSE, legend = "line", ...) {
  p <- plotly::add_lines(p, x = x, y = y, line = list(color = halo, width = width + 2.4), hoverinfo = "skip",
                         name = name, showlegend = showlegend && legend == "halo", ...)
  plotly::add_lines(p, x = x, y = y, line = list(color = color, width = width, dash = dash), hoverinfo = "skip",
                    name = name, showlegend = showlegend && legend == "line", ...)
}
.plot_halo_contour <- function(p, x, y, z, level, width = 2.2) {
  for (w in list(list(CHART_LINE_HALO, width + 2.4), list(CHART_LINE, width)))
    p <- plotly::add_contour(p, x = x, y = y, z = z, hoverinfo = "skip", showscale = FALSE, showlegend = FALSE,
                             contours = list(start = level, end = level, size = 1, coloring = "none"),
                             line = list(color = w[[1]], width = w[[2]]))
  p
}
.rx_colorbar <- function(sc, title) list(title = list(text = title, font = list(size = 12)), tickvals = sc$tickvals,
                                         ticktext = sc$ticktext, len = 0.9, outlinewidth = 0, thickness = 14)

# Month x hour heatmap of a share (0-1). `cells` has the month and hour column,
# `stat` (share), and `text` (from tip_cells). Hours run top to bottom like a
# timetable: midnight at the top, late evening at the bottom.
# `threshold` (0-100, or NA for none): cells at or above it get an outline in
# black (white-haloed) outline, so the window shows by line as well as by colour.
plot_hour_heatmap <- function(cells, hour_col = "hour", x_col = "month", clock = FALSE,
                              colorbar_title = "% of hours<br>in prescription",
                              threshold = NA, split = if (is.na(threshold)) CLIM_HIGHLIGHT_PCT else threshold, ign = NULL) {
  xs <- sort(unique(cells[[x_col]])); hs <- 0:23
  z <- matrix(NA_real_, nrow = 24, ncol = length(xs)); tx <- matrix("", nrow = 24, ncol = length(xs))
  i <- match(cells[[hour_col]], hs); j <- match(cells[[x_col]], xs)
  z[cbind(i, j)] <- 100 * cells$stat
  tx[cbind(i, j)] <- cells$text
  xlab <- if (x_col == "month") month.abb[xs] else as.character(xs)
  shapes <- list()
  # outline cells at or above the threshold: black over a white halo
  if (!is.na(threshold)) {
    hit <- which(!is.na(z) & z >= threshold, arr.ind = TRUE)
    rect <- function(k, col, w) {
      r <- hit[k, 1] - 1; cc <- hit[k, 2] - 1          # 0-based category positions
      list(type = "rect", xref = "x", yref = "y", x0 = cc - 0.5, x1 = cc + 0.5, y0 = r - 0.5, y1 = r + 0.5,
           line = list(color = col, width = w), fillcolor = "rgba(0,0,0,0)", layer = "above")
    }
    shapes <- c(lapply(seq_len(nrow(hit)), rect, col = CHART_LINE_HALO, w = 4),
                lapply(seq_len(nrow(hit)), rect, col = CHART_LINE, w = 1.8))
  }
  # `ign`: data frame of x (same column as cells), start, end in the chart's hours: the
  # ignition period as dashed lines across each column (a wash would read as "fewer hours")
  if (!is.null(ign) && nrow(ign)) {
    seg <- function(jx, yv, col, w, dash) list(type = "line", xref = "x", yref = "y", x0 = jx - 0.5, x1 = jx + 0.5,
                                               y0 = yv - 0.5, y1 = yv - 0.5, line = list(color = col, width = w, dash = dash),
                                               layer = "above")
    jj <- match(ign[[x_col]], xs) - 1
    for (k in which(!is.na(jj))) for (yv in c(ign$start[k], ign$end[k]))
      shapes <- c(shapes, list(seg(jj[k], yv, CHART_LINE_HALO, 4, "solid"), seg(jj[k], yv, CHART_LINE, 1.6, "dash")))
  }
  sc <- .rx_scale(split)
  plotly::plot_ly(x = xlab, y = sprintf("%02d:00", hs), z = z, text = tx, type = "heatmap",
                  colorscale = sc$colorscale, zmin = 0, zmax = 100,
                  xgap = CHART_CELL_GAP, ygap = CHART_CELL_GAP,
                  hovertemplate = "%{text}<extra></extra>",
                  colorbar = .rx_colorbar(sc, colorbar_title)) |>
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
    failed <- !is.na(pass) & !pass & !(!is.na(flagged$in_rx) & flagged$in_rx)
    p <- plotly::add_markers(p, x = x, y = y, yaxis = yname(ax), text = text, hoverinfo = "text",
                             marker = list(color = col, size = ifelse(failed, if (circ) 8 else 7, if (circ) 6 else 5),
                                           symbol = ifelse(failed, "x-thin-open", "circle"),
                                           line = list(color = col, width = ifelse(failed, 2, 0))),
                             showlegend = FALSE)
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
plot_year_day <- function(days, labels, zmax = 24, split = SAME_MIN_RUN_H) {
  yrs <- sort(unique(days$wyear))
  z <- matrix(NA_real_, length(yrs), length(labels)); tx <- matrix("", length(yrs), length(labels))
  i <- match(days$wyear, yrs); j <- match(days$wday, labels)
  ok <- !is.na(j)
  z[cbind(i, j)[ok, , drop = FALSE]] <- ifelse(days$n_valid[ok] > 0, days$n_true[ok], NA)
  tx[cbind(i, j)[ok, , drop = FALSE]] <- days$text[ok]
  u <- which(ok & days$usable)
  rect <- function(k, col, w)
    list(type = "rect", xref = "x", yref = "y", x0 = j[k] - 1.5, x1 = j[k] - 0.5, y0 = i[k] - 1.5, y1 = i[k] - 0.5,
         line = list(color = col, width = w), fillcolor = "rgba(0,0,0,0)", layer = "above")
  shapes <- c(lapply(u, rect, col = CHART_LINE_HALO, w = 2.6), lapply(u, rect, col = CHART_LINE, w = 1.1))
  sc <- .rx_scale(split, zmax = zmax, step = if (zmax > 12) 2 else 1, unit = " h", split_label = "min window")
  step <- if (length(labels) > 45) 7 else if (length(labels) > 20) 3 else 1
  tv <- labels[seq(1, length(labels), by = step)]
  plotly::plot_ly(x = labels, y = as.character(yrs), z = z, text = tx, type = "heatmap",
                  colorscale = sc$colorscale, zmin = 0, zmax = zmax,
                  xgap = 1, ygap = 1, hovertemplate = "%{text}<extra></extra>",
                  colorbar = .rx_colorbar(sc, paste0("Hours in<br>prescription<br>(of ", zmax, ")"))) |>
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
                             split = if (is.na(threshold)) CLIM_HIGHLIGHT_PCT else threshold) {
  N <- CLIM_DOY_N
  x <- sort(unique(cells$doy)); y <- 0:23 + 0.5      # x: the days drawn (every CLIM_DOY_STEP-th)
  z <- matrix(NA_real_, 24, length(x)); tx <- matrix("", 24, length(x))
  j <- match(cells$doy, x)
  z[cbind(cells[[hour_col]] + 1, j)] <- 100 * cells$stat
  tx[cbind(cells[[hour_col]] + 1, j)] <- cells$text
  first <- as.integer(format(as.Date(sprintf("2001-%02d-01", 1:12)), "%j"))
  sc <- .rx_scale(split)
  p <- plotly::plot_ly() |>
    plotly::add_heatmap(x = x, y = y, z = z, text = tx, hovertemplate = "%{text}<extra></extra>",
                        colorscale = sc$colorscale, zmin = 0, zmax = 100,
                        colorbar = .rx_colorbar(sc, "% of hours<br>in prescription"))
  if (!is.na(threshold)) p <- .plot_halo_contour(p, x, y, z, threshold)
  if (!is.null(sun)) {
    p <- .plot_halo_line(p, sun$doy, sun$sunrise, CHART_SUN, CHART_SUN_HALO, 1.4, name = "Sunrise / sunset", showlegend = TRUE,
                         legend = "halo")
    p <- .plot_halo_line(p, sun$doy, sun$sunset, CHART_SUN, CHART_SUN_HALO, 1.4)
    p <- plotly::add_lines(p, x = sun$doy, y = sun$civil_dawn, line = list(color = CHART_SUN_HALO, width = 1.2, dash = "dot"),
                           hoverinfo = "skip", name = "Civil twilight", showlegend = TRUE)
    p <- .plot_halo_line(p, sun$doy, sun$civil_dawn, CHART_SUN, CHART_SUN_HALO, 1.0, dash = "dot")
    p <- .plot_halo_line(p, sun$doy, sun$civil_dusk, CHART_SUN, CHART_SUN_HALO, 1.0, dash = "dot")
  }
  if (!is.null(ign)) {
    p <- .plot_halo_line(p, ign$doy, ign$start, width = 1.4, dash = "dash", name = "Ignition period", showlegend = TRUE)
    p <- .plot_halo_line(p, ign$doy, ign$end, width = 1.4, dash = "dash")
  }
  if (!is.na(threshold))
    p <- plotly::add_lines(p, x = c(-20, -19), y = c(-5, -5), line = list(color = CHART_LINE, width = 2.2), hoverinfo = "skip",
                           name = sprintf("Threshold: %d%% of hours in prescription", round(threshold)), showlegend = TRUE)
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
                         split = if (is.na(threshold)) CLIM_HIGHLIGHT_PCT else threshold) {
  x <- sort(unique(cells$doy)); y <- 0:23 + 0.5
  z <- matrix(NA_real_, 24, length(x)); tx <- matrix("", 24, length(x))
  ij <- cbind(cells[[hour_col]] + 1, match(cells$doy, x))
  z[ij] <- 100 * cells$pass
  tx[ij] <- cells$text
  sc <- .rx_scale(split)
  cb <- .rx_colorbar(sc, "% of hours<br>within this limit"); cb$len <- 0.95; cb$thickness <- 12
  p <- plotly::plot_ly() |>
    plotly::add_heatmap(x = x, y = y, z = z, text = tx, hovertemplate = "%{text}<extra></extra>",
                        colorscale = sc$colorscale, zmin = 0, zmax = 100, colorbar = cb)
  if (!is.na(threshold)) p <- .plot_halo_contour(p, x, y, z, threshold, width = 1.8)
  if (!is.null(sun)) for (col in c("sunrise", "sunset"))
    p <- .plot_halo_line(p, sun$doy, sun[[col]], CHART_SUN, CHART_SUN_HALO, 1.1)
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

# ---- Season tracker: daily hours, stacked panels ------------------------------------------
# days: totals_season_days() with `text` (tip_season_days); bands: the reference
# (doy, value, q25, q50, q75, q100). measures: "in_rx" then "pass_<var>" per limit.
# Bars: observed (blue) and FEMS forecast (hatched), stacked for today; usable
# days get an orange tick above the bar. Behind: reference middle half (band),
# typical year (line), record (dotted). xrange: the dates shown first. Clicks
# are reported on plotly source "season".
plot_season <- function(days, bands, measures, titles, xrange, today = NULL, ymax = 24, year = NULL) {
  k <- length(measures)
  units <- c(1.6, rep(1, k - 1)); gap <- 0.06
  tot <- sum(units) + gap * (k - 1)
  top <- 1 - c(0, cumsum(units + gap))[seq_len(k)] / tot
  bottom <- top - units / tot
  # three y axes per panel, drawn in order: the band (base), the bars (overlaid
  # on it), and the reference lines (overlaid on the bars). plotly draws bars
  # under any filled scatter on the same axes, so the band needs its own axis
  # to sit behind them.
  ax <- function(i, layer) i + (layer - 1) * k
  yname <- function(n) if (n == 1) "y" else paste0("y", n)
  ylay  <- function(n) if (n == 1) "yaxis" else paste0("yaxis", n)
  x <- format(days$date, "%Y-%m-%d")
  ref_x <- format(as.Date(sprintf("%d-01-01", year)) + seq_len(365 + (year %% 4 == 0)) - 1, "%Y-%m-%d")
  ref_doy <- {
    r <- as.Date(ref_x); d <- as.integer(format(as.Date(format(r, "2001-%m-%d"), optional = TRUE), "%j")); d[is.na(d)] <- 59L; d
  }
  p <- plotly::plot_ly(source = "season")
  lay <- list(); shapes <- list(); ann <- list()
  for (i in seq_len(k)) {
    m <- measures[i]
    b <- bands[bands$value == m, ]; j <- match(ref_doy, b$doy)
    rng <- c(0, ymax * (if (i == 1) 1.12 else 1.02))
    p <- plotly::add_ribbons(p, x = ref_x, ymin = b$q25[j], ymax = b$q75[j], yaxis = yname(ax(i, 1)),
                             fillcolor = "rgba(120,130,140,0.22)", line = list(width = 0), hoverinfo = "skip",
                             name = "Middle half of years", legendgroup = "band", showlegend = i == 1)
    obs <- days[[paste0(m, "_obs")]]; fc <- days[[paste0(m, "_fc")]]
    p <- plotly::add_bars(p, x = x, y = obs, yaxis = yname(ax(i, 2)), text = days$text, hoverinfo = "text", textposition = "none",
                          marker = list(color = CHART_IN_RX), name = "Observed", legendgroup = "obs", showlegend = i == 1)
    if (any(fc > 0))
      p <- plotly::add_bars(p, x = x, y = fc, yaxis = yname(ax(i, 2)), text = days$text, hoverinfo = "text", textposition = "none",
                            marker = list(color = "rgba(11,94,128,0.25)", line = list(color = CHART_IN_RX, width = 1),
                                          pattern = list(shape = "/", fgcolor = CHART_IN_RX, size = 5, solidity = 0.3)),
                            name = "FEMS forecast", legendgroup = "fc", showlegend = i == 1)
    if (i == 1 && any(days$usable)) {
      u <- days$usable
      p <- plotly::add_markers(p, x = x[u], y = (obs + fc)[u] + ymax * 0.05, yaxis = yname(ax(i, 2)), text = days$text[u],
                               hoverinfo = "text", marker = list(symbol = "triangle-down", size = 7, color = CHART_USABLE,
                                             line = list(color = CHART_LINE, width = 0.8)),
                               name = "Usable day", showlegend = TRUE)
    }
    p <- plotly::add_lines(p, x = ref_x, y = b$q100[j], yaxis = yname(ax(i, 3)), line = list(color = "#8a949c", width = 1, dash = "dot"),
                           hoverinfo = "skip", name = "Record (2005-2025)", legendgroup = "max", showlegend = i == 1)
    p <- plotly::add_lines(p, x = ref_x, y = b$q50[j], yaxis = yname(ax(i, 3)), line = list(color = "#333333", width = 1.6),
                           hoverinfo = "skip", name = "Typical year (median)", legendgroup = "med", showlegend = i == 1)
    lay[[ylay(ax(i, 1))]] <- .plot_axis(domain = c(bottom[i], top[i]), range = rng,
                                        title = list(text = titles[i], font = list(size = 11, color = "#555555")),
                                        showgrid = TRUE, gridcolor = "#eeeeee", fixedrange = TRUE)
    for (l in 2:3) lay[[ylay(ax(i, l))]] <- list(overlaying = yname(ax(i, 1)), range = rng, visible = FALSE, fixedrange = TRUE)
  }
  if (!is.null(today)) {
    shapes <- list(list(type = "line", xref = "x", yref = "paper", x0 = format(today), x1 = format(today), y0 = 0, y1 = 1,
                        line = list(color = "#444444", width = 1, dash = "dash")))
    ann <- list(list(x = format(today), y = 1, xref = "x", yref = "paper", text = "today", showarrow = FALSE,
                     yanchor = "bottom", font = list(size = 11, color = "#444444")))
  }
  lay$xaxis <- .plot_axis(type = "date", range = format(xrange), fixedrange = FALSE, showgrid = TRUE, gridcolor = "#eeeeee",
                          tickformat = "%d %b", anchor = yname(ax(k, 1)))
  lay$barmode <- "stack"; lay$bargap <- 0.15
  lay$shapes <- shapes; lay$annotations <- ann
  lay$hovermode <- "closest"; lay$dragmode <- "zoom"
  lay$legend <- list(orientation = "h", x = 0, y = -0.03, yanchor = "top", font = list(size = 11))
  lay$margin <- list(l = 70, r = 20, t = 30, b = 40)
  lay$plot_bgcolor <- "rgba(0,0,0,0)"; lay$paper_bgcolor <- "rgba(0,0,0,0)"
  lay$hoverlabel <- list(bgcolor = "white", bordercolor = "#cccccc", font = list(size = 13, color = "#222222"), align = "left")
  p <- do.call(plotly::layout, c(list(p), lay))
  p <- plotly::event_register(p, "plotly_click")
  plotly::config(p, displaylogo = FALSE, modeBarButtonsToRemove = c("select2d", "lasso2d", "autoScale2d"),
                 toImageButtonOptions = list(format = "png", filename = "rx_season", scale = 2))
}
