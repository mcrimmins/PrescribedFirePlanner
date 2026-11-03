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
                              threshold = NA, colors = CHART_SEQ, highlight = CHART_HIGHLIGHT) {
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
  p <- plotly::plot_ly()
  p <- plotly::add_bars(p, x = x, y = rep(1, n), yaxis = "y", text = text, hoverinfo = "text", textposition = "none",
                        marker = list(color = c(`in` = CHART_IN_RX, out = CHART_OUT, unknown = "#ffffff")[st],
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
