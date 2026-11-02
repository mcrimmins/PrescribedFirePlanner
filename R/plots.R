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
