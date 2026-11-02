# ==============================================================================
# R/mod_rx.R -- the prescription controls (sidebar)
#
# Compact by design: for each linear variable, an on/off checkbox and one
# two-handle range slider (no number boxes). A handle dragged to the end of
# its slider means "no limit on that side". Wind direction is a clockwise
# sector picked from two 16-point compass menus (from, to); it may wrap
# through north. Problems show under the control in plain words; while a
# control has a problem, the last valid prescription stays in use, so charts
# never break mid-edit.
#
# Input ids: on_<var>; sl_<var> for linear variables; min_windDirection and
# max_windDirection for the sector.
#
# mod_rx_server() returns a reactive: the current valid prescription
# (rx data frame, see R/rx.R), debounced by RX_DEBOUNCE_MS.
# Requires R/constants.R and R/rx.R.
# ==============================================================================

# 16-point compass for the wind sector menus: "SW (225\u00b0)" -> "225"
.RX_COMPASS <- stats::setNames(
  as.character(seq(0, 337.5, by = 22.5)),
  paste0(c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW"),
         " (", seq(0, 337.5, by = 22.5), "\u00b0)"))

# Slider value -> limits: a handle at the end of the slider is no limit (NA).
.rx_from_slider <- function(s, var) {
  m <- RX_VARS[RX_VARS$var == var, ]
  if (is.null(s) || length(s) != 2) return(c(NA_real_, NA_real_))
  c(if (s[1] <= m$slider_min) NA_real_ else as.numeric(s[1]),
    if (s[2] >= m$slider_max) NA_real_ else as.numeric(s[2]))
}

# The starting prescription, mapped exactly as the controls would report it.
.rx_example_limits <- function() {
  ex <- RX_EXAMPLE[RX_EXAMPLE$on, ]
  for (i in seq_len(nrow(ex))) {
    if (RX_VARS$kind[RX_VARS$var == ex$var[i]] == "linear") {
      l <- .rx_from_slider(c(ex$min[i], ex$max[i]), ex$var[i]); ex$min[i] <- l[1]; ex$max[i] <- l[2]
    }
  }
  data.frame(var = ex$var, min = ex$min, max = ex$max, stringsAsFactors = FALSE, row.names = NULL)
}

mod_rx_ui <- function(id) {
  ns <- shiny::NS(id)
  ctl <- lapply(seq_len(nrow(RX_VARS)), function(i) {
    v <- RX_VARS[i, ]; d <- RX_EXAMPLE[RX_EXAMPLE$var == v$var, ]
    lab <- paste0(v$label, " (", v$units, ")")
    body <- if (v$kind == "circular") {
      near <- function(x) .RX_COMPASS[which.min(abs((as.numeric(.RX_COMPASS) - x + 180) %% 360 - 180))]
      shiny::tagList(
        shiny::div(class = "rx-sector d-flex align-items-center gap-1",
          shiny::span(class = "small", "From"),
          shiny::selectInput(ns(paste0("min_", v$var)), NULL, .RX_COMPASS, selected = near(d$min), selectize = FALSE, width = "100%"),
          shiny::span(class = "small", "to"),
          shiny::selectInput(ns(paste0("max_", v$var)), NULL, .RX_COMPASS, selected = near(d$max), selectize = FALSE, width = "100%")),
        shiny::div(class = "rx-note", "Clockwise; NW to NE wraps through north. Calm hours pass."))
    } else {
      shiny::sliderInput(ns(paste0("sl_", v$var)), NULL, min = v$slider_min, max = v$slider_max,
                         value = c(d$min, d$max), step = v$step, ticks = FALSE, width = "100%")
    }
    shiny::div(class = "rx-var",
      shiny::checkboxInput(ns(paste0("on_", v$var)), lab, value = d$on),
      shiny::conditionalPanel(sprintf("input['%s']", ns(paste0("on_", v$var))),
        class = "rx-body", body, shiny::uiOutput(ns(paste0("msg_", v$var)))))
  })
  shiny::tagList(
    shiny::div(class = "sb-head", "Prescription"),
    shiny::div(class = "rx-note mb-2", "Example values, not a recommendation. Drag a handle to the end for no limit on that side."),
    ctl)
}

mod_rx_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    # ---- assemble, check, keep the last valid one ------------------------------------
    num <- function(x) if (is.null(x) || length(x) == 0 || is.na(x) || !nzchar(x)) NA_real_ else as.numeric(x)
    raw <- shiny::reactive({
      on <- vapply(RX_VARS$var, function(v) isTRUE(input[[paste0("on_", v)]]), logical(1))
      vars <- RX_VARS$var[on]
      lim <- lapply(vars, function(v) {
        if (RX_VARS$kind[RX_VARS$var == v] == "linear") .rx_from_slider(input[[paste0("sl_", v)]], v)
        else c(num(input[[paste0("min_", v)]]), num(input[[paste0("max_", v)]]))
      })
      data.frame(var = vars,
                 min = vapply(lim, `[`, numeric(1), 1),
                 max = vapply(lim, `[`, numeric(1), 2),
                 stringsAsFactors = FALSE, row.names = NULL)
    })
    for (v in RX_VARS$var) local({
      var <- v
      output[[paste0("msg_", var)]] <- shiny::renderUI({
        r <- raw(); r <- r[r$var == var, ]
        if (!nrow(r)) return(NULL)
        p <- rx_problems(r)
        if (!length(p) && is.na(r$min) && is.na(r$max)) p <- "The slider spans its full range: this limit does nothing."
        if (length(p)) shiny::div(class = "small text-danger", paste(p, collapse = " "))
      })
    })
    last_ok <- shiny::reactiveVal(rx_validate(.rx_example_limits()))
    deb <- shiny::debounce(raw, RX_DEBOUNCE_MS)
    shiny::observeEvent(deb(), {
      r <- deb()
      if (!length(rx_problems(r))) last_ok(r)
    })
    shiny::reactive(last_ok())
  })
}
