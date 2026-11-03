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
# A preset menu at the top fills every control (DRAFT presets, RX_PRESETS);
# the note under it says when the values have been edited. Below the limits:
# the ignition period (clock time) and the minimum usable window.
#
# mod_rx_server() returns a reactive: the current valid prescription
# (rx data frame, see R/rx.R), debounced by RX_DEBOUNCE_MS, carrying the
# attributes "ignition", "min_window" and "preset" (rx_ignition(),
# rx_min_window()).
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

# A preset's limits, mapped exactly as the controls would report them
# (a slider end is no limit; wind direction rounds to the 16 compass points).
.rx_limits_of <- function(rows) {
  ex <- rows[rows$on, ]
  for (i in seq_len(nrow(ex))) {
    if (RX_VARS$kind[RX_VARS$var == ex$var[i]] == "linear") {
      l <- .rx_from_slider(c(ex$min[i], ex$max[i]), ex$var[i]); ex$min[i] <- l[1]; ex$max[i] <- l[2]
    } else {
      pts <- as.numeric(.RX_COMPASS)
      ex$min[i] <- pts[which.min(abs((pts - ex$min[i] + 180) %% 360 - 180))]
      ex$max[i] <- pts[which.min(abs((pts - ex$max[i] + 180) %% 360 - 180))]
    }
  }
  data.frame(var = ex$var, min = ex$min, max = ex$max, stringsAsFactors = FALSE, row.names = NULL)
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
    shiny::div(class = "rx-preset",
      shiny::selectInput(ns("preset"), NULL, stats::setNames(RX_PRESET_META$preset, RX_PRESET_META$label),
                         selected = "example", selectize = FALSE, width = "100%"),
      shiny::uiOutput(ns("preset_note"))),
    ctl,
    shiny::div(class = "rx-var",
      shiny::checkboxInput(ns("ign_on"), "Ignition period (clock time)", value = FALSE),
      shiny::conditionalPanel(sprintf("input['%s']", ns("ign_on")), class = "rx-body",
        shiny::sliderInput(ns("ign"), NULL, min = 0, max = 24, value = c(10, 17), step = 1, post = ":00",
                           ticks = FALSE, width = "100%"),
        shiny::div(class = "rx-note", "Counts, runs and usable days use only these hours."))),
    shiny::div(class = "rx-var",
      shiny::div(class = "rx-label", "Minimum usable window"),
      shiny::div(class = "rx-body",
        shiny::sliderInput(ns("min_window"), NULL, min = 1, max = 12, value = SAME_MIN_RUN_H, step = 1, post = " h",
                           ticks = FALSE, width = "100%"),
        shiny::div(class = "rx-note", "A usable day has at least this many consecutive hours in prescription."))))
}

# The sidebar values a preset sets, as the controls hold them.
.rx_preset_values <- function(id) {
  p <- RX_PRESETS[RX_PRESETS$preset == id, ]; m <- RX_PRESET_META[RX_PRESET_META$preset == id, ]
  list(rows = p, ign = if (is.na(m$ign_start)) NULL else c(m$ign_start, m$ign_end), min_window = m$min_window, meta = m)
}

mod_rx_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    # ---- a preset fills every control ------------------------------------------------
    near <- function(x) .RX_COMPASS[which.min(abs((as.numeric(.RX_COMPASS) - x + 180) %% 360 - 180))]
    shiny::observeEvent(input$preset, {
      pv <- .rx_preset_values(input$preset)
      for (i in seq_len(nrow(pv$rows))) {
        v <- pv$rows$var[i]; kind <- RX_VARS$kind[RX_VARS$var == v]
        shiny::updateCheckboxInput(session, paste0("on_", v), value = pv$rows$on[i])
        if (kind == "linear") {
          m <- RX_VARS[RX_VARS$var == v, ]
          lo <- if (is.na(pv$rows$min[i])) m$slider_min else pv$rows$min[i]
          hi <- if (is.na(pv$rows$max[i])) m$slider_max else pv$rows$max[i]
          shiny::updateSliderInput(session, paste0("sl_", v), value = c(lo, hi))
        } else {
          shiny::updateSelectInput(session, paste0("min_", v), selected = near(pv$rows$min[i]))
          shiny::updateSelectInput(session, paste0("max_", v), selected = near(pv$rows$max[i]))
        }
      }
      shiny::updateCheckboxInput(session, "ign_on", value = !is.null(pv$ign))
      if (!is.null(pv$ign)) shiny::updateSliderInput(session, "ign", value = pv$ign)
      shiny::updateSliderInput(session, "min_window", value = pv$min_window)
    }, ignoreInit = TRUE)

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

    # ---- ignition period, minimum window, and whether the preset was edited -----------
    ign <- shiny::reactive(if (isTRUE(input$ign_on) && length(input$ign) == 2) as.integer(input$ign) else NULL)
    mw <- shiny::reactive(if (is.null(input$min_window)) SAME_MIN_RUN_H else as.integer(input$min_window))
    settings <- shiny::debounce(shiny::reactive(list(ign = ign(), mw = mw())), RX_DEBOUNCE_MS)
    edited <- shiny::reactive({
      pid <- if (is.null(input$preset)) "example" else input$preset
      pv <- .rx_preset_values(pid)
      want <- .rx_limits_of(pv$rows)
      have <- raw()
      !(identical(want$var, have$var) && isTRUE(all.equal(want$min, have$min)) && isTRUE(all.equal(want$max, have$max)) &&
          identical(if (is.null(pv$ign)) NULL else as.integer(pv$ign), ign()) && identical(as.integer(pv$min_window), mw()))
    })
    output$preset_note <- shiny::renderUI({
      pid <- if (is.null(input$preset)) "example" else input$preset
      m <- RX_PRESET_META[RX_PRESET_META$preset == pid, ]
      shiny::div(class = "rx-note mb-2",
                 if (edited()) shiny::tags$b("Edited from this preset. "),
                 m$note, " Draft values for training, not a recommendation. Drag a handle to the end for no limit on that side.")
    })

    shiny::reactive({
      s <- settings()
      pid <- if (is.null(input$preset)) "example" else input$preset
      rx_with_settings(last_ok(), ignition = s$ign, min_window = s$mw,
                       preset = paste0(RX_PRESET_META$label[RX_PRESET_META$preset == pid],
                                       if (shiny::isolate(edited())) " (edited)" else ""))
    })
  })
}
