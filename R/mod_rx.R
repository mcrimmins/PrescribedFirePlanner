# ==============================================================================
# R/mod_rx.R -- the prescription controls (sidebar)
#
# For each linear variable: an on/off checkbox, min and max number boxes, and
# a two-handle range slider, kept in sync both ways. The number boxes are the
# value that counts (exact entry); the slider is for quick exploring. Wind
# direction is a clockwise sector (from, to) that may wrap through north.
# A blank box means "no limit on that side". Problems show under the control
# in plain words; while a control has a problem, the last valid prescription
# stays in use, so charts never break mid-edit.
#
# mod_rx_server() returns a reactive: the current valid prescription
# (rx data frame, see R/rx.R), debounced by RX_DEBOUNCE_MS.
# Requires R/constants.R and R/rx.R.
# ==============================================================================

mod_rx_ui <- function(id) {
  ns <- shiny::NS(id)
  ctl <- lapply(seq_len(nrow(RX_VARS)), function(i) {
    v <- RX_VARS[i, ]; d <- RX_EXAMPLE[RX_EXAMPLE$var == v$var, ]
    lab <- paste0(v$label, " (", v$units, ")")
    boxes <- shiny::div(class = "d-flex gap-2",
      shiny::div(class = "flex-fill", shiny::numericInput(ns(paste0("min_", v$var)), if (v$kind == "circular") "From" else "Min",
                                                          value = d$min, min = v$slider_min, max = v$slider_max, step = v$step, width = "100%")),
      shiny::div(class = "flex-fill", shiny::numericInput(ns(paste0("max_", v$var)), if (v$kind == "circular") "To" else "Max",
                                                          value = d$max, min = v$slider_min, max = v$slider_max, step = v$step, width = "100%")))
    shiny::div(class = "rx-var mb-2",
      shiny::checkboxInput(ns(paste0("on_", v$var)), shiny::strong(lab), value = d$on),
      shiny::conditionalPanel(sprintf("input['%s']", ns(paste0("on_", v$var))),
        boxes,
        if (v$kind == "circular")
          shiny::div(class = "small text-muted", "Clockwise sector; e.g. 315 to 45 wraps through north. Calm hours pass.")
        else shiny::sliderInput(ns(paste0("sl_", v$var)), NULL, min = v$slider_min, max = v$slider_max,
                                value = c(d$min, d$max), step = v$step, ticks = FALSE, width = "100%"),
        shiny::uiOutput(ns(paste0("msg_", v$var)))))
  })
  shiny::tagList(
    shiny::h6("Prescription", class = "mt-2 mb-1"),
    shiny::div(class = "small text-muted mb-2", "Starting values are an example, not a recommendation. A blank box means no limit on that side."),
    ctl)
}

mod_rx_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    linear <- RX_VARS$var[RX_VARS$kind == "linear"]

    # ---- keep number boxes and slider in sync --------------------------------------
    for (v in linear) local({
      var <- v; step <- RX_VARS$step[RX_VARS$var == var]
      lo_id <- paste0("min_", var); hi_id <- paste0("max_", var); sl_id <- paste0("sl_", var)
      # slider dragged -> boxes (only for real moves: a typed off-step value
      # that the slider snapped must not be overwritten)
      shiny::observeEvent(input[[sl_id]], {
        s <- input[[sl_id]]; lo <- input[[lo_id]]; hi <- input[[hi_id]]
        if (is.null(lo) || is.na(lo) || abs(s[1] - lo) >= 0.99 * step) shiny::updateNumericInput(session, lo_id, value = s[1])
        if (is.null(hi) || is.na(hi) || abs(s[2] - hi) >= 0.99 * step) shiny::updateNumericInput(session, hi_id, value = s[2])
      }, ignoreInit = TRUE)
      # boxes typed -> slider (only when the pair is valid)
      shiny::observeEvent(list(input[[lo_id]], input[[hi_id]]), {
        lo <- input[[lo_id]]; hi <- input[[hi_id]]; s <- input[[sl_id]]
        m <- RX_VARS[RX_VARS$var == var, ]
        lo2 <- if (is.null(lo) || is.na(lo)) m$slider_min else lo
        hi2 <- if (is.null(hi) || is.na(hi)) m$slider_max else hi
        if (lo2 >= m$slider_min && hi2 <= m$slider_max && lo2 <= hi2 && !is.null(s) &&
            (abs(s[1] - lo2) > 1e-9 || abs(s[2] - hi2) > 1e-9))
          shiny::updateSliderInput(session, sl_id, value = c(lo2, hi2))
      }, ignoreInit = TRUE)
    })

    # ---- assemble, check, keep the last valid one ------------------------------------
    num <- function(x) if (is.null(x) || length(x) == 0 || is.na(x)) NA_real_ else as.numeric(x)
    raw <- shiny::reactive({
      on <- vapply(RX_VARS$var, function(v) isTRUE(input[[paste0("on_", v)]]), logical(1))
      vars <- RX_VARS$var[on]
      data.frame(var = vars,
                 min = vapply(vars, function(v) num(input[[paste0("min_", v)]]), numeric(1)),
                 max = vapply(vars, function(v) num(input[[paste0("max_", v)]]), numeric(1)),
                 stringsAsFactors = FALSE, row.names = NULL)
    })
    for (v in RX_VARS$var) local({
      var <- v
      output[[paste0("msg_", var)]] <- shiny::renderUI({
        r <- raw(); r <- r[r$var == var, ]
        if (!nrow(r)) return(NULL)
        p <- rx_problems(r)
        if (!length(p) && is.na(r$min) && is.na(r$max)) p <- "Both boxes are blank: this limit does nothing."
        if (length(p)) shiny::div(class = "small text-danger", paste(p, collapse = " "))
      })
    })
    last_ok <- shiny::reactiveVal(rx_validate(data.frame(
      var = RX_EXAMPLE$var[RX_EXAMPLE$on], min = RX_EXAMPLE$min[RX_EXAMPLE$on], max = RX_EXAMPLE$max[RX_EXAMPLE$on],
      stringsAsFactors = FALSE)))
    deb <- shiny::debounce(raw, RX_DEBOUNCE_MS)
    shiny::observeEvent(deb(), {
      r <- deb()
      if (!length(rx_problems(r))) last_ok(r)
    })
    shiny::reactive(last_ok())
  })
}
