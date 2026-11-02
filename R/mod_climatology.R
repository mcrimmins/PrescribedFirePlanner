# ==============================================================================
# R/mod_climatology.R -- Climatology tab (v0.1: month x hour)
#
# Flags the baseline years against the current prescription, summarises with
# clim_summarise() (the one aggregation), and draws the month x hour heatmap
# plus the same numbers as a table. Clock-time toggle only at DST stations.
# Takes reactives st() (station object) and rx() (prescription).
# Requires R/constants.R, R/hourly.R, R/rx.R, R/climatology.R,
# R/tooltips.R, R/plots.R.
# ==============================================================================

mod_climatology_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    full_screen = TRUE,
    bslib::card_header(shiny::uiOutput(ns("title"))),
    bslib::card_body(
      shiny::div(class = "d-flex flex-wrap align-items-center gap-3",
        shiny::checkboxInput(ns("outline_on"), "Outline cells at or above", value = TRUE),
        shiny::div(style = "width: 220px;",
                   shiny::sliderInput(ns("outline_pct"), NULL, min = 0, max = 100, value = CLIM_HIGHLIGHT_PCT,
                                      step = 5, post = "%", ticks = FALSE, width = "100%")),
        shiny::div(class = "small text-muted", "of hours in prescription"),
        shiny::uiOutput(ns("controls"))),
      bslib::navset_underline(
        bslib::nav_panel("Heatmap", plotly::plotlyOutput(ns("heat"), height = "560px")),
        bslib::nav_panel("Table", shiny::div(class = "small text-muted my-2",
                                             "% of hours in prescription, by month (columns) and hour (rows). Bold: at or above the outline threshold."),
                         shiny::tableOutput(ns("tbl")))),
      shiny::uiOutput(ns("caption"))))
}

mod_climatology_server <- function(id, st, rx) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    hour_col <- shiny::reactive(if (isTRUE(input$clock)) "hour_clock" else "hour")
    # outline threshold in %, or NA when switched off (debounced while dragging)
    thr_raw <- shiny::reactive(if (isTRUE(input$outline_on) && !is.null(input$outline_pct)) input$outline_pct else NA_real_)
    thr <- shiny::debounce(thr_raw, RX_DEBOUNCE_MS)

    base <- shiny::reactive({
      shiny::req(st())
      hourly_baseline(st()$hourly)
    })
    summ <- shiny::reactive({
      r <- rx(); b <- base()
      fl <- rx_flag(b, r)
      vars <- rx_constrained(r)
      vals <- c("in_rx", paste0("pass_", vars), intersect(vars, c("temperature", "relativeHumidity", "windSpeed",
                                                                 "precipitation", "solarRadiation")))
      list(long = clim_summarise(fl, vals, by = c("month", hour_col())), vars = vars, n = nrow(fl),
           years = range(fl$year))
    })

    output$title <- shiny::renderUI({
      if (is.null(st())) return("Climatology")
      shiny::span("When is ", shiny::strong(st()$meta$name), " in prescription? ",
                  shiny::span(class = "text-muted fw-normal", "% of hours, by month and hour of day"))
    })
    output$controls <- shiny::renderUI({
      shiny::req(st())
      if (!isTRUE(st()$meta$has_dst)) return(NULL)
      shiny::checkboxInput(ns("clock"), "Show clock time (daylight saving) instead of local standard time (LST)",
                           value = isTRUE(shiny::isolate(input$clock)))
    })

    output$heat <- plotly::renderPlotly({
      shiny::validate(shiny::need(st(), "Load a station (sidebar or Station tab) to see its climatology."))
      s <- summ()
      cells <- tip_cells(s$long, x_col = "month", hour_col = hour_col(), rx_vars = s$vars,
                         obs_minute = as.integer(names(which.max(table(st()$hourly$obs_minute)))),
                         threshold = thr())
      ir <- s$long[s$long$value == "in_rx", ]
      cells$stat <- ir$stat[match(paste(cells$month, cells[[hour_col()]]), paste(ir$month, ir[[hour_col()]]))]
      plot_hour_heatmap(cells, hour_col = hour_col(), clock = hour_col() == "hour_clock", threshold = thr())
    })

    output$tbl <- shiny::renderTable({
      shiny::validate(shiny::need(st(), "Load a station first."))
      ir <- summ()$long; ir <- ir[ir$value == "in_rx", ]
      hc <- hour_col()
      m <- matrix("", 24, 12, dimnames = list(sprintf("%02d:00", 0:23), month.abb))
      pct <- ifelse(is.na(ir$stat), "", paste0(round(100 * ir$stat), "%"))
      t <- thr()
      if (!is.na(t)) pct <- ifelse(!is.na(ir$stat) & 100 * ir$stat >= t, paste0("<b>", pct, "</b>"), pct)
      m[cbind(ir[[hc]] + 1, ir$month)] <- pct
      data.frame(Hour = rownames(m), m, check.names = FALSE)
    }, striped = TRUE, spacing = "xs", align = "c", sanitize.text.function = function(x) x)

    output$caption <- shiny::renderUI({
      shiny::req(st())
      s <- summ(); ir <- s$long[s$long$value == "in_rx", ]
      excl <- sum(ir$n_missing); tot <- sum(ir$n_valid) + excl
      lim <- if (length(s$vars)) paste(RX_VARS$label[match(s$vars, RX_VARS$var)], collapse = ", ") else "none (every hour counts)"
      shiny::div(class = "small text-muted mt-2",
                 shiny::p(sprintf("Years %d-%d. Each cell pools every hour in that month and hour across those years. Limits on: %s.%s",
                                  s$years[1], s$years[2], lim,
                                  if (is.na(thr())) "" else sprintf(" Outlined: cells where at least %d%% of hours are in prescription.", round(thr())))),
                 shiny::p(sprintf("%s of %s hours (%s) are excluded because a limited variable is missing or was screened out; see Data quality.",
                                  format(excl, big.mark = ","), format(tot, big.mark = ","), tip_pct(excl / tot))))
    })
  })
}
