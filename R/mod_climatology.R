# ==============================================================================
# R/mod_climatology.R -- Climatology tab (v0.1: month x hour)
#
# Flags the baseline years against the current prescription, summarises with
# clim_summarise() (the one aggregation), and draws the month x hour heatmap
# plus the same numbers as a table. Clock-time toggle only at DST stations.
# Takes reactives st() (station object) and rx() (prescription).
# Requires R/constants.R, R/hourly.R, R/rx.R, R/climatology.R,
# R/tooltips.R, R/plots.R, R/totals.R.
#
# Below the chart: hour totals (R/totals.R) by month -- typical year, range
# and total for hours in prescription, the same per limit, and two CSVs.
# ==============================================================================

mod_climatology_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(bslib::card(
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
      shiny::uiOutput(ns("caption")))),
    bslib::card(
      bslib::card_header(class = "d-flex flex-wrap align-items-center gap-2",
        shiny::span("Hours in prescription, by month ", shiny::span(class = "text-muted fw-normal", "(counts, for reporting)")),
        shiny::div(class = "ms-auto d-flex gap-2",
          shiny::downloadButton(ns("csv_totals"), "Totals (CSV)", class = "btn-sm btn-outline-primary"),
          shiny::downloadButton(ns("csv_years"), "Per year (CSV)", class = "btn-sm btn-outline-primary"))),
      bslib::card_body(
        shiny::uiOutput(ns("totals_note")),
        shiny::tableOutput(ns("totals_main")),
        shiny::h6("By limit: hours in a typical year (median), by month", class = "mt-2"),
        shiny::div(class = "small text-muted mb-1",
                   "Met: hours that limit is met on its own. Only one failing: hours that miss the prescription only because of that limit, ",
                   "so the larger it is, the more that limit is holding the window shut."),
        shiny::div(class = "table-responsive tbl-limits", shiny::tableOutput(ns("totals_limit"))))))
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
    flagged <- shiny::reactive(rx_flag(base(), rx()))   # shared by the heatmap and the totals
    summ <- shiny::reactive({
      r <- rx(); fl <- flagged()
      vars <- rx_constrained(r)
      vals <- c("in_rx", paste0("pass_", vars), intersect(vars, c("temperature", "relativeHumidity", "windSpeed",
                                                                 "precipitation", "solarRadiation")))
      list(long = clim_summarise(fl, vals, by = c("month", hour_col())), vars = vars, n = nrow(fl),
           years = range(fl$year))
    })

    # ---- hour totals (counts; independent of the hour toggle) --------------------------
    tot <- shiny::reactive({
      r <- rx(); fl <- flagged(); vars <- rx_constrained(r)
      fl <- fl[rx_in_ignition(fl, rx_ignition(r)), ]       # counts use only the ignition period
      py <- totals_per_year(fl, vars, "month")
      list(per_year = py, summary = totals_summary(py), vars = vars, rx = r, years = range(fl$year))
    })
    tot_disp <- shiny::reactive(totals_display(tot()$summary, tot()$vars))
    output$totals_note <- shiny::renderUI({
      shiny::validate(shiny::need(st(), "Load a station to see hour totals."))
      t <- tot()
      shiny::div(class = "small text-muted mb-2",
                 sprintf("Years %d-%d. Hours counted: %s. Typical year = the median year; the range names the lowest and highest years. ",
                         t$years[1], t$years[2], rx_ignition_text(rx_ignition(t$rx))),
                 if (tot_disp()$any_left_out)
                   sprintf("* Some years are left out of the median and range because under %d%% of that month's hours were usable (they still count in the total; see the per-year CSV).",
                           TOTALS_COVERAGE_MIN))
    })
    output$totals_main <- shiny::renderTable({
      shiny::req(st()); tot_disp()$main
    }, striped = TRUE, spacing = "xs", align = "lrcrcr")
    output$totals_limit <- shiny::renderTable({
      shiny::req(st())
      d <- tot_disp()$by_limit
      shiny::validate(shiny::need(nrow(d) > 0, "No limits are on, so every hour counts."))
      d
    }, striped = TRUE, spacing = "xs", align = paste0("ll", strrep("r", 13)))
    csv_out <- function(what) shiny::downloadHandler(
      filename = function() totals_csv_name(st()$meta, what),
      content = function(file) {
        t <- tot()
        lines <- totals_csv_lines(if (what == "summary") t$summary else t$per_year, st()$meta, st()$source$detail,
                                  t$rx, t$years, paste0("local standard time (LST); counted within the ignition period, ",
                                                        rx_ignition_text(rx_ignition(t$rx))), what)
        con <- file(file, open = "wb"); on.exit(close(con))
        writeBin(charToRaw("\xef\xbb\xbf"), con)       # UTF-8 byte-order mark, so Excel shows degree signs
        writeLines(enc2utf8(lines), con, sep = "\r\n", useBytes = TRUE)
      })
    output$csv_totals <- csv_out("summary")
    output$csv_years <- csv_out("per_year")

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

    # month x hour cells outside the ignition period (clock time): washed out on the heatmap.
    # In LST at a daylight-saving station the period sits an hour earlier in summer months.
    outside_cells <- shiny::reactive({
      ig <- rx_ignition(rx()); if (is.null(ig)) return(NULL)
      g <- expand.grid(month = 1:12, h = 0:23)
      b <- base()
      shift <- tapply((b$hour_clock - b$hour) %% 24, b$month, stats::median)
      clock_h <- if (hour_col() == "hour_clock") g$h else (g$h + shift[as.character(g$month)]) %% 24
      out <- g[!(clock_h >= ig[1] & clock_h < ig[2]), ]
      stats::setNames(out, c("month", hour_col()))
    })
    output$heat <- plotly::renderPlotly({
      shiny::validate(shiny::need(st(), "Load a station (sidebar or Station tab) to see its climatology."))
      s <- summ()
      cells <- tip_cells(s$long, x_col = "month", hour_col = hour_col(), rx_vars = s$vars,
                         obs_minute = as.integer(names(which.max(table(st()$hourly$obs_minute)))),
                         threshold = thr())
      ir <- s$long[s$long$value == "in_rx", ]
      cells$stat <- ir$stat[match(paste(cells$month, cells[[hour_col()]]), paste(ir$month, ir[[hour_col()]]))]
      plot_hour_heatmap(cells, hour_col = hour_col(), clock = hour_col() == "hour_clock", threshold = thr(),
                        outside = outside_cells())
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
                                  paste0(if (is.na(thr())) "" else sprintf(" Outlined: cells where at least %d%% of hours are in prescription.", round(thr())),
                                         if (is.null(rx_ignition(rx()))) "" else
                                           sprintf(" Washed out: hours outside the ignition period (%s).", rx_ignition_text(rx_ignition(rx())))))),
                 shiny::p(sprintf("%s of %s hours (%s) are excluded because a limited variable is missing or was screened out; see Data quality.",
                                  format(excl, big.mark = ","), format(tot, big.mark = ","), tip_pct(excl / tot))))
    })
  })
}
