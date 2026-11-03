# ==============================================================================
# R/mod_same.R -- Same dates tab
#
# One window of the year (e.g. 15 Mar to 15 Apr, up to SAME_MAX_DAYS days, may
# wrap through New Year) shown for every year in the station's record: a
# year x day heatmap of hours in prescription with usable days outlined, then
# totals by year (R/totals.R) and two CSVs (per year, daily).
# Takes reactives st(), rx(), and event_dates() (the Past event range, for
# the "Use the Past event dates" button).
# Requires R/constants.R, R/hourly.R, R/rx.R, R/climatology.R, R/totals.R,
# R/tooltips.R, R/plots.R.
# ==============================================================================

mod_same_ui <- function(id) {
  ns <- shiny::NS(id)
  d0 <- as.Date(paste0("2001-", SAME_DEFAULT))
  shiny::tagList(
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(shiny::uiOutput(ns("title"))),
      bslib::card_body(
        shiny::div(class = "d-flex flex-wrap align-items-end gap-3",
          shiny::div(style = "width: 230px;",
                     shiny::dateRangeInput(ns("window"), paste0("Window of the year (up to ", SAME_MAX_DAYS, " days)"),
                                           start = d0[1], end = d0[2], format = "d M", startview = "year", width = "100%")),
          shiny::div(class = "mb-3", shiny::actionButton(ns("use_event"), "Use the Past event dates",
                                                          class = "btn-sm btn-outline-secondary")),
          shiny::div(style = "width: 250px;",
                     shiny::sliderInput(ns("min_run"), "A usable day has a run of at least", min = 1, max = 12,
                                        value = SAME_MIN_RUN_H, step = 1, post = " h", ticks = FALSE, width = "100%")),
          shiny::div(class = "mb-3", shiny::uiOutput(ns("controls")))),
        shiny::uiOutput(ns("plot_box")),
        shiny::uiOutput(ns("caption")))),
    bslib::card(
      bslib::card_header(class = "d-flex flex-wrap align-items-center gap-2",
        shiny::span("Hours in prescription, by year ", shiny::span(class = "text-muted fw-normal", "(counts, for reporting)")),
        shiny::div(class = "ms-auto d-flex gap-2",
          shiny::downloadButton(ns("csv_years"), "Per year (CSV)", class = "btn-sm btn-outline-primary"),
          shiny::downloadButton(ns("csv_days"), "Daily (CSV)", class = "btn-sm btn-outline-primary"))),
      bslib::card_body(
        shiny::uiOutput(ns("totals_note")),
        shiny::tableOutput(ns("totals_main")),
        shiny::h6("By limit: hours in the window", class = "mt-2"),
        shiny::div(class = "small text-muted mb-1",
                   "Met: hours that limit is met on its own. Only one failing: hours that miss the prescription only because of that limit."),
        shiny::div(class = "table-responsive tbl-limits", shiny::tableOutput(ns("totals_limit"))))))
}

mod_same_server <- function(id, st, rx, event_dates = shiny::reactive(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    clock <- shiny::reactive(isTRUE(input$clock) && isTRUE(st()$meta$has_dst))
    min_run <- shiny::debounce(shiny::reactive(if (is.null(input$min_run)) SAME_MIN_RUN_H else input$min_run), RX_DEBOUNCE_MS)

    shiny::observeEvent(input$use_event, {
      d <- event_dates()
      if (length(d) != 2 || anyNA(d)) {
        shiny::showNotification("Choose dates on the Past event tab first.", type = "warning"); return()
      }
      shiny::updateDateRangeInput(session, "window", start = d[1], end = d[2])
    })

    win <- shiny::reactive({
      shiny::validate(shiny::need(st(), "Load a station (sidebar or Station tab) to compare the same dates across years."))
      d <- input$window
      shiny::validate(shiny::need(length(d) == 2 && !anyNA(d), "Choose the first and last day of the window."))
      # only month and day count; 29 Feb is read as 28 Feb so every year has the day
      fix <- function(x) if (format(x, "%m-%d") == "02-29") x - 1 else x
      d <- c(fix(d[1]), fix(d[2]))
      ref <- as.Date(format(d, "2001-%m-%d"))
      n <- as.numeric(ref[2] - ref[1]) + 1; if (n <= 0) n <- n + 365
      shiny::validate(shiny::need(n <= SAME_MAX_DAYS, sprintf("Choose up to %d days (that's %d).", SAME_MAX_DAYS, n)))
      list(start = d[1], end = d[2], n = n,
           label = paste(format(d[1], "%d %b"), "to", format(d[2], "%d %b")))
    })
    res <- shiny::reactive({
      w <- win(); r <- rx()
      hw <- hourly_window(st()$hourly, w$start, w$end)
      shiny::validate(shiny::need(nrow(hw) > 0, "The station has no data in that window."))
      fl <- rx_flag(hw, r); vars <- rx_constrained(r)
      yrs <- sort(unique(fl$wyear))
      nd <- stats::setNames(hourly_window_ndays(yrs, w$start, w$end), yrs)
      tw <- totals_window(fl, vars, nd, min_run = min_run(), hour_col = if (clock()) "hour_clock" else "hour")
      tw$days$text <- tip_window_days(tw$days, min_run(), clock = clock())
      list(tw = tw, vars = vars, rx = r, labels = attr(hw, "window_days"), years = range(yrs), w = w)
    })

    output$title <- shiny::renderUI({
      if (is.null(st())) return("Same dates, many years")
      shiny::span("The same dates every year at ", shiny::strong(st()$meta$name), " ",
                  shiny::span(class = "text-muted fw-normal", "hours in prescription each day"))
    })
    output$controls <- shiny::renderUI({
      shiny::req(st())
      if (!isTRUE(st()$meta$has_dst)) return(NULL)
      shiny::checkboxInput(ns("clock"), "Clock time for run start times", value = isTRUE(shiny::isolate(input$clock)))
    })
    output$plot_box <- shiny::renderUI({
      r <- res(); ny <- diff(r$years) + 1
      plotly::plotlyOutput(ns("plot"), height = paste0(max(260, 60 + 22 * ny), "px"))
    })
    output$plot <- plotly::renderPlotly(plot_year_day(res()$tw$days, res()$labels))
    output$caption <- shiny::renderUI({
      r <- res(); s <- r$tw$summary
      ir <- s[s$value == "in_rx", ]; ud <- s[s$value == "usable_days", ]
      shiny::div(class = "small text-muted mt-2",
                 sprintf("%s, %d-%d. Each cell is one day; its colour is the hours in prescription that day. Outlined: usable days (a run of %d+ consecutive hours). In a typical year the window has %s hours in prescription and %s usable days.",
                         r$w$label, r$years[1], r$years[2], min_run(), .totals_h(ir$median), .totals_h(ud$median)))
    })

    disp <- shiny::reactive(totals_window_display(res()$tw, res()$vars, min_run()))
    output$totals_note <- shiny::renderUI({
      d <- disp()
      shiny::div(class = "small text-muted mb-2",
                 "Typical year = the median year; lowest and highest name their years.",
                 if (d$any_left_out) sprintf(" * Left out of the typical year, lowest and highest: under %d%% of the window's hours were usable (missing data, or outside the record).",
                                             TOTALS_COVERAGE_MIN))
    })
    output$totals_main <- shiny::renderTable(disp()$main, striped = TRUE, spacing = "xs", align = "lrrrrr")
    output$totals_limit <- shiny::renderTable({
      d <- disp()$by_limit
      shiny::validate(shiny::need(nrow(d) > 0, "No limits are on, so every hour counts."))
      d
    }, striped = TRUE, spacing = "xs", align = "r")

    csv_out <- function(what) shiny::downloadHandler(
      filename = function() totals_csv_name(st()$meta, what, tab = "same-dates"),
      content = function(file) {
        r <- res()
        lines <- totals_window_csv_lines(r$tw, st()$meta, st()$source$detail, r$rx, r$w$label, min_run(),
                                         if (clock()) "run starts in clock time" else "local standard time (LST)", what)
        con <- file(file, open = "wb"); on.exit(close(con))
        writeBin(charToRaw("\xef\xbb\xbf"), con)       # UTF-8 byte-order mark, so Excel shows degree signs
        writeLines(enc2utf8(lines), con, sep = "\r\n", useBytes = TRUE)
      })
    output$csv_years <- csv_out("per_year")
    output$csv_days <- csv_out("daily")
  })
}
