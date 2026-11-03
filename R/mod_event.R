# ==============================================================================
# R/mod_event.R -- Past event tab
#
# Hour by hour for a stretch of up to EVENT_MAX_DAYS days from the station's
# whole record: a strip of in / out / not-known hours, one panel per variable
# with the prescription range shaded and the failing limit marked, night
# shaded. Below: hour totals by day (R/totals.R) and two CSVs (daily, hourly).
# Takes reactives st() (station object) and rx() (prescription). Returns
# list(dates) -- the chosen range, a reactive.
# Requires R/constants.R, R/sun.R, R/rx.R, R/climatology.R, R/totals.R,
# R/tooltips.R, R/plots.R.
# ==============================================================================

.EVENT_ALWAYS <- c("temperature", "relativeHumidity", "windSpeed", "windDirection")

mod_event_ui <- function(id) {
  ns <- shiny::NS(id)
  key <- function(col, label, border = FALSE)
    shiny::span(class = "me-3 text-nowrap",
                shiny::span(style = sprintf("display:inline-block;width:.8rem;height:.8rem;vertical-align:-1px;margin-right:.3rem;background:%s;%s",
                                            col, if (border) "border:1px solid #c3ccd3;" else "")),
                label)
  shiny::tagList(
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(shiny::uiOutput(ns("title"))),
      bslib::card_body(
        shiny::div(class = "d-flex flex-wrap align-items-end gap-2",
          shiny::div(style = "width: 270px;",
                     shiny::dateRangeInput(ns("dates"), paste0("Dates (up to ", EVENT_MAX_DAYS, " days)"),
                                           start = Sys.Date() - EVENT_DEFAULT_DAYS + 1, end = Sys.Date(), width = "100%")),
          shiny::div(class = "mb-3 d-flex gap-1",
            shiny::actionButton(ns("back"), "\u25c0 Earlier", class = "btn-sm btn-outline-secondary"),
            shiny::actionButton(ns("fwd"), "Later \u25b6", class = "btn-sm btn-outline-secondary")),
          shiny::div(class = "mb-3 ms-2", shiny::uiOutput(ns("controls")))),
        shiny::div(class = "small text-muted mb-1",
                   key(CHART_IN_RX, "in prescription"), key(CHART_OUT, "out"), key(CHART_FAIL, "this limit failed"),
                   key("#ffffff", "not known (missing data)", border = TRUE), key("rgba(25,45,70,0.18)", "night"),
                   key("rgba(11,94,128,0.25)", "prescription range")),
        shiny::uiOutput(ns("plot_box")),
        shiny::uiOutput(ns("caption")))),
    bslib::card(
      bslib::card_header(class = "d-flex flex-wrap align-items-center gap-2",
        shiny::span("Hours in prescription, by day ", shiny::span(class = "text-muted fw-normal", "(counts, for reporting)")),
        shiny::div(class = "ms-auto d-flex gap-2",
          shiny::downloadButton(ns("csv_daily"), "Daily (CSV)", class = "btn-sm btn-outline-primary"),
          shiny::downloadButton(ns("csv_hourly"), "Hourly (CSV)", class = "btn-sm btn-outline-primary"))),
      bslib::card_body(
        shiny::tableOutput(ns("totals_main")),
        shiny::h6("By limit: hours per day", class = "mt-2"),
        shiny::div(class = "small text-muted mb-1",
                   "Met: hours that limit is met on its own. Only one failing: hours that miss the prescription only because of that limit."),
        shiny::div(class = "table-responsive tbl-limits", shiny::tableOutput(ns("totals_limit"))))))
}

mod_event_server <- function(id, st, rx) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    clock <- shiny::reactive(isTRUE(input$clock) && isTRUE(st()$meta$has_dst))
    record <- shiny::reactive({ shiny::req(st()); range(st()$hourly$date) })

    # a new station: the last EVENT_DEFAULT_DAYS days of its record
    shiny::observeEvent(st(), {
      r <- record()
      shiny::updateDateRangeInput(session, "dates", start = max(r[1], r[2] - EVENT_DEFAULT_DAYS + 1), end = r[2],
                                  min = r[1], max = r[2])
    })
    shift <- function(sign) {
      d <- input$dates; r <- record(); shiny::req(length(d) == 2, !anyNA(d))
      len <- as.numeric(d[2] - d[1]) + 1
      s <- d[1] + sign * len; s <- min(max(s, r[1]), r[2] - len + 1); s <- max(s, r[1])
      shiny::updateDateRangeInput(session, "dates", start = s, end = min(s + len - 1, r[2]))
    }
    shiny::observeEvent(input$back, shift(-1))
    shiny::observeEvent(input$fwd, shift(1))

    dates <- shiny::reactive({
      shiny::validate(shiny::need(st(), "Load a station (sidebar or Station tab) to look at a past event."))
      d <- input$dates; r <- record()
      shiny::validate(shiny::need(length(d) == 2 && !anyNA(d), "Choose a start and an end date."))
      shiny::validate(shiny::need(d[1] <= d[2], "The start date is after the end date."))
      shiny::validate(shiny::need(as.numeric(d[2] - d[1]) + 1 <= EVENT_MAX_DAYS,
                                  sprintf("Choose up to %d days (that's %d).", EVENT_MAX_DAYS, as.numeric(d[2] - d[1]) + 1)))
      shiny::validate(shiny::need(d[1] <= r[2] && d[2] >= r[1],
                                  sprintf("This station's record runs %s to %s.", format(r[1], "%d %b %Y"), format(r[2], "%d %b %Y"))))
      c(max(d[1], r[1]), min(d[2], r[2]))
    })
    ev <- shiny::reactive({
      d <- dates(); r <- rx()
      h <- st()$hourly; h <- h[h$date >= d[1] & h$date <= d[2], ]
      fl <- rx_flag(h[order(h$date, h$hour), ], r)
      vars <- rx_constrained(r)
      list(fl = fl, rx = r, vars = vars, dates = d,
           show = c(.EVENT_ALWAYS, intersect(c("precipitation", "solarRadiation"), vars)),
           totals = totals_event(fl, vars, hour_col = if (clock()) "hour_clock" else "hour"))
    })

    output$title <- shiny::renderUI({
      if (is.null(st())) return("Past event")
      shiny::span("Hour by hour at ", shiny::strong(st()$meta$name), " ",
                  shiny::span(class = "text-muted fw-normal", "when was it in prescription, and which limit was holding it out?"))
    })
    output$controls <- shiny::renderUI({
      shiny::req(st())
      if (!isTRUE(st()$meta$has_dst)) return(NULL)
      shiny::checkboxInput(ns("clock"), "Clock time (daylight saving)", value = isTRUE(shiny::isolate(input$clock)))
    })
    output$plot_box <- shiny::renderUI({
      e <- ev()
      plotly::plotlyOutput(ns("plot"), height = paste0(70 + 125 * length(e$show) + 50, "px"))
    })
    output$plot <- plotly::renderPlotly({
      e <- ev(); m <- st()$meta
      om <- as.integer(names(which.max(table(st()$hourly$obs_minute))))
      txt <- tip_event_hours(e$fl, e$rx, clock = clock(), obs_minute = om)
      sh <- if (clock()) as.numeric(stats::median((e$fl$hour_clock - e$fl$hour) %% 24)) else 0
      plot_event(e$fl, e$rx, txt, sun_nights(m$lat, m$lon, m$std_offset_h, e$dates), e$show,
                 clock = clock(), clock_shift = sh)
    })
    output$caption <- shiny::renderUI({
      e <- ev(); ir <- e$totals[e$totals$value == "in_rx" & e$totals$day == "Total", ]
      shiny::div(class = "small text-muted mt-2",
                 sprintf("%s to %s: %s of %s hours with data in prescription (%s); %s not known. Hover any hour for its values and the limit that failed. Drag across the chart to zoom; double-click to reset.",
                         format(e$dates[1], "%d %b %Y"), format(e$dates[2], "%d %b %Y"),
                         format(ir$n_true, big.mark = ","), format(ir$n_valid, big.mark = ","),
                         tip_pct(if (ir$n_valid) ir$n_true / ir$n_valid else NA), tip_hours(ir$n_missing)))
    })

    disp <- shiny::reactive(totals_event_display(ev()$totals, ev()$vars, clock = clock()))
    output$totals_main <- shiny::renderTable(disp()$main, striped = TRUE, spacing = "xs", align = "lrlrr")
    output$totals_limit <- shiny::renderTable({
      d <- disp()$by_limit
      shiny::validate(shiny::need(nrow(d) > 0, "No limits are on, so every hour counts."))
      d
    }, striped = TRUE, spacing = "xs", align = "r")   # the day count varies, so one alignment for every column

    csv_out <- function(what) shiny::downloadHandler(
      filename = function() totals_csv_name(st()$meta, what, tab = "past-event"),
      content = function(file) {
        e <- ev()
        lines <- totals_event_csv_lines(if (what == "daily") e$totals else e$fl, st()$meta, st()$source$detail,
                                        e$rx, e$dates, if (clock()) "clock time (run starts) and LST" else "local standard time (LST)", what)
        con <- file(file, open = "wb"); on.exit(close(con))
        writeBin(charToRaw("\xef\xbb\xbf"), con)       # UTF-8 byte-order mark, so Excel shows degree signs
        writeLines(enc2utf8(lines), con, sep = "\r\n", useBytes = TRUE)
      })
    output$csv_daily <- csv_out("daily")
    output$csv_hourly <- csv_out("hourly")

    # the chosen range, for the Same dates tab's "Use the Past event dates"
    list(dates = shiny::reactive(input$dates))
  })
}
