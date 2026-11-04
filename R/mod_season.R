# ==============================================================================
# R/mod_season.R -- Season tracker tab (the project doc's "Rx Burn Period")
#
# Hours in prescription per day for one year -- the current year with the
# latest FEMS observations and FEMS's hourly forecast, or any past year --
# against the 2005-2025 reference for each day (middle half, typical year,
# record). One panel for all limits together, then one per limit, sharing the
# date axis and zoom. Clicking a day opens its week on Past event.
#
# The live top-up (hourly_live) runs the first time the tab is shown for a
# station, and again from the Refresh button; it is cached for the session.
#
#   mod_season_server(id, st, rx, active, fetch = fems_download_weather, now = Sys.time)
#     st      station object reactive
#     rx      prescription reactive (with ignition / min window attributes)
#     active  reactive TRUE while the tab is showing (starts the live fetch)
#     fetch, now  replaceable for tests
#   returns list(st_full = reactive(station with live observations appended),
#                goto    = reactive(list(date, at) for the last click, or NULL))
# Requires R/constants.R, R/hourly.R, R/rx.R, R/climatology.R, R/totals.R,
# R/tooltips.R, R/plots.R.
# ==============================================================================

mod_season_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(shiny::uiOutput(ns("title"))),
      bslib::card_body(
        shiny::div(class = "d-flex flex-wrap align-items-end gap-3",
          shiny::div(style = "width: 130px;", shiny::selectInput(ns("year"), "Year", choices = NULL, width = "100%")),
          shiny::div(style = "width: 230px;",
                     shiny::dateRangeInput(ns("window"), "Season window", start = as.Date("2001-01-01"),
                                           end = as.Date("2001-12-31"), format = "d M", startview = "year", width = "100%")),
          shiny::div(class = "mb-3 d-flex gap-1",
            shiny::uiOutput(ns("recent_box"), inline = TRUE),
            shiny::actionButton(ns("whole"), "Whole year", class = "btn-sm btn-outline-secondary"),
            shiny::actionButton(ns("refresh"), "Refresh from FEMS", class = "btn-sm btn-outline-secondary"))),
        shiny::uiOutput(ns("status")),
        shiny::uiOutput(ns("plot_box")),
        shiny::uiOutput(ns("caption")))),
    bslib::card(
      bslib::card_header(class = "d-flex flex-wrap align-items-center gap-2",
        shiny::span("Season so far ", shiny::span(class = "text-muted fw-normal", "(counts, for reporting)")),
        shiny::div(class = "ms-auto", shiny::downloadButton(ns("csv_days"), "Daily (CSV)", class = "btn-sm btn-outline-primary"))),
      bslib::card_body(
        shiny::uiOutput(ns("totals_note")),
        shiny::div(class = "table-responsive", shiny::tableOutput(ns("totals"))))))
}

mod_season_server <- function(id, st, rx, active = shiny::reactive(TRUE),
                              fetch = function(...) fems_download_weather(...), now = Sys.time) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    live <- shiny::reactiveVal(list())     # station_id -> hourly_live() result, or an error message
    rendered <- shiny::reactiveVal(FALSE)  # TRUE once the chart has been drawn (see `click`)

    get_live <- function(s) {
      sid <- s$meta$station_id
      res <- shiny::withProgress(message = "Getting the latest observations and the FEMS forecast", value = 0.3, {
        tryCatch(hourly_live(s, fetch = fetch, now = now()),
                 fems_unavailable = function(e) conditionMessage(e),
                 error = function(e) paste("Couldn't get live data:", conditionMessage(e)))
      })
      if (is.character(res)) shiny::showNotification(res, type = "warning", duration = 8)
      l <- live(); l[[sid]] <- res; live(l)
    }
    shiny::observe({
      shiny::req(isTRUE(active()), st())
      if (is.null(shiny::isolate(live())[[st()$meta$station_id]])) get_live(shiny::isolate(st()))
    })
    shiny::observeEvent(input$refresh, { shiny::req(st()); get_live(st()) })

    lv <- shiny::reactive({ shiny::req(st()); x <- live()[[st()$meta$station_id]]; if (is.list(x)) x else NULL })
    st_full <- shiny::reactive({ shiny::req(st()); hourly_merge_live(st(), lv()) })
    today <- shiny::reactive(as.Date(format(now(), tz = st()$meta$tz)))

    # ---- year menu: every year in the record, newest first --------------------------------
    shiny::observe({
      yrs <- sort(unique(st_full()$hourly$year), decreasing = TRUE)
      cur <- shiny::isolate(input$year)
      sel <- if (!is.null(cur) && cur %in% yrs) cur else as.character(max(yrs))
      shiny::updateSelectInput(session, "year", choices = as.character(yrs), selected = sel)
    })
    shiny::observeEvent(input$whole, shiny::updateDateRangeInput(session, "window", start = as.Date("2001-01-01"),
                                                                 end = as.Date("2001-12-31")))

    year <- shiny::reactive({ shiny::req(input$year); as.integer(input$year) })

    # ---- zoom: the season window, or the last 7 days + the forecast (current year) -----------
    zoom <- shiny::reactiveVal(NULL)           # NULL = show the season window
    output$recent_box <- shiny::renderUI({
      shiny::req(st(), input$year)
      if (!is_current()) return(NULL)
      shiny::actionButton(ns("recent"), "Last 7 days + forecast", class = "btn-sm btn-outline-primary",
                          title = "Zoom to the past week and the FEMS forecast")
    })
    shiny::observeEvent(input$recent, {
      t <- today(); f <- if (!is.null(lv()) && nrow(lv()$fc)) max(lv()$fc$date) else t + SEASON_RECENT_DAYS
      zoom(c(t - SEASON_RECENT_DAYS, max(f, t)))
    })
    shiny::observeEvent(list(input$whole, input$window, input$year), zoom(NULL), ignoreInit = TRUE)
    xrange <- shiny::reactive({
      z <- zoom(); w <- win()
      if (is.null(z)) c(w$start - 0.5, w$end + 0.5) else c(z[1] - 0.5, z[2] + 0.5)
    })
    win <- shiny::reactive({
      d <- input$window
      shiny::validate(shiny::need(length(d) == 2 && !anyNA(d), "Choose the first and last day of the season window."))
      a <- format(d[1], "%m-%d"); b <- format(d[2], "%m-%d")
      if (a == "02-29") a <- "02-28"; if (b == "02-29") b <- "02-28"
      shiny::validate(shiny::need(a <= b, "The season window runs within one calendar year: choose a start before the end."))
      y <- year()
      list(start = as.Date(paste0(y, "-", a)), end = as.Date(paste0(y, "-", b)), label = paste(format(d[1], "%d %b"), "to", format(d[2], "%d %b")))
    })
    is_current <- shiny::reactive(year() == as.integer(format(today(), "%Y")))

    # ---- the year's days, and the reference --------------------------------------------------
    days <- shiny::reactive({
      r <- rx(); vars <- rx_constrained(r); ig <- rx_ignition(r)
      h <- st_full()$hourly; o <- h[h$year == year(), , drop = FALSE]
      shiny::validate(shiny::need(nrow(o) > 0, "No data for that year."))
      o$part <- "observed"
      f <- if (is_current() && !is.null(lv()) && nrow(lv()$fc)) { x <- lv()$fc; x$part <- "forecast"; x[, names(o)] } else o[0, ]
      fl <- rx_flag(rbind(o, f), r)
      fl <- fl[rx_in_ignition(fl, ig), ]
      d <- totals_season_days(fl, vars, min_run = rx_min_window(r))
      d$text <- tip_season_days(d, ref()$bands, vars, rx_min_window(r))
      d
    })
    ref <- shiny::reactive({
      r <- rx(); b <- rx_flag(hourly_baseline(st()$hourly), r)
      totals_season_reference(b[rx_in_ignition(b, rx_ignition(r)), ], rx_constrained(r), min_run = rx_min_window(r))
    })
    hours_per_day <- shiny::reactive({ ig <- rx_ignition(rx()); if (is.null(ig)) 24 else diff(ig) })

    output$title <- shiny::renderUI({
      if (is.null(st())) return("Season tracker")
      shiny::span("Hours in prescription each day at ", shiny::strong(st()$meta$name), " ",
                  shiny::span(class = "text-muted fw-normal", "this season against a typical year"))
    })
    output$status <- shiny::renderUI({
      shiny::req(st())
      x <- live()[[st()$meta$station_id]]
      msg <- if (is.null(x)) "Live data: not fetched yet."
             else if (is.character(x)) paste0("Live data unavailable (", x, "). Showing the stored record only.")
             else paste0("Latest observations and FEMS forecast fetched ", format(x$fetched_at, "%d %b %H:%M"),
                         if (!is.na(x$fc_start)) paste0("; forecast from ", x$fc_start, " for about 7 days") else "; no forecast returned",
                         ". FEMS gives no forecast issue time.")
      shiny::div(class = "small text-muted mb-1", msg)
    })
    output$plot_box <- shiny::renderUI({
      shiny::req(st())
      n <- 1 + length(rx_constrained(rx()))
      plotly::plotlyOutput(ns("plot"), height = paste0(120 + 260 + 150 * (n - 1), "px"))
    })
    output$plot <- plotly::renderPlotly({
      shiny::validate(shiny::need(st(), "Load a station (sidebar or Station tab) to track a season."))
      d <- days(); vars <- rx_constrained(rx()); w <- win()
      meas <- c("in_rx", paste0("pass_", vars))
      ttl <- c("All limits (h)", paste0(c(temperature = "Temperature", relativeHumidity = "RH", windSpeed = "Wind",
                                           windDirection = "Wind dir", precipitation = "Precip", solarRadiation = "Solar")[vars], " met (h)"))
      p <- plot_season(d, ref()$bands, meas, ttl, xrange = xrange(),
                       today = if (is_current()) today() else NULL, ymax = hours_per_day(), year = year())
      if (!shiny::isolate(rendered())) rendered(TRUE)
      p
    })
    output$caption <- shiny::renderUI({
      shiny::req(st())
      ig <- rx_ignition(rx())
      shiny::div(class = "small text-muted mt-2",
                 sprintf("Bars: hours in prescription each day (%s); hatched: FEMS forecast. Grey band: the middle half of %d-%d for that date (\u00b1%d days), dark line the typical year, dotted line the record. Orange triangles: usable days (a run of %d+ h). Drag to zoom, double-click to reset, click a day to open its week on Past event.",
                         rx_ignition_text(ig), min(RX_BASELINE_YEARS), max(RX_BASELINE_YEARS), CLIM_SMOOTH_DAYS, rx_min_window(rx())))
    })

    # ---- season so far ---------------------------------------------------------------------
    summ <- shiny::reactive({
      w <- win(); asof <- if (is_current()) today() else w$end
      totals_season_summary(days(), ref(), rx_constrained(rx()), w$start, w$end, asof, min_run = rx_min_window(rx()))
    })
    output$totals_note <- shiny::renderUI({
      s <- summ(); w <- win()
      shiny::div(class = "small text-muted mb-2",
                 sprintf("%s %d, %s through %s. Typical year = the median of %d-%d over the same dates; lowest and highest name their years. Hours counted: %s.",
                         w$label, year(), "observed", format(attr(s, "through"), "%d %b"),
                         min(RX_BASELINE_YEARS), max(RX_BASELINE_YEARS), rx_ignition_text(rx_ignition(rx()))),
                 if (is_current()) " The forecast column is the FEMS forecast for the coming days, kept apart from what has happened.")
    })
    output$totals <- shiny::renderTable(totals_season_display(summ()), striped = TRUE, spacing = "xs", align = "lrrrrrr")
    output$csv_days <- shiny::downloadHandler(
      filename = function() totals_csv_name(st()$meta, "daily", tab = paste0("season-", year())),
      content = function(file) {
        x <- lv()
        lines <- totals_season_csv_lines(days()[, setdiff(names(days()), "text")], ref()$bands, st()$meta,
                                         paste(c(st()$source$detail, st_full()$source$live), collapse = "; "), rx(), year(),
                                         paste0("local standard time (LST); counted within the ignition period, ", rx_ignition_text(rx_ignition(rx()))),
                                         if (is_current() && !is.null(x) && !is.na(x$fc_start))
                                           paste0("FEMS forecast fetched ", format(x$fetched_at, "%Y-%m-%d %H:%M"), ", starting ", x$fc_start) else NA)
        con <- file(file, open = "wb"); on.exit(close(con))
        writeBin(charToRaw("\xef\xbb\xbf"), con)       # UTF-8 byte-order mark, so Excel shows degree signs
        writeLines(enc2utf8(lines), con, sep = "\r\n", useBytes = TRUE)
      })

    # ---- a click opens that week on Past event ---------------------------------------------
    goto <- shiny::reactiveVal(NULL)
    # listen for clicks only once the chart exists: plotly warns about an
    # unregistered source if event_data() runs before the plot is drawn
    click <- shiny::reactive({ shiny::req(rendered()); plotly::event_data("plotly_click", source = "season") })
    shiny::observeEvent(click(), {
      e <- click()
      d <- tryCatch(as.Date(substr(as.character(e$x[1]), 1, 10)), error = function(err) NA)
      if (!is.na(d)) goto(list(date = d, at = Sys.time()))     # `at`: a second click on the same day still counts
    })

    list(st_full = st_full, goto = goto)
  })
}
