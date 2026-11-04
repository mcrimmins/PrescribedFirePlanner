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
      bslib::navset_underline(id = ns("view"),
        bslib::nav_panel("Day of year", value = "doy", plotly::plotlyOutput(ns("doy"), height = "600px")),
        bslib::nav_panel("Month \u00d7 hour", value = "month", plotly::plotlyOutput(ns("heat"), height = "560px")),
        bslib::nav_panel("By variable", value = "byvar",
                         shiny::div(class = "small text-muted my-2",
                                    "Each limit on its own, by hour and day of year (\u00b17 days): the share of hours that pass that one limit, ",
                                    "on the same colours as the main chart. Where a panel is pale, that limit is closing the window. ",
                                    "The black line is your outline threshold; hover for the mean value."),
                         shiny::uiOutput(ns("byvar_box"))),
        bslib::nav_panel("Daily ranges", value = "ranges",
                         shiny::uiOutput(ns("ranges_note")),
                         shiny::uiOutput(ns("ranges_box"))),
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
                                                                 "gustSpeed", "precipitation", "solarRadiation")))
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
                  shiny::span(class = "text-muted fw-normal", "% of hours, by time of year and hour of day"))
    })
    output$controls <- shiny::renderUI({
      shiny::req(st())
      if (!isTRUE(st()$meta$has_dst)) return(NULL)
      shiny::checkboxInput(ns("clock"), "Clock time (daylight saving) instead of LST",
                           value = isTRUE(shiny::isolate(input$clock)))
    })

    # ---- hour x day-of-year chart (smoothed +/- CLIM_SMOOTH_DAYS days) ------------------------
    shift_doy <- shiny::reactive({          # clock minus LST hours by day of year (0 or 1)
      b <- base(); s <- tapply((b$hour_clock - b$hour) %% 24, b$doy, stats::median)
      out <- numeric(CLIM_DOY_N); out[as.integer(names(s))] <- s; out
    })
    summ_doy <- shiny::reactive({
      r <- rx(); fl <- flagged(); vars <- rx_constrained(r)
      clim_summarise(fl, c("in_rx", paste0("pass_", vars), intersect(vars, c("temperature", "relativeHumidity",
                                                                               "windSpeed", "gustSpeed", "solarRadiation"))),
                     by = c("doy", hour_col()), smooth_days = CLIM_SMOOTH_DAYS)
    })
    sun_doy <- shiny::reactive({
      m <- st()$meta
      s <- sun_times(m$lat, m$lon, m$std_offset_h, seq_len(CLIM_DOY_N))
      if (hour_col() == "hour_clock") s[, -1] <- s[, -1] + shift_doy()
      s
    })
    output$doy <- plotly::renderPlotly({
      shiny::validate(shiny::need(st(), "Load a station (sidebar or Station tab) to see its climatology."))
      s <- summ_doy(); sun <- sun_doy(); vars <- rx_constrained(rx())
      s <- s[s$doy %in% seq(1, CLIM_DOY_N, by = CLIM_DOY_STEP), ]     # every 3rd day (already smoothed over 15)
      cells <- tip_cells(s, x_col = "doy", hour_col = hour_col(), rx_vars = vars,
                         obs_minute = as.integer(names(which.max(table(st()$hourly$obs_minute)))),
                         threshold = thr(), sun = sun)
      ir <- s[s$value == "in_rx", ]
      cells$stat <- ir$stat[match(paste(cells$doy, cells[[hour_col()]]), paste(ir$doy, ir[[hour_col()]]))]
      ig <- rx_ignition(rx())
      ign <- if (is.null(ig)) NULL else {
        sh <- if (hour_col() == "hour_clock") 0 else shift_doy()
        data.frame(doy = seq_len(CLIM_DOY_N), start = ig[1] - sh, end = ig[2] - sh)
      }
      plot_doy_heatmap(cells, hour_col = hour_col(), clock = hour_col() == "hour_clock", sun = sun, ign = ign,
                       threshold = thr(), split = split())
    })

    # ---- by variable: one panel per limit ----------------------------------------------------
    lim_of <- function(v) { r <- rx(); i <- which(r$var == v); c(r$min[i], r$max[i]) }
    var_title <- function(v) {
      m <- RX_VARS[RX_VARS$var == v, ]; l <- lim_of(v)
      shiny::h6(class = "mt-3 mb-1", m$label, shiny::span(class = "text-muted fw-normal",
                paste0(" \u2014 your limit: ", tip_limit_text(v, l[1], l[2]))))
    }
    output$byvar_box <- shiny::renderUI({
      shiny::validate(shiny::need(st(), "Load a station first."))
      vars <- rx_constrained(rx())
      shiny::validate(shiny::need(length(vars) > 0, "Turn on at least one limit in the sidebar."))
      shiny::tagList(lapply(vars, function(v) shiny::tagList(var_title(v), plotly::plotlyOutput(ns(paste0("byvar_", v)), height = "260px"))))
    })
    for (v0 in RX_VARS$var) local({
      v <- v0
      output[[paste0("byvar_", v)]] <- plotly::renderPlotly({
        shiny::req(v %in% rx_constrained(rx()))
        s <- summ_doy(); s <- s[s$doy %in% seq(1, CLIM_DOY_N, by = CLIM_DOY_STEP), ]
        l <- lim_of(v)
        cells <- tip_var_cells(s, v, hour_col = hour_col(), lo = l[1], hi = l[2], sun = sun_doy())
        plot_var_doy(cells, v, hour_col = hour_col(), sun = sun_doy(), clock = hour_col() == "hour_clock", threshold = thr(),
                     split = split())
      })
    })

    # ---- daily ranges: percentiles of the hours that count (ignition period, else all day) ----
    range_vars <- shiny::reactive(intersect(rx_constrained(rx()), c("temperature", "relativeHumidity", "windSpeed", "gustSpeed", "solarRadiation")))
    ranges <- shiny::reactive({
      r <- rx(); fl <- flagged(); fl <- fl[rx_in_ignition(fl, rx_ignition(r)), ]
      vars <- range_vars(); shiny::req(length(vars))
      list(q = clim_quantiles(fl, vars),
           pass = clim_summarise(fl, paste0("pass_", vars), by = "doy", smooth_days = CLIM_SMOOTH_DAYS),
           hours = if (is.null(rx_ignition(r))) "all hours" else paste("hours", rx_ignition_text(rx_ignition(r))))
    })
    output$ranges_note <- shiny::renderUI({
      shiny::req(st())
      ig <- rx_ignition(rx())
      shiny::div(class = "small text-muted my-2",
                 "The spread of hourly values through the year (\u00b17 days, 2005-2025): the line is the median, the dark band the middle half ",
                 "of hours, the light band 8 in 10 hours; your limits are shaded. ",
                 if (is.null(ig)) "All 24 hours count; set an ignition period in the sidebar to see just the burning hours."
                 else paste0("Only hours in the ignition period count (", rx_ignition_text(ig), ")."))
    })
    output$ranges_box <- shiny::renderUI({
      shiny::validate(shiny::need(st(), "Load a station first."))
      vars <- range_vars()
      shiny::validate(shiny::need(length(vars) > 0, "Turn on a temperature, RH, wind speed, gust or solar radiation limit to see its daily range."))
      shiny::tagList(lapply(vars, function(v) shiny::tagList(var_title(v), plotly::plotlyOutput(ns(paste0("range_", v)), height = "240px"))))
    })
    for (v0 in c("temperature", "relativeHumidity", "windSpeed", "gustSpeed", "solarRadiation")) local({
      v <- v0
      output[[paste0("range_", v)]] <- plotly::renderPlotly({
        shiny::req(v %in% range_vars())
        rg <- ranges(); q <- rg$q[rg$q$value == v, ]
        ps <- rg$pass[rg$pass$value == paste0("pass_", v), ]
        l <- lim_of(v)
        plot_daily_range(q, v, l[1], l[2], text = tip_daily_range(q, v, ps$stat[match(q$doy, ps$doy)], rg$hours))
      })
    })

    # the ignition period (clock time) on the month x hour heatmap: start and end hour per month,
    # in the chart's hours. In LST at a daylight-saving station it sits an hour earlier in summer.
    ign_month <- shiny::reactive({
      ig <- rx_ignition(rx()); if (is.null(ig)) return(NULL)
      sh <- if (hour_col() == "hour_clock") rep(0, 12) else {
        b <- base(); x <- tapply((b$hour_clock - b$hour) %% 24, b$month, stats::median); as.numeric(x[as.character(1:12)])
      }
      data.frame(month = 1:12, start = ig[1] - sh, end = ig[2] - sh)
    })
    # the colour split: the outline threshold (the slider value, even with the outline off)
    split <- shiny::debounce(shiny::reactive(if (is.null(input$outline_pct)) CLIM_HIGHLIGHT_PCT else input$outline_pct), RX_DEBOUNCE_MS)
    output$heat <- plotly::renderPlotly({
      shiny::validate(shiny::need(st(), "Load a station (sidebar or Station tab) to see its climatology."))
      s <- summ()
      cells <- tip_cells(s$long, x_col = "month", hour_col = hour_col(), rx_vars = s$vars,
                         obs_minute = as.integer(names(which.max(table(st()$hourly$obs_minute)))),
                         threshold = thr())
      ir <- s$long[s$long$value == "in_rx", ]
      cells$stat <- ir$stat[match(paste(cells$month, cells[[hour_col()]]), paste(ir$month, ir[[hour_col()]]))]
      plot_hour_heatmap(cells, hour_col = hour_col(), clock = hour_col() == "hour_clock", threshold = thr(),
                        split = split(), ign = ign_month())
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
                 shiny::p(sprintf("Years %d-%d. Day of year: each cell pools that hour over a 15-day window (7 days either side) across those years; lines show sunrise, sunset and civil twilight%s. Month x hour: each cell pools the whole month. Limits on: %s.%s",
                                  s$years[1], s$years[2], if (is.null(rx_ignition(rx()))) "" else ", and the dashed lines the ignition period", lim,
                                  paste0(sprintf(" Colours: warm below %d%% of hours, blue at or above it, darker as the share rises, in 10%% steps.", round(split())),
                                         if (is.na(thr())) "" else " The black line (a contour by day of year, outlined cells by month) marks the threshold.",
                                         if (is.null(rx_ignition(rx()))) "" else
                                           sprintf(" Dashed lines: the ignition period (%s).", rx_ignition_text(rx_ignition(rx())))))),
                 shiny::p(sprintf("%s of %s hours (%s) are excluded because a limited variable is missing or was screened out; see Data quality.",
                                  format(excl, big.mark = ","), format(tot, big.mark = ","), tip_pct(excl / tot))))
    })
  })
}
