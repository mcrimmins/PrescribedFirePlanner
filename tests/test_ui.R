# ==============================================================================
# tests/test_ui.R -- the app's UI and modules, without a browser
#
#   A  packages and sourcing (in app.R's order)
#   B  the UI: evaluate only `ui <- ...` from app.R (carryover sec. 3), walk the
#      shiny.tag tree and check every input/output id the server relies on
#   C  mod_rx_server with shiny::testServer: example values, typed edits,
#      an invalid entry keeps the last valid prescription
#   D  mod_climatology_server with a real bundled station
#
#     source("tests/test_ui.R")
# ==============================================================================

local({
  source("tests/helpers.R", local = TRUE)
  .T <- .test_open("test_ui")

  tryCatch({
    .env_header("App UI and modules (no network, no browser)")
    .section("A. Packages and sources")
    pk <- c("shiny", "bslib", "leaflet", "plotly")
    have <- vapply(pk, requireNamespace, logical(1), quietly = TRUE)
    .check("A1 shiny, bslib, leaflet and plotly are installed", all(have),
           if (all(have)) paste(pk, vapply(pk, function(p) as.character(utils::packageVersion(p)), ""), collapse = ", ")
           else paste("install.packages(c(", paste0('"', pk[!have], '"', collapse = ", "), "))"))
    if (!all(have)) stop("missing packages")
    suppressPackageStartupMessages({ library(shiny); library(bslib) })
    .source_core("A")
    for (f in c("fems_download", "tooltips", "plots", "mod_station", "mod_rx", "mod_quality", "mod_climatology", "mod_event", "mod_same", "mod_season")) {
      r <- .run(paste("source", f), source(file.path("R", paste0(f, ".R"))))
      .check(paste0("A5 R/", f, ".R loads"), r$ok)
    }
    .check("A6 jsonlite still not attached", !("package:jsonlite" %in% search()))

    .section("B. The UI (from app.R)")
    ex <- parse("app.R")
    is_ui <- vapply(ex, function(e) is.call(e) && identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name("ui")), logical(1))
    .check("B1 app.R has exactly one `ui <- ...`", sum(is_ui) == 1)
    env <- new.env(parent = globalenv())
    for (e in ex[vapply(ex, function(e) is.call(e) && identical(e[[1]], as.name("<-")) &&
                          as.character(e[[2]]) %in% c("app_css", "coming", "APP_NAME", "APP_BLUE"), logical(1))]) eval(e, env)
    ui <- eval(ex[[which(is_ui)]], env)
    .check("B2 the UI builds", inherits(ui, c("shiny.tag", "shiny.tag.list", "bslib_page")))
    ids <- character(0); vals <- character(0); srcs <- character(0)
    walk <- function(x) {
      if (inherits(x, "shiny.tag")) {
        if (!is.null(x$attribs$id)) ids <<- c(ids, as.character(x$attribs$id))
        if (identical(x$name, "img") && !is.null(x$attribs$src)) srcs <<- c(srcs, as.character(x$attribs$src))
        if (!is.null(x$attribs[["data-value"]])) vals <<- c(vals, as.character(x$attribs[["data-value"]]))
        for (ch in x$children) walk(ch)
      } else if (is.list(x)) for (ch in x) walk(ch)
    }
    walk(htmltools::tagQuery(ui)$allTags())
    need <- c("main_nav", "station-card", "station-map",
              paste0("rx-on_", RX_VARS$var), paste0("rx-", c("preset", "preset_note", "ign_on", "ign", "min_window")),
              paste0("rx-sl_", RX_VARS$var[RX_VARS$kind == "linear"]),
              paste0("rx-", c("min_", "max_"), "windDirection"),
              paste0("rx-msg_", RX_VARS$var),
              "clim-title", "clim-controls", "clim-heat", "clim-tbl", "clim-caption", "clim-outline_on", "clim-outline_pct",
              "clim-totals_note", "clim-totals_main", "clim-totals_limit", "clim-csv_totals", "clim-csv_years",
              paste0("event-", c("dates", "back", "fwd", "controls", "plot_box", "caption", "totals_main", "totals_limit",
                                 "csv_daily", "csv_hourly")),
              paste0("season-", c("year", "window", "recent_box", "whole", "refresh", "status", "plot_box", "caption", "totals_note",
                                  "totals", "csv_days")),
              paste0("same-", c("window", "use_event", "controls", "plot_box", "caption", "totals_note",
                                "totals_main", "totals_limit", "csv_years", "csv_days")),
              "quality-items", "quality-coverage", "quality-log")
    miss <- setdiff(need, ids)
    .check("B3 every input/output id the server uses is in the UI", !length(miss), paste("missing:", paste(miss, collapse = ", ")))
    .check("B4 no slider for the circular wind direction", !"rx-sl_windDirection" %in% ids)
    lin <- RX_VARS$var[RX_VARS$kind == "linear"]
    .check("B4b no number boxes for the linear variables (sliders only)",
           !any(paste0("rx-", c(outer(c("min_", "max_"), lin, paste0))) %in% ids))
    .check("B4c the sidebar has a Station and a Prescription heading with a rule between",
           all(c("Station", "Prescription") %in% unlist(htmltools::tagQuery(ui)$find(".sb-head")$selectedTags() |>
                                                        lapply(function(t) as.character(t$children[[1]])))) &&
             length(htmltools::tagQuery(ui)$find("hr.sb-rule")$selectedTags()) == 1)
    dup <- unique(ids[duplicated(ids)])
    .check("B5 no duplicate ids", !length(dup), paste(dup, collapse = ", "))
    tabs <- c("station", "quality", "climatology", "season", "past", "same", "forecast", "about")
    .check("B6 all eight tabs are present", all(tabs %in% vals), paste("missing:", paste(setdiff(tabs, vals), collapse = ", ")))
    tv <- unique(vals[vals %in% tabs])
    .check("B6b tab order: Station, Climatology, Season tracker, Past event, Same dates, Forecast, Data quality, About",
           identical(tv, c("station", "climatology", "season", "past", "same", "forecast", "quality", "about")), paste(tv, collapse = ", "))
    html <- as.character(htmltools::renderTags(ui)$html)
    srcs <- unique(srcs[!startsWith(srcs, "http") & !startsWith(srcs, "data:")])   # from the tag walk, not regex on HTML
    www <- list.files("www")
    .check("B8 every image the UI uses is in www/ with exactly that name (Connect is case-sensitive)",
           length(srcs) > 0 && all(srcs %in% www), paste(srcs, collapse = ", "))
    .check("B9 the banner shows the RxWx logo (left) and the University of Arizona logo (right)",
           all(c("RxWxLogo_banner.png", "UAlogo_banner.png") %in% srcs))
    .check("B10 the browser tab is titled Prescribed Fire Weather Explorer",
           grepl("<title>Prescribed Fire Weather Explorer</title>", as.character(htmltools::renderTags(ui)$head), fixed = TRUE) ||
             grepl("Prescribed Fire Weather Explorer", html, fixed = TRUE))
    .check("B7 renders to HTML; the preset menu offers every preset, starting with the example",
           all(vapply(RX_PRESET_META$label, grepl, logical(1), x = html, fixed = TRUE)))

    .section("B11. Heatmap colours (docs/SPEC.md 'Colour')")
    lum <- function(hex) { m <- grDevices::col2rgb(hex) / 255; m <- ifelse(m <= 0.04045, m / 12.92, ((m + 0.055) / 1.055)^2.4)
                           as.numeric(c(0.2126, 0.7152, 0.0722) %*% m) }
    for (thr0 in c(20, 30, 50, 75)) {
      sc <- .rx_scale(thr0)
      cols <- vapply(sc$colorscale, function(x) x[[2]], "")[c(TRUE, FALSE)]   # one colour per step
      .check(sprintf("B11.%d a %d%% split: every step is darker than the one before (reads without hue)", thr0, thr0),
             all(diff(vapply(cols, lum, 0)) < 0), paste(cols, collapse = " "))
    }
    sc <- .rx_scale(30)
    .check("B11.9 the split is a step edge, labelled on the colour bar", 30 %in% sc$tickvals &&
             grepl("threshold", sc$ticktext[sc$tickvals == 30], fixed = TRUE))
    .check("B11.10 warm below the split, blue above (the ramp's ends)", sc$colour_at(5) != sc$colour_at(95) &&
             grDevices::col2rgb(sc$colour_at(25))[1] > grDevices::col2rgb(sc$colour_at(25))[3] &&
             grDevices::col2rgb(sc$colour_at(35))[3] > grDevices::col2rgb(sc$colour_at(35))[1])
    sh <- .rx_scale(4, zmax = 7, step = 1, unit = " h", split_label = "min window")
    .check("B11.11 Same dates: hours scale split at the minimum window", identical(sh$tickvals, as.numeric(0:7)) &&
             grepl("min window", sh$ticktext[5], fixed = TRUE))

    .section("C. Prescription module (testServer)")
    ex_on <- .rx_example_limits()
    shiny::testServer(mod_rx_server, {
      args <- list()
      for (i in seq_len(nrow(RX_EXAMPLE))) {
        v <- RX_EXAMPLE$var[i]
        args[[paste0("on_", v)]] <- RX_EXAMPLE$on[i]
        if (RX_VARS$kind[RX_VARS$var == v] == "linear") args[[paste0("sl_", v)]] <- c(RX_EXAMPLE$min[i], RX_EXAMPLE$max[i])
        else { args[[paste0("min_", v)]] <- as.character(RX_EXAMPLE$min[i]); args[[paste0("max_", v)]] <- as.character(RX_EXAMPLE$max[i]) }
      }
      do.call(session$setInputs, args)
      session$elapse(RX_DEBOUNCE_MS + 100)
      r0 <- session$returned()
      .check("C0 the preset note says the values are drafts, not a recommendation",
             grepl("not a recommendation", paste(unlist(output$preset_note), collapse = " "), fixed = TRUE))
      .check("C1 starts as the example prescription", identical(r0$var, ex_on$var) && identical(r0$min, ex_on$min) &&
               identical(r0$max, ex_on$max))
      .check("C1b precipitation 0-0: the handle at the slider's start means no lower limit, max stays 0",
             is.na(r0$min[r0$var == "precipitation"]) && r0$max[r0$var == "precipitation"] == 0)
      session$setInputs(sl_relativeHumidity = c(20, 30)); session$elapse(RX_DEBOUNCE_MS + 100)
      .check("C2 a slider move is used", session$returned()$max[session$returned()$var == "relativeHumidity"] == 30)
      m <- RX_VARS[RX_VARS$var == "temperature", ]
      session$setInputs(sl_temperature = c(40, m$slider_max)); session$elapse(RX_DEBOUNCE_MS + 100)
      rr <- session$returned()
      .check("C3 a handle at the slider's end means no limit on that side",
             is.na(rr$max[rr$var == "temperature"]) && rr$min[rr$var == "temperature"] == 40)
      session$setInputs(sl_temperature = c(m$slider_min, m$slider_max)); session$elapse(RX_DEBOUNCE_MS + 100)
      .check("C4 the full range is no limit at all, and says so",
             !"temperature" %in% rx_constrained(session$returned()) &&
               grepl("full range", paste(unlist(output$msg_temperature), collapse = " "), fixed = TRUE))
      session$setInputs(sl_temperature = c(40, 85)); session$elapse(RX_DEBOUNCE_MS + 100)
      session$setInputs(on_windDirection = TRUE, min_windDirection = "315", max_windDirection = "45"); session$elapse(RX_DEBOUNCE_MS + 100)
      rr <- session$returned()
      .check("C5 turning on a wrapping wind sector (NW 315 -> NE 45) works", "windDirection" %in% rr$var &&
               rr$min[rr$var == "windDirection"] == 315 && rr$max[rr$var == "windDirection"] == 45)
      .check("C6 the compass menus offer 16 points", length(.RX_COMPASS) == 16 && all(c("315", "45", "180", "270") %in% .RX_COMPASS))
      session$setInputs(preset = "example", ign_on = FALSE, min_window = 4); session$elapse(RX_DEBOUNCE_MS + 100)
      .check("C6b the prescription carries the ignition period (none) and the minimum window (4 h)",
             is.null(rx_ignition(session$returned())) && rx_min_window(session$returned()) == 4)
      session$setInputs(ign_on = TRUE, ign = c(10, 17), min_window = 6); session$elapse(RX_DEBOUNCE_MS + 100)
      rr <- session$returned()
      .check("C6c turning on an ignition period 10:00-17:00 and a 6 h window reaches the tabs",
             identical(rx_ignition(rr), c(10L, 17L)) && rx_min_window(rr) == 6)
      .check("C6d ... and the preset note says the values were edited",
             grepl("Edited from this preset", paste(unlist(output$preset_note), collapse = " "), fixed = TRUE))
      pv <- .rx_preset_values("conifer"); a <- list(preset = "conifer", ign_on = TRUE, ign = pv$ign, min_window = pv$min_window)
      for (i in seq_len(nrow(pv$rows))) {
        v <- pv$rows$var[i]; a[[paste0("on_", v)]] <- pv$rows$on[i]
        if (RX_VARS$kind[RX_VARS$var == v] == "linear") a[[paste0("sl_", v)]] <- c(pv$rows$min[i], pv$rows$max[i])
        else { a[[paste0("min_", v)]] <- "180"; a[[paste0("max_", v)]] <- "270" }
      }
      do.call(session$setInputs, a); session$elapse(RX_DEBOUNCE_MS + 100)
      .check("C6e controls holding exactly a preset's values are not 'edited'",
             !grepl("Edited", paste(unlist(output$preset_note), collapse = " "), fixed = TRUE) &&
               identical(rx_ignition(session$returned()), c(9L, 17L)) && rx_min_window(session$returned()) == 6)
      session$setInputs(on_temperature = FALSE); session$elapse(RX_DEBOUNCE_MS + 100)
      .check("C7 unticking a variable removes its limit", !"temperature" %in% session$returned()$var)
    })

    .section("D. Climatology module (testServer, real station)")
    mf <- hourly_bundled_manifest()
    if (!nrow(mf)) {
      .info("D0 skipped", "no bundled snapshots")
    } else {
      st0 <- hourly_load_station(mf$station_id[1]); st0$report <- quality_report(st0)
      rx0 <- rx_validate(data.frame(var = ex_on$var, min = ex_on$min, max = ex_on$max, stringsAsFactors = FALSE))
      shiny::testServer(mod_climatology_server, args = list(st = shiny::reactive(st0), rx = shiny::reactive(rx0)), {
        s <- summ()
        .check("D1 the summary has 288 in_rx cells", sum(s$long$value == "in_rx") == 288)
        session$setInputs(outline_on = TRUE, outline_pct = 50); session$elapse(RX_DEBOUNCE_MS + 100)
        .check("D1b outline threshold follows the slider (50%)", identical(thr(), 50))
        session$setInputs(outline_on = FALSE); session$elapse(RX_DEBOUNCE_MS + 100)
        .check("D1c unticking the outline turns it off", is.na(thr()))
        session$setInputs(outline_on = TRUE, outline_pct = 40); session$elapse(RX_DEBOUNCE_MS + 100)
        .check("D2 the table output renders, with cells at or above 40% in bold",
               grepl("<table", paste(output$tbl, collapse = ""), fixed = TRUE) && grepl("<b>", paste(output$tbl, collapse = ""), fixed = TRUE))
        .check("D3 the heatmap output renders", !is.null(output$heat) && grepl("heatmap", paste(output$heat, collapse = ""), fixed = TRUE))
        dj <- paste(output$doy, collapse = "")
        .check("D3b the day-of-year chart renders with sun lines and the threshold contour",
               grepl("heatmap", dj, fixed = TRUE) && grepl("Sunrise / sunset", dj, fixed = TRUE) && grepl("contour", dj, fixed = TRUE))
        .check("D3c day-of-year summary: 365 days x 24 hours of in_rx", sum(summ_doy()$value == "in_rx") == 365 * 24)
        .check("D3d the widget stays a sensible size (under 4 MB)", nchar(dj) < 4e6, sprintf("%.1f MB", nchar(dj) / 1e6))
        bv <- paste(output$byvar_temperature, collapse = "")
        .check("D3e a by-variable panel renders for each limit on (temperature shown)",
               grepl("heatmap", bv, fixed = TRUE) && grepl("within this limit", bv, fixed = TRUE) &&
                 grepl("byvar_relativeHumidity", paste(output$byvar_box, collapse = ""), fixed = TRUE))
        rg <- paste(output$range_relativeHumidity, collapse = "")
        .check("D3f the daily range chart renders with median and bands", grepl("Median", rg, fixed = TRUE) && grepl("Middle half", rg, fixed = TRUE) && grepl("8 in 10 hours", rg, fixed = TRUE))
        .check("D3g no daily range for precipitation (always 0)", !"precipitation" %in% range_vars())
        tm <- paste(output$totals_main, collapse = "")
        .check("D5 the totals table renders, 12 months + Year", grepl("<table", tm, fixed = TRUE) && grepl("Year", tm, fixed = TRUE) &&
                 all(vapply(month.abb, grepl, logical(1), x = tm, fixed = TRUE)))
        .check("D6 the by-limit table has a 'met' and an 'only one failing' column per limit",
               grepl("only one failing", paste(output$totals_limit, collapse = ""), fixed = TRUE))
        csv <- output$csv_totals
        ln <- readLines(csv, encoding = "UTF-8", warn = FALSE)
        .check("D7 the totals CSV downloads, with header lines and the station name",
               length(ln) > 20 && any(grepl(st0$meta$name, ln, fixed = TRUE)) && any(grepl("^\"period\"", ln)))
        ln2 <- readLines(output$csv_years, encoding = "UTF-8", warn = FALSE)
        .check("D8 the per-year CSV downloads", any(grepl("^\"year\"", ln2)))
        cap <- paste(unlist(output$caption), collapse = " ")
        .check("D4 the caption states the years and the excluded hours",
               grepl("Years 2005-2025", cap, fixed = TRUE) && grepl("are excluded", cap, fixed = TRUE))
      })
    }

    .section("D9. Climatology with an ignition period")
    if (nrow(mf)) {
      rxi <- rx_with_settings(rx0, ignition = c(10, 17))
      shiny::testServer(mod_climatology_server, args = list(st = shiny::reactive(st0), rx = shiny::reactive(rxi)), {
        .check("D9.1 the ignition period 10:00-17:00 is drawn per month (start and end lines, no wash)",
               nrow(ign_month()) == 12 && all(ign_month()$end - ign_month()$start == 7))
        ti <- tot()$per_year; ti <- ti[ti$value == "in_rx" & ti$period == "Year", ]
        .check("D9.2 the totals count only ignition hours (7 a day)",
               all(ti$hours <= 366 * 7) && any(ti$hours >= 365 * 7 * 0.99), paste(range(ti$hours), collapse = "-"))
        .check("D9.3 the totals note names the ignition period",
               grepl("10:00-17:00 clock time", paste(unlist(output$totals_note), collapse = " "), fixed = TRUE))
      })
    }

    .section("E. Past event module (testServer, real station)")
    if (nrow(mf)) {
      shiny::testServer(mod_event_server, args = list(st = shiny::reactive(st0), rx = shiny::reactive(rx0)), {
        r <- range(st0$hourly$date)
        session$setInputs(dates = c(r[2] - 6, r[2]))
        e <- ev()
        .check("E1 a 7-day event: 168 hours flagged, panels for temperature, RH, wind speed and direction",
               nrow(e$fl) == 168 && all(c("temperature", "relativeHumidity", "windSpeed", "windDirection") %in% e$show),
               paste(nrow(e$fl), "hours;", paste(e$show, collapse = ", ")))
        .check("E2 precipitation gets a panel because it is limited", "precipitation" %in% e$show)
        .check("E3 the chart renders", grepl("scatter", paste(output$plot, collapse = ""), fixed = TRUE))
        .check("E4 the daily table: 7 days + Total", grepl("Total", paste(output$totals_main, collapse = ""), fixed = TRUE) &&
                 lengths(regmatches(paste(output$totals_main, collapse = ""), gregexpr("<tr", paste(output$totals_main, collapse = "")))) == 9)
        rl <- function(f) { con <- file(f, encoding = "UTF-8-BOM"); on.exit(close(con)); readLines(con, warn = FALSE) }
        ln <- rl(output$csv_daily); lh <- rl(output$csv_hourly)
        .check("E5 both CSVs download (daily and 168 hourly rows)", any(grepl("^\"day\"", ln)) && sum(!startsWith(lh, "#")) == 169)
        session$setInputs(dates = c(r[2] - 20, r[2]))
        msg <- tryCatch({ ev(); "" }, error = function(e) conditionMessage(e))
        .check("E6 more than 14 days is refused in plain words", grepl("up to 14 days", msg, fixed = TRUE), msg)
      })
    }

    .section("F. Same dates module (testServer, real station)")
    if (nrow(mf)) {
      shiny::testServer(mod_same_server, args = list(st = shiny::reactive(st0), rx = shiny::reactive(rx0),
                                                     event_dates = shiny::reactive(as.Date(c("2015-04-01", "2015-04-07")))), {
        session$setInputs(window = as.Date(c("2001-03-15", "2001-04-15")))
        session$elapse(RX_DEBOUNCE_MS + 100)
        r <- res()
        .check("F1 15 Mar-15 Apr: 32 days a year, every year of the record",
               length(r$labels) == 32 && diff(r$years) + 1 == length(unique(r$tw$days$wyear)),
               sprintf("%d days, %d-%d", length(r$labels), r$years[1], r$years[2]))
        .check("F2 the heatmap renders", grepl("heatmap", paste(output$plot, collapse = ""), fixed = TRUE))
        tm <- paste(output$totals_main, collapse = "")
        .check("F3 the totals table has the summary rows", all(vapply(c("Typical year", "Lowest", "Highest"), grepl, logical(1), x = tm, fixed = TRUE)))
        rl <- function(f) { con <- file(f, encoding = "UTF-8-BOM"); on.exit(close(con)); readLines(con, warn = FALSE) }
        .check("F4 both CSVs download", any(grepl("^\"year\"", rl(output$csv_years))) && any(grepl("^\"year\",\"date\"", rl(output$csv_days))))
        session$setInputs(window = as.Date(c("2001-01-01", "2001-06-30")))
        msg <- tryCatch({ res(); "" }, error = function(e) conditionMessage(e))
        .check("F5 more than 92 days is refused in plain words", grepl("up to 92 days", msg, fixed = TRUE), msg)
        session$setInputs(window = as.Date(c("2001-12-20", "2001-01-10")))
        .check("F6 a window through New Year works (22 days)", length(res()$labels) == 22)
      })
    }

    .section("G. Season tracker module (testServer, real station, fake FEMS)")
    if (nrow(mf)) {
      now0 <- as.POSIXct(paste(max(st0$hourly$date) + 2, "08:30"), tz = st0$meta$tz)
      calls <- 0
      fake <- function(id, start, end, verbose = FALSE, ...) {
        calls <<- calls + 1
        d <- .fake_wx(seq(as.Date(start), as.Date(end), by = "day"), station_id = id)
        k <- as.integer(d$date) * 24 + d$hour; cut <- as.integer(as.Date(format(now0, tz = st0$meta$tz))) * 24 + 8
        d$record_type <- ifelse(k <= cut, "O", "F"); d[k <= cut + 7 * 24 + 3, ]
      }
      rxs <- rx_with_settings(rx0, ignition = c(10, 17), min_window = 4)
      shiny::testServer(mod_season_server, args = list(st = shiny::reactive(st0), rx = shiny::reactive(rxs),
                                                       active = shiny::reactive(TRUE), fetch = fake, now = function() now0), {
        session$flushReact()
        .check("G1 opening the tab fetches live data once", calls == 1, paste(calls, "call(s)"))
        n0 <- nrow(st0$hourly); n1 <- nrow(st_full()$hourly)
        .check("G2 the latest observations are appended (the day after the record, then today through 08:00)",
               n1 - n0 == 24 + 9, paste(n1 - n0, "hours added"))
        y <- format(as.Date(format(now0, tz = st0$meta$tz)), "%Y")
        session$setInputs(year = y, window = as.Date(c("2001-01-01", "2001-12-31")))
        d <- days()
        .check("G3 forecast days follow today: hatched part counted apart from observed",
               any(d$in_rx_fc > 0) && all(d$in_rx_fc[d$date < as.Date(format(now0, tz = st0$meta$tz))] == 0))
        t0 <- as.Date(format(now0, tz = st0$meta$tz))
        session$setInputs(recent = 1)
        xr <- xrange()
        .check("G3b 'Last 7 days + forecast' zooms to a week back through the forecast's last day",
               xr[1] == t0 - 7 - 0.5 && xr[2] == max(d$date[d$has_fc]) + 0.5, paste(format(xr + 0.5), collapse = " to "))
        .check("G3c ... and the button is offered for the current year", grepl("Last 7 days + forecast", paste(output$recent_box, collapse = ""), fixed = TRUE))
        session$setInputs(whole = 1)
        .check("G3d 'Whole year' goes back to the season window", xrange()[1] == as.Date(paste0(y, "-01-01")) - 0.5)
        pj <- paste(output$plot, collapse = "")
        .check("G4 the chart renders with observed and forecast bars, the band and the typical year",
               grepl("FEMS forecast", pj, fixed = TRUE) && grepl("Middle half of years", pj, fixed = TRUE) &&
                 grepl("Typical year (median)", pj, fixed = TRUE))
        tb <- paste(output$totals, collapse = "")
        .check("G5 the season table: this season, forecast, typical year", grepl("Typical year, same dates", tb, fixed = TRUE) &&
                 grepl("Forecast (next 7 days)", tb, fixed = TRUE))
        rl <- function(f) { con <- file(f, encoding = "UTF-8-BOM"); on.exit(close(con)); readLines(con, warn = FALSE) }
        cs <- rl(output$csv_days)
        .check("G6 the daily CSV downloads with obs/forecast columns and the reference", any(grepl("in_rx_fc", cs, fixed = TRUE)) &&
                 any(grepl("ref_median", cs, fixed = TRUE)))
        session$setInputs(year = as.character(as.integer(y) - 3))
        .check("G7 a past year has no forecast, and no 'Last 7 days' button", all(days()$in_rx_fc == 0) &&
                 !grepl("Last 7 days", paste(output$recent_box, collapse = ""), fixed = TRUE))
        session$setInputs(window = as.Date(c("2001-10-01", "2001-03-01")))
        msg <- tryCatch({ win(); "" }, error = function(e) conditionMessage(e))
        .check("G8 a window through New Year is refused in plain words", grepl("within one calendar year", msg, fixed = TRUE), msg)
        session$setInputs(refresh = 1)
        .check("G9 Refresh fetches again", calls == 2)
      })
    }
  }, error = function(e) {
    .T$abort <- paste("test aborted:", conditionMessage(e))
    .say(""); .say("ABORTED: ", conditionMessage(e))
  })
  .test_finish()
})
