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
    for (f in c("fems_download", "tooltips", "plots", "mod_station", "mod_rx", "mod_quality", "mod_climatology")) {
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
              paste0("rx-on_", RX_VARS$var),
              paste0("rx-sl_", RX_VARS$var[RX_VARS$kind == "linear"]),
              paste0("rx-", c("min_", "max_"), "windDirection"),
              paste0("rx-msg_", RX_VARS$var),
              "clim-title", "clim-controls", "clim-heat", "clim-tbl", "clim-caption", "clim-outline_on", "clim-outline_pct",
              "clim-totals_note", "clim-totals_main", "clim-totals_limit", "clim-csv_totals", "clim-csv_years",
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
    tabs <- c("station", "quality", "climatology", "past", "same", "forecast", "about")
    .check("B6 all seven tabs are present", all(tabs %in% vals), paste("missing:", paste(setdiff(tabs, vals), collapse = ", ")))
    tv <- unique(vals[vals %in% tabs])
    .check("B6b tab order: Station, Climatology, Past event, Same dates, Forecast, Data quality, About",
           identical(tv, c("station", "climatology", "past", "same", "forecast", "quality", "about")), paste(tv, collapse = ", "))
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
    .check("B7 renders to HTML; example values are labelled as an example",
           grepl("Example values, not a recommendation", html, fixed = TRUE))

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
  }, error = function(e) {
    .T$abort <- paste("test aborted:", conditionMessage(e))
    .say(""); .say("ABORTED: ", conditionMessage(e))
  })
  .test_finish()
})
