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
              paste0("rx-", c(outer(c("on_", "min_", "max_"), RX_VARS$var, paste0))),
              paste0("rx-sl_", RX_VARS$var[RX_VARS$kind == "linear"]),
              paste0("rx-msg_", RX_VARS$var),
              "clim-title", "clim-controls", "clim-heat", "clim-tbl", "clim-caption", "clim-outline_on", "clim-outline_pct",
              "quality-items", "quality-coverage", "quality-log")
    miss <- setdiff(need, ids)
    .check("B3 every input/output id the server uses is in the UI", !length(miss), paste("missing:", paste(miss, collapse = ", ")))
    .check("B4 no slider for the circular wind direction", !"rx-sl_windDirection" %in% ids)
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
           grepl("an example, not a recommendation", html, fixed = TRUE))

    .section("C. Prescription module (testServer)")
    ex_on <- RX_EXAMPLE[RX_EXAMPLE$on, ]
    shiny::testServer(mod_rx_server, {
      args <- list()
      for (i in seq_len(nrow(RX_EXAMPLE))) {
        v <- RX_EXAMPLE$var[i]
        args[[paste0("on_", v)]] <- RX_EXAMPLE$on[i]
        args[[paste0("min_", v)]] <- RX_EXAMPLE$min[i]; args[[paste0("max_", v)]] <- RX_EXAMPLE$max[i]
      }
      do.call(session$setInputs, args)
      session$elapse(RX_DEBOUNCE_MS + 100)
      r0 <- session$returned()
      .check("C1 starts as the example prescription", identical(r0$var, ex_on$var) && identical(r0$min, ex_on$min) &&
               identical(r0$max, ex_on$max))
      session$setInputs(max_relativeHumidity = 30); session$elapse(RX_DEBOUNCE_MS + 100)
      .check("C2 a typed value is used", session$returned()$max[session$returned()$var == "relativeHumidity"] == 30)
      session$setInputs(min_temperature = 90); session$elapse(RX_DEBOUNCE_MS + 100)
      .check("C3 an invalid entry (min above max) keeps the last valid prescription",
             session$returned()$min[session$returned()$var == "temperature"] == 40)
      .check("C4 ... and shows a plain-words message",
             grepl("minimum is above the maximum", paste(unlist(output$msg_temperature), collapse = " "), fixed = TRUE))
      session$setInputs(min_temperature = 40, max_temperature = NA); session$elapse(RX_DEBOUNCE_MS + 100)
      rr <- session$returned()
      .check("C5 a blank box means no limit on that side", is.na(rr$max[rr$var == "temperature"]) && rr$min[rr$var == "temperature"] == 40)
      session$setInputs(on_windDirection = TRUE, min_windDirection = 315, max_windDirection = 45); session$elapse(RX_DEBOUNCE_MS + 100)
      rr <- session$returned()
      .check("C6 turning on a wrapping wind sector (315 -> 45) works", "windDirection" %in% rr$var &&
               rr$min[rr$var == "windDirection"] == 315)
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
