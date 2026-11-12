# ==============================================================================
# tests/test_static.R -- the offline (Shinylive) build's static mode, in normal R
#
# Turns on static mode (options(rxwx.static = TRUE)) without the FEMS layer and
# checks what the browser copy will do. Doesn't run webR itself: the browser
# check is scripts/build_static.R and its checklist.
#
#   A  static mode on; app files load without R/fems_*.R
#   B  stations: only the bundled ones; the UI has the offline note and no
#      Season "Refresh from FEMS" button
#   C  Season tracker: no FEMS call at all, the record is shown as is, the
#      status line says it's an offline copy
#   D  what scripts/build_static.R ships: no FEMS layer, no .Renviron, and no
#      code using the FEMS-only packages (httr, jsonlite, dplyr, tibble)
#
# No network. Static mode is switched off again at the end. Run from the
# project root:
#     source("tests/test_static.R")
# ==============================================================================

local({
  source("tests/helpers.R", local = TRUE)
  .T <- .test_open("test_static")
  old_opt <- options(rxwx.static = TRUE)
  had_fems <- exists("fems_download_weather", envir = globalenv())
  if (had_fems) rm("fems_download_weather", envir = globalenv())   # the static build has no FEMS layer

  tryCatch({
    .env_header("Static (Shinylive) mode, in normal R (no network)")
    .section("A. Static mode and sources")
    suppressPackageStartupMessages({ library(shiny); library(bslib) })
    .source_core("A")
    .check("A4 RXWX_STATIC is on", isTRUE(RXWX_STATIC))
    for (f in c("tooltips", "plots", "mod_station", "mod_rx", "mod_quality", "mod_climatology", "mod_event", "mod_same", "mod_season")) {
      r <- .run(paste("source", f), source(file.path("R", paste0(f, ".R"))))
      .check(paste0("A5 R/", f, ".R loads without the FEMS layer"), r$ok)
    }
    .check("A6 no FEMS download function exists", !exists("fems_download_weather"))

    .section("B. Stations and UI")
    ch <- .station_choices()
    mf <- hourly_bundled_manifest()
    .check("B1 the station list is exactly the bundled stations", setequal(ch$station_id, mf$station_id),
           paste(nrow(ch), "stations:", paste(ch$station_id, collapse = ", ")))
    ex <- parse("app.R")
    is_ui <- vapply(ex, function(e) is.call(e) && identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name("ui")), logical(1))
    env <- new.env(parent = globalenv())
    for (e in ex[vapply(ex, function(e) is.call(e) && identical(e[[1]], as.name("<-")) &&
                          as.character(e[[2]]) %in% c("app_css", "coming", "APP_NAME", "APP_BLUE"), logical(1))]) eval(e, env)
    html <- as.character(htmltools::renderTags(eval(ex[[which(is_ui)]], env))$html)
    .check("B2 the sidebar and map say it's an offline copy", grepl("Offline copy: bundled stations only", html, fixed = TRUE) &&
             grepl("only the stations bundled with the app", html, fixed = TRUE))
    .check("B3 no Season tracker 'Refresh from FEMS' button", !grepl('id="season-refresh"', html, fixed = TRUE))
    shiny::testServer(mod_station_server, {
      m <- tryCatch(paste(output$map, collapse = ""), error = function(e) paste("ERROR:", conditionMessage(e)))
      .check("B3b the station map renders (every station is featured, so no grey-marker layer)",
             !startsWith(m, "ERROR") && grepl("addCircleMarkers", m, fixed = TRUE), if (startsWith(m, "ERROR")) m else "")
    })
    .check("B4 app.R skips the FEMS layer in static mode", any(grepl('f == "fems_download" && RXWX_STATIC', readLines("app.R"), fixed = TRUE)))

    .section("C. Season tracker: stored record only")
    st0 <- hourly_load_station(mf$station_id[1]); st0$report <- quality_report(st0)
    ex_on <- RX_EXAMPLE[RX_EXAMPLE$on, ]
    rx0 <- rx_validate(data.frame(var = ex_on$var, min = ex_on$min, max = ex_on$max, stringsAsFactors = FALSE))
    calls <- 0
    fake <- function(...) { calls <<- calls + 1; NULL }
    shiny::testServer(mod_season_server, args = list(st = shiny::reactive(st0), rx = shiny::reactive(rx0),
                                                     active = shiny::reactive(TRUE), fetch = fake), {
      session$flushReact()
      .check("C1 opening the tab makes no FEMS call", calls == 0, paste(calls, "call(s)"))
      .check("C2 the record is shown as it is (nothing appended)", nrow(st_full()$hourly) == nrow(st0$hourly))
      .check("C3 the status line says it's an offline copy, with the bundle's last date",
             grepl("Offline copy: observations up to", paste(output$status, collapse = ""), fixed = TRUE) &&
               grepl(format(max(st0$hourly$date), "%d %b %Y"), paste(output$status, collapse = ""), fixed = TRUE))
      y <- format(max(st0$hourly$date), "%Y")
      session$setInputs(year = y, window = as.Date(c("2001-01-01", "2001-12-31")))
      d <- days()
      .check("C4 this year's days are counted, with no forecast", nrow(d) > 0 && all(d$in_rx_fc == 0) && !any(d$has_fc))
      .check("C5 the chart renders", nchar(paste(output$plot, collapse = "")) > 1000)
    })

    .section("D. What the build ships")
    ship_r <- setdiff(list.files("R", pattern = "\\.R$"), c("fems_api.R", "fems_download.R"))
    .check("D1 scripts/build_static.R leaves out the FEMS layer and private files",
           any(grepl('c("fems_api.R", "fems_download.R")', readLines("scripts/build_static.R"), fixed = TRUE)) &&
             any(grepl("Renviron", readLines("scripts/build_static.R"), fixed = TRUE)))
    code <- unlist(lapply(c("app.R", file.path("R", ship_r)), function(f) {
      x <- readLines(f, warn = FALSE); sub("#.*$", "", x)            # drop comments
    }))
    used <- c("httr", "jsonlite", "dplyr", "tibble")[vapply(c("httr", "jsonlite", "dplyr", "tibble"), function(p)
      any(grepl(paste0("\\b", p, "::|library\\(", p, "\\)"), code)), logical(1))]
    .check("D2 the shipped code doesn't use httr, jsonlite, dplyr or tibble (fewer packages for the browser to fetch)",
           !length(used), if (length(used)) paste("uses:", paste(used, collapse = ", ")) else "")
    .check("D3 app.R names plotly and leaflet for shinylive's package scan",
           any(grepl("if (FALSE) { library(plotly); library(leaflet) }", readLines("app.R"), fixed = TRUE)))
    .check("D4 the build output is kept out of git and the Connect bundle",
           any(grepl("^static_site/?$", readLines(".gitignore"))) && any(grepl("^static_site/?$", readLines(".rscignore"))))
    mb <- sum(file.size(c(list.files("data", recursive = TRUE, full.names = TRUE), list.files("www", full.names = TRUE)))) / 2^20
    .info("D5 data and images shipped", sprintf("%.1f MB (plus the webR runtime and packages, fetched by the browser)", mb))
  }, error = function(e) {
    .T$abort <- paste("test aborted:", conditionMessage(e))
    .say(""); .say("ABORTED: ", conditionMessage(e))
  })
  options(old_opt)
  assign("RXWX_STATIC", isTRUE(getOption("rxwx.static", identical(R.version$os, "emscripten"))), envir = globalenv())
  if (had_fems) source("R/fems_download.R")
  .test_finish()
})
