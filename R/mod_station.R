# ==============================================================================
# R/mod_station.R -- choose and load one station
#
# Two UI pieces: mod_station_sidebar_ui() (search, featured list, the loaded
# station's card and badge) and mod_station_map_ui() (the Station tab map).
# mod_station_server() returns a list of reactives:
#   st()          the loaded station object (hourly_load_station) + $report,
#                 or NULL before anything is loaded
#   open_quality  increments when the badge link is clicked
# Loads come from the bundled snapshot when there is one; "Refresh from FEMS"
# forces a live download. Loaded stations are cached for the session only.
# Requires R/constants.R, R/hourly.R, R/quality.R, R/tooltips.R; leaflet is
# namespaced, never attached.
# ==============================================================================

.station_choices <- function() {
  a <- utils::read.csv(STATION_META_FILE, colClasses = c(station_id = "character"), stringsAsFactors = FALSE)
  b <- if (file.exists(STATION_MASTER_FILE))
    utils::read.csv(STATION_MASTER_FILE, colClasses = c(station_id = "character"), stringsAsFactors = FALSE)
    else NULL
  a$state <- if (!is.null(b)) b$state[match(a$station_id, b$station_id)] else NA
  f <- if (file.exists(STATIONS_FEATURED))
    utils::read.csv(STATIONS_FEATURED, colClasses = c(station_id = "character"), stringsAsFactors = FALSE)
    else data.frame(station_id = character(0), note = character(0))
  mf <- hourly_bundled_manifest()
  a$featured <- a$station_id %in% f$station_id
  a$bundled  <- a$station_id %in% mf$station_id
  a$note     <- f$note[match(a$station_id, f$station_id)]
  a$label    <- paste0(a$station_name, " (", a$station_id, ")", ifelse(is.na(a$state), "", paste0(", ", a$state)))
  a[order(!a$featured, a$station_name), ]
}

mod_station_sidebar_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(shiny::uiOutput(ns("card")))
}

mod_station_map_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::p(class = "text-muted mb-2",
             "Every FEMS weather station. Larger blue markers are featured stations; those bundled with the app load instantly. ",
             "Click a marker, then 'Select this station', or search in the sidebar."),
    leaflet::leafletOutput(ns("map"), height = "620px"))
}

mod_station_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    stations <- .station_choices()
    feat <- stations[stations$featured, ]
    cache <- shiny::reactiveValues()
    current <- shiny::reactiveVal(NULL)
    open_quality <- shiny::reactiveVal(0)
    # ~2,000 choices are sent with the input itself: an update sent right after
    # renderUI can arrive before the browser has built the input.
    choice_vec <- stats::setNames(stations$station_id, stations$label)

    # ---- sidebar card -------------------------------------------------------------
    output$card <- shiny::renderUI({
      st <- current()
      pick <- shiny::div(
        shiny::selectizeInput(ns("pick"), "Station", width = "100%",
                              choices = c(stats::setNames("", ""), choice_vec),
                              selected = if (is.null(st)) "" else st$meta$station_id,
                              options = list(placeholder = "Search by name or ID")),
        if (nrow(feat)) shiny::div(class = "small text-muted mb-1", "Featured:"),
        shiny::div(class = "d-flex flex-wrap gap-1 mb-2",
                   lapply(seq_len(nrow(feat)), function(i)
                     shiny::actionButton(ns(paste0("feat_", feat$station_id[i])), feat$station_name[i],
                                         class = "btn-sm btn-outline-primary",
                                         title = if (feat$bundled[i]) "Bundled: loads instantly" else "Downloads from FEMS"))),
        shiny::actionButton(ns("load"), "Load station", class = "btn-primary btn-sm w-100"))
      if (is.null(st)) return(pick)
      n_aff <- quality_badge(st$report)
      shiny::tagList(
        shiny::div(class = "station-card mb-2",
                   shiny::div(class = "fw-semibold", st$meta$name, " ", shiny::span(class = "text-muted", paste0("(", st$meta$station_id, ")"))),
                   shiny::div(class = "small text-muted",
                              paste0(if (is.na(st$meta$state)) "" else paste0(st$meta$state, " \u00b7 "),
                                     format(round(st$meta$elev_ft), big.mark = ","), " ft \u00b7 ", st$source$detail)),
                   shiny::actionLink(ns("badge"),
                                     shiny::span(class = if (n_aff) "badge text-bg-warning mt-1" else "badge text-bg-secondary mt-1",
                                                 if (n_aff == 1) "1 data note" else paste(n_aff, "data notes")),
                                     title = "Open the Data quality tab"),
                   if (identical(st$source$type, "bundled"))
                     shiny::actionLink(ns("refresh"), "Refresh from FEMS", class = "small ms-2")),
        shiny::tags$details(shiny::tags$summary(class = "small", "Change station"), pick))
    })


    # ---- loading ----------------------------------------------------------------------
    do_load <- function(sid, prefer_bundled = TRUE) {
      if (is.null(sid) || !nzchar(sid)) {
        shiny::showNotification("Choose a station first.", type = "warning"); return(invisible())
      }
      key <- paste0(sid, if (prefer_bundled) "" else "_live")
      if (!is.null(cache[[key]])) { current(cache[[key]]); return(invisible()) }
      st <- shiny::withProgress(message = "Loading station", value = 0, {
        tryCatch({
          s <- hourly_load_station(sid, prefer_bundled = prefer_bundled,
                                   progress = function(f, msg) shiny::setProgress(f, detail = msg))
          shiny::setProgress(0.95, detail = "Checking data quality...")
          s$report <- quality_report(s)
          s
        },
        fems_unavailable = function(e) { shiny::showNotification(conditionMessage(e), type = "error", duration = 10); NULL },
        error = function(e) { shiny::showNotification(paste("Couldn't load the station:", conditionMessage(e)),
                                                      type = "error", duration = 10); NULL })
      })
      if (!is.null(st)) { cache[[key]] <- st; current(st) }
    }

    shiny::observeEvent(input$load, do_load(input$pick))
    shiny::observeEvent(input$refresh, do_load(current()$meta$station_id, prefer_bundled = FALSE))
    shiny::observeEvent(input$badge, open_quality(open_quality() + 1))
    for (sid in feat$station_id) local({
      s <- sid
      shiny::observeEvent(input[[paste0("feat_", s)]], do_load(s))
    })
    shiny::observeEvent(input$map_pick, {
      do_load(input$map_pick)
    })

    # ---- map ----------------------------------------------------------------------------
    output$map <- leaflet::renderLeaflet({
      pop <- paste0(tip_station(stations$station_id, stations$station_name, stations$elevation, stations$state,
                                stations$featured, stations$bundled, stations$note),
                    "<br><button class='btn btn-sm btn-primary mt-2' onclick=\"Shiny.setInputValue('", ns("map_pick"),
                    "', '", stations$station_id, "', {priority: 'event'})\">Select this station</button>")
      other <- !stations$featured
      leaflet::leaflet(options = leaflet::leafletOptions(minZoom = 2)) |>
        leaflet::addProviderTiles("Esri.WorldTopoMap") |>
        leaflet::setView(lng = -98, lat = 39, zoom = 4) |>
        leaflet::addCircleMarkers(lng = stations$longitude[other], lat = stations$latitude[other], radius = 3,
                                  stroke = FALSE, fillColor = "#6b6b6b", fillOpacity = 0.6,
                                  popup = pop[other], label = stations$label[other]) |>
        leaflet::addCircleMarkers(lng = stations$longitude[!other], lat = stations$latitude[!other], radius = 8,
                                  color = "#ffffff", weight = 2, fillColor = "#256abf", fillOpacity = 0.95,
                                  popup = pop[!other], label = stations$label[!other])
    })

    list(st = shiny::reactive(current()), open_quality = shiny::reactive(open_quality()))
  })
}
