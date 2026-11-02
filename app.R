# ==============================================================================
# app.R -- Prescribed Fire Weather Explorer (RxWx), v0.1 (repo: PrescribedFirePlanner)
#
# A training tool for the Prescribed Fire Training Center: how often, and
# when, is a weather station inside a burn prescription? See docs/SPEC.md and
# docs/ARCHITECTURE.md.
#
# v0.1: Station (map + bundled stations), Data quality, Climatology
# (month x hour). Past event, Same dates and Forecast arrive in v1.
#
# Run locally: shiny::runApp()  |  Deploy: rsconnect::deployApp(appName = "...")
# ==============================================================================

library(shiny)
library(bslib)

# Fixed order; every file namespaces its other packages (CLAUDE.md).
for (f in c("constants", "fems_download", "sun", "hourly", "quality", "rx", "climatology",
            "tooltips", "plots", "mod_station", "mod_rx", "mod_quality", "mod_climatology")) {
  source(file.path("R", paste0(f, ".R")))
}

app_css <- "
.station-card { border: 1px solid var(--bs-border-color); border-radius: .5rem; padding: .5rem .6rem; }
.rx-var .form-group { margin-bottom: .25rem; }
.rx-var .checkbox { margin-bottom: .25rem; }
.dq-item { border-left: 4px solid var(--bs-border-color); padding: .5rem .75rem; margin-bottom: .6rem; background: var(--bs-body-bg); }
.dq-affects { border-left-color: var(--bs-warning); }
.coming-soon { max-width: 46rem; }
/* banner logos: sized so the RxWx tagline is readable; the navbar grows only as much as they need */
.navbar .navbar-brand { display: flex; align-items: center; height: 52px; padding: 0; margin-right: 1.5rem; }
.navbar .app-logo { display: block; height: 52px; width: auto; }
.navbar .ua-link { display: flex; align-items: center; height: 52px; padding: 0 !important; }
.navbar .ua-logo { display: block; height: 40px; width: auto; }
"

coming <- function(title, what) {
  shiny::div(class = "coming-soon",
             shiny::h4(title), shiny::p(what),
             shiny::p(class = "text-muted", "Coming in v1. It will use the station and prescription in the sidebar."))
}

# Banner logos: small cropped copies of www/RxWxLogo_compact.png and www/UAlogo.jpg
# (the originals are 1.1 MB / wide margins). File names are case-sensitive on
# Posit Connect, so they must match www/ exactly.
APP_NAME <- "Prescribed Fire Weather Explorer"
APP_BLUE <- "#0b5e80"   # the RxWx logo's blue

ui <- page_navbar(
  title = tags$img(src = "RxWxLogo_banner.png", height = "52", class = "app-logo",
                   alt = paste0("RxWx: ", APP_NAME)),
  window_title = APP_NAME,
  id = "main_nav",
  theme = bs_theme(version = 5, primary = APP_BLUE),
  header = tags$head(tags$style(HTML(app_css))),
  sidebar = sidebar(
    width = 330,
    mod_station_sidebar_ui("station"),
    mod_rx_ui("rx")),
  nav_panel("Station", value = "station", mod_station_map_ui("station")),
  nav_panel("Climatology", value = "climatology", mod_climatology_ui("clim")),
  nav_panel("Past event", value = "past",
            coming("Past event", "Hour-by-hour conditions for a stretch of up to two weeks, with in-prescription hours highlighted and the limit that failed at each hour.")),
  nav_panel("Same dates", value = "same",
            coming("Same dates, many years", "One window of the year (say 15 Mar to 15 Apr) shown for every year in the record, with hours in prescription counted per year.")),
  nav_panel("Forecast", value = "forecast",
            coming("Synthetic forecast", "Practice reading a forecast: pick a past issue time, see a forecast with widening uncertainty, then reveal what happened. Always marked SYNTHETIC - FOR TRAINING ONLY.")),
  nav_panel("Data quality", value = "quality", mod_quality_ui("quality")),
  nav_panel("About", value = "about",
    div(class = "coming-soon",
      h4("About this tool"),
      p("The Prescribed Fire Weather Explorer (RxWx) shows how often, and when, a weather station's conditions fall inside a prescribed-fire prescription,",
        " using hourly observations from 2005 to 2025. It is a training tool for the Prescribed Fire Training Center, under development; it is not an operational planning tool."),
      h5("How to read the heatmap"),
      p("Each cell is one month and one hour of the day. Its colour is the share of all those hours, across the baseline years,",
        " that met every limit you turned on. Hover a cell for the count of hours behind it, how many were excluded, the limit that failed most often, and typical values."),
      p("Cells at or above the outline threshold (50% to start; set it above the chart) get an orange outline, so the usual burn window stands out."),
      p("Hours are local standard time (LST) all year, as FEMS reports them. At stations that observe daylight saving, a toggle shows clock time instead."),
      h5("Data"),
      p("Hourly RAWS weather from the USDA Forest Service Fire Environment Mapping System (FEMS). Featured stations are bundled with the app as a snapshot;",
        " others download live. Clear sensor faults are removed before analysis and listed on the Data quality tab."),
      tags$img(src = "UA_CSAP_CLIMAS_logos_horiz.png", alt = "University of Arizona, CSAP and CLIMAS logos", style = "max-width: 420px; width: 100%;"),
      p(class = "small text-muted mt-2", "Mike Crimmins, University of Arizona Cooperative Extension."))),
  nav_spacer(),
  nav_item(tags$a(href = "https://www.arizona.edu", target = "_blank", rel = "noopener", title = "The University of Arizona", class = "ua-link",
                  tags$img(src = "UAlogo_banner.png", class = "ua-logo", alt = "The University of Arizona")))
)

server <- function(input, output, session) {
  station <- mod_station_server("station")
  rx <- mod_rx_server("rx")
  mod_quality_server("quality", station$st)
  mod_climatology_server("clim", station$st, rx)

  # after a station loads, go to Climatology; the badge opens Data quality
  observeEvent(station$st(), {
    if (!is.null(station$st())) nav_select("main_nav", "climatology")
  }, ignoreInit = TRUE)
  observeEvent(station$open_quality(), nav_select("main_nav", "quality"), ignoreInit = TRUE)
}

shinyApp(ui, server)
