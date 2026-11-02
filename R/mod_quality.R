# ==============================================================================
# R/mod_quality.R -- Data quality tab
#
# Shows the loaded station's quality report (st()$report, built once at load
# by quality_report()): "affects results" items first, then "for
# information". Each item says what was found, what the app did, and what it
# affects. Below: the year x variable coverage grid and the run-by-run
# screening log. Takes a reactive st().
# Requires R/constants.R, R/quality.R, R/tooltips.R, R/plots.R.
# ==============================================================================

mod_quality_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("items")),
    bslib::card(bslib::card_header("Coverage: % of hours with a value, by year"),
                plotly::plotlyOutput(ns("coverage"), height = "260px")),
    bslib::card(bslib::card_header("Screening log: every run removed"),
                shiny::div(class = "small text-muted", "Each row is one run of values set to missing before any analysis."),
                shiny::tableOutput(ns("log"))))
}

.quality_item_ui <- function(r) {
  aff <- r$severity == "affects"
  shiny::div(class = paste("dq-item", if (aff) "dq-affects" else "dq-info"),
    shiny::span(class = if (aff) "badge text-bg-warning" else "badge text-bg-secondary",
                if (aff) "Affects results" else "For information"),
    shiny::span(class = "text-muted small ms-2", tools::toTitleCase(r$section)),
    shiny::div(class = "mt-1", r$found),
    shiny::div(class = "small", shiny::strong("What the app did: "), r$action),
    shiny::div(class = "small", shiny::strong("What it affects: "), r$affects))
}

mod_quality_server <- function(id, st) {
  shiny::moduleServer(id, function(input, output, session) {
    output$items <- shiny::renderUI({
      shiny::validate(shiny::need(st(), "Load a station to see its data-quality report."))
      rep <- st()$report
      n_aff <- quality_badge(rep)
      shiny::tagList(
        shiny::h5(paste0("Data quality: ", st()$meta$name)),
        shiny::p(class = "text-muted",
                 sprintf("%d item%s can change results; %d %s for information. Nothing here is hidden from the charts: removed values are counted as excluded hours.",
                         n_aff, if (n_aff == 1) "" else "s", nrow(rep) - n_aff, if (nrow(rep) - n_aff == 1) "is" else "are")),
        lapply(seq_len(nrow(rep)), function(i) .quality_item_ui(rep[i, ])))
    })
    output$coverage <- plotly::renderPlotly({
      shiny::req(st())
      cov <- quality_coverage(st()$hourly)
      labs <- stats::setNames(RX_VARS$label, RX_VARS$var)
      plot_coverage_grid(cov, labs)
    })
    output$log <- shiny::renderTable({
      shiny::req(st())
      lg <- st()$qc_log
      if (!nrow(lg)) return(data.frame(Note = "Nothing was screened out."))
      lab <- RX_VARS$label[match(lg$variable, RX_VARS$var)]
      data.frame(Rule = lg$rule, Variable = ifelse(is.na(lab), lg$variable, lab),
                 Start = paste(format(lg$start_date), sprintf("%02d:00", lg$start_hour)),
                 Hours = lg$hours, Value = ifelse(is.na(lg$value), "", format(lg$value)), check.names = FALSE)
    }, striped = TRUE, spacing = "xs")
  })
}
