#' modConfigUI
#'
#' @param id ...
#' @return No object returned, this is a shiny module
#' @import shiny
#' @export


modConfigUI <- function(id) {
  fluidRow(
    column(3,
           tags$p(HTML(paste(textOutput(NS(id,"config_explanation1"))))),
           tags$hr(),
           tags$p(HTML(paste(textOutput(NS(id, "config_explanation2"))))),
           shinyFiles::shinyFilesButton(NS(id, "configfile"), label = "Configuration file...",
                                        title = "", multiple = FALSE, fileType = c(".json", ".csv")),
           shiny::downloadButton(NS(id, "download"), "Download template", class = "info-xs"),
           shiny::actionButton(NS(id, "reset"), "Reset"),
           tags$hr(),
    ),
    column(9,
           span(h4(textOutput(NS(id, "config_instruction"))), style = "color:purple"),
           DT::DTOutput(NS(id, "mod_table")),
           # This line has no function locally, but seems critical for the app to work on UCloud:
           DT::DTOutput(NS(id, 'test_shinytable1')),
           span(htmlOutput(NS(id, "config_issues")), style = "color:red"),
           span(htmlOutput(NS(id, "config_green")), style = "color:green"),
    )
  )
}