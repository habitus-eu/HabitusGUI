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
           # fileInput(NS(id, "configfile"), NULL, label = "", width = '100%',
           #           accept = c(".json", ".csv"), multiple = FALSE,
           #           buttonLabel = "File ..."),
           shinyFiles::shinyFilesButton(NS(id, "configfile"), label = "Configuration file...",
                                        title = "", multiple = FALSE, fileType = c(".json", ".csv")),
           shiny::downloadButton(NS(id, "download"), "Download template", class = "info-xs"),
           shiny::actionButton(NS(id, "reset"), "Reset"),
           tags$hr(),
    ),
    column(9,
           span(h4(textOutput(NS(id, "config_instruction"))), style="color:purple"),
           DT::DTOutput(NS(id, "mod_table")),
           DT::DTOutput(NS(id, "test_dt_table1")),
           # DT::DTOutput(NS(id, "test_dt_table2")),
           # DT::DTOutput(NS(id, "test_dt_table3")),
           # shiny::dataTableOutput(NS(id, 'test_shinytable1')),
           span(htmlOutput(NS(id, "configfile_check")), style="color:blue"),
           span(htmlOutput(NS(id, "config_issues")), style="color:red"),
           span(htmlOutput(NS(id, "config_green")), style="color:green"),
    )
  )
}