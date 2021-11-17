#' modConfigUI
#'
#' @param id ...
#' @return No object returned, this is a shiny module
#' @export


modConfigUI <- function(id) {
  fluidRow(
    column(3,
           tags$p(HTML(paste(textOutput(NS(id,"config_explanation1"))))),
           tags$hr(),
           tags$p(HTML(paste(textOutput(NS(id, "config_explanation2"))))),
           fileInput(NS(id, "configfile"), NULL, label = "", width = '100%',
                     accept = c(".json", ".csv"), multiple = FALSE,
                     buttonLabel = "File ..."),
           downloadButton(NS(id, "download"), "Download template", class = "info-xs"),
           actionButton(NS(id, "reset"), "Reset"),
           # actionButton(NS(id, "save"), "Save"),
           tags$hr(),
    ),
    column(9,
           h4(textOutput(NS(id, "config_instruction"))),
           DT::dataTableOutput(NS(id, "mod_table")),
           # span(verbatimTextOutput(NS(id, "config_issues")), style="color:red"),
           span(htmlOutput(NS(id, "config_issues")), style="color:red"),
           span(htmlOutput(NS(id, "config_green")), style="color:green"),
    )
  )
}