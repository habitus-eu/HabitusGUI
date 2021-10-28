#' modConfigUI
#'
#' @param id ...
#' @return No object returned, this is a shiny module
#' @export


modConfigUI <- function(id) {
  fluidRow(
    column(3,
           tags$p(HTML(paste(textOutput(NS(id,"config_explanation"))))),
           tags$hr(),
           tags$p(HTML(paste(textOutput(NS(id, "config_instruction"))))),
           fileInput(NS(id, "configfile"), NULL, label = "", width = '100%',
                     accept = c(".json", ".csv"), multiple = FALSE,
                     buttonLabel = "File ..."),
           downloadButton(NS(id, "download"), "Download template", class = "info-xs"),
           actionButton("reset", "Reset"),
           actionButton("save", "Save"),
           tags$hr(),
    ),
    column(9,
           DT::dataTableOutput(NS(id, "mod_table"))
    )
  )
}