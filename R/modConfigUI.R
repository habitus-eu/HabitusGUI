#' modConfigUI
#'
#' @param id ...
#' @return No object returned, this is a shiny module
#' @export


modConfigUI <- function(id) {
  # tagList(
  fluidRow(
    column(3,
           # verbatimTextOutput(NS(id,"config_header")),
           # tags$h1(HTML(paste(textOutput(NS(id,"config_header"))))),
           tags$p(HTML(paste(textOutput(NS(id,"config_explanation"))))),
           tags$hr(),
           tags$h5(HTML(paste(textOutput(NS(id,"config_instruction"))))),
           fileInput(NS(id,"configfile"), NULL, label = "", width = '100%',
                     accept = c(".json", ".csv"), multiple = FALSE,
                     buttonLabel = "File ..."),
           actionButton("reset", "Reset"),
           actionButton("save", "Save"),
           tags$hr(),
    ),
    column(9,
           DT::dataTableOutput(NS(id, "mod_table"))
    )
  )
  # )
}