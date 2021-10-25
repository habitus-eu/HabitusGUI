#' modConfigUI
#'
#' @param id ...
#' @return No object returned, this is a shiny module
#' @export


modConfigUI <- function(id) {
  tagList(
    fileInput("configfile", NULL, label = "Select your configuration file", width = '100%',
              accept = ".json|.csv", multiple = FALSE,
              buttonLabel = "Configuration file ..."),
    actionButton("reset", "Reset"),
    actionButton("save", "Save"),
    tags$hr(),
    DT::dataTableOutput(NS(id, "mod_table"))
  )
}