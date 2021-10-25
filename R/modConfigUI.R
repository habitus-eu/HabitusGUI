#' modConfigUI
#'
#' @param id ...
#' @return No object returned, this is a shiny module
#' @export


modConfigUI <- function(id) {
  tagList(
    fileInput("configfile", NULL, label = "(optional)", width = '100%',
              accept = ".json", multiple = FALSE,
              buttonLabel = "Configuration file ..."),
    actionButton("reset", "Reset"),
    actionButton("save", "Save"),
    tags$hr(),
    DT::dataTableOutput(NS(id, "mod_table"))
  )
}