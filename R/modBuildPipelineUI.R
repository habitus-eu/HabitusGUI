#' modBuildPipelineUI
#'
#' @param id ...
#' @return No object returned, this is a shiny module
#' @export


modBuildPipelineUI <- function(id) {
  tagList(
    titlePanel("Welcome to Habitus"),
    checkboxGroupInput(NS(id, "availabledata"), label = "Which type(s) of data would you like to analyse? ",
                       choiceNames = list("Raw acceleration", "ActiGraph counts", "GPS", "GIS"),
                       choiceValues = list("AccRaw", "ACount", "GPS", "GIS"), width = '100%'),
    
    # If there is AccRaw or ACount data then show second text box that asks user about research goals
    conditionalPanel(condition = paste0("input.availabledata.indexOf(`AccRaw`) > -1  || ",
                                        "input.availabledata.indexOf(`ACount`) > -1"),
                     checkboxGroupInput(NS(id, "researchgoals"), label = "", 
                                        choiceNames = "", choiceValues = "", width = '100%')
    ), 
    # Show possible pipelines:
    textOutput(NS(id, "pipeline"))
  )
}