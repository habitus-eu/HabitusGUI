#' modBuildPipelineServer
#'
#' @param id ...
#' @return List of pipeline, availabledata and research goals
#' @export

modBuildPipelineServer = function(id) {
  
  moduleServer(id, function(input, output, session) {
    # Update checkbox possible research goals depending on available data
    observe({
      x <- input$availabledata
      
      # Can use character(0) to remove all choices
      if (is.null(x)) x <- character(0)
      researchgoals = c()
      if ("GPS" %in% x & any(c("AccRaw", "ACount") %in% x)) researchgoals = c(researchgoals, "Trips", "QC")
      if (all(c("GPS", "GIS") %in% x) & any(c("AccRaw", "ACount") %in% x)) researchgoals = c(researchgoals, "Environment", "QC")
      if ("AccRaw" %in% x | all(c("AccCount", "GPS")  %in% x)) researchgoals = c(researchgoals, "PA", "QC")
      if ("AccRaw" %in% x) researchgoals = c(researchgoals, "Sleep", "QC")
      reasearchgoalsNames = c("Data quality assessment", "Physical Activity",
                              "Sleep", "Trips", "Behaviour environment relation")
      reasearchgoalsValues = c("QC", "PA", "Sleep", "Trips", "Environment")
      
      if (length(researchgoals) == 0) {
        researchgoalsLabel = ""
        reasearchgoalsValues = researchgoalsNames = c()
      } else {
        researchgoalsNames =  reasearchgoalsNames[which(reasearchgoalsValues %in% researchgoals == TRUE)]
        reasearchgoalsValues =  reasearchgoalsValues[which(reasearchgoalsValues %in% researchgoals == TRUE)]
        researchgoalsLabel = "What is you research interest?"
      }
      # Update checkbox
      updateCheckboxGroupInput(session, "researchgoals",
                               label = researchgoalsLabel,
                               choiceNames = researchgoalsNames,
                               choiceValues = reasearchgoalsValues,
                               selected = input$researchgoals)
    })
    
     # Identify pipeline with tools to be used and send to UI
    x123 <- reactive(identify_tools(datatypes = input$availabledata, goals = input$researchgoals)$tools_needed)
    output$pipeline <- renderText({
      message = paste0("Proposed software pipeline: ",paste0(x123(), collapse = " + "))
      ifelse(length(x123()) == 0, yes = "Select data types and research interest above.", no = message)
    })
    x123
  })
}
