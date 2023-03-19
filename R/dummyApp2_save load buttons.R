library(shiny)

ui = fluidPage(
  column(3,
         textInput("text", "text", ""),
         selectInput("select", "select", 1:5),
         uiOutput("ui"),
         fileInput("file1", "Choose CSV File", accept = ".csv"),
         actionButton("save", "Save"),
         actionButton("load", "Load")
  ),
  column(9,
         tableOutput("table")
  )
)

server = function(input, output, session) {
  req()
  output$ui <- renderUI({
    tagList(
      numericInput("num", "num", 7),
      checkboxGroupInput("chk", "chk", 1:5, c(2,4))
    )
  })
  
  output$table <- renderTable({
    req(input$file1)
    df <- read.csv(input$file1$datapath)
    return(df)
  })
  
  # save file if uploaded
  observe({
    if (is.null(input$file1)) return()
    if (!dir.exists("previous_config/")) dir.create("previous_config/")
    file.copy(input$file1$datapath, "./previous_config/configfile.csv")
  })
  
  observeEvent(input$save, {
    values <<- lapply(reactiveValuesToList(input), unclass)
  })
  
  observeEvent(input$load, {
    if (exists("values")) {
      lapply(names(values),
             function(x) session$sendInputMessage(x, list(value = values[[x]]))
      )
    }
    if (dir.exists("previous_config/")) {
      if (length(dir("previous_config/")) == 1) {
        output$table <- renderTable({
          df <- read.csv(dir("previous_config/", full.names = TRUE))
          return(df)
        })
      }
    }
  })
}

shinyApp(ui, server)
