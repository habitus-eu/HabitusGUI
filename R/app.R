library(shiny)
library(shinyFiles)

# pkgload::load_all(".")
# myApp()
# OR AFTER BUILDING IT
# HabitusGUI::myApp()
# create_test_files(dir="~/projects/fontys/testfolder", Nfiles=10, Nobs = 10)
# mytool(inputdir="~/projects/fontys/testfolder", outputdir="~/projects/fontys", config=c())

myApp <- function(...) {
  ui <- fluidPage( # Application title
    mainPanel(
      shinyDirButton("inputdir", "Input directory", "Select folder with data to be processed"),
      verbatimTextOutput("inputdir", placeholder = TRUE),
      shinyDirButton("outputdir", "Output directory", "Select folder where output should be stored"),
      verbatimTextOutput("outputdir", placeholder = TRUE),
      actionButton("analyse", "Analyse data"),
      textOutput("nfiles"),
      textOutput("result")
    ))
  
  server <- function(input, output) {
    shinyDirChoose(input, 'inputdir',  roots = c(home = '~'))
    shinyDirChoose(input, 'outputdir',  roots = c(home = '~'))
    global <- reactiveValues(data_in = getwd(), data_out = getwd())
    
    inputdir <- reactive(input$inputdir)
    outputdir <- reactive(input$outputdir)
    
    output$inputdir <- renderText({
      global$data_in
    })
    output$outputdir <- renderText({
      global$data_out
    })
    
    observeEvent(ignoreNULL = TRUE,
                 eventExpr = {
                   input$inputdir
                 },
                 handlerExpr = {
                   if (!"path" %in% names(inputdir())) return()
                   home <- normalizePath("~")
                   global$data_in <-
                     file.path(home, paste(unlist(inputdir()$path[-1]), collapse = .Platform$file.sep))
                 })
    
    observeEvent(ignoreNULL = TRUE,
                 eventExpr = {
                   input$outputdir
                 },
                 handlerExpr = {
                   if (!"path" %in% names(outputdir())) return()
                   home <- normalizePath("~")
                   global$data_out <-
                     file.path(home, paste(unlist(outputdir()$path[-1]), collapse = .Platform$file.sep))
                 })
    
    x1 <- reactive(length(dir(global$data_in)))
    output$nfiles <- renderText({
      paste0("There are ",x1()," files in the input folder")
    })
    
    x2 <- eventReactive(input$analyse, {
      mytool(inputdir = global$data_in, outputdir=global$data_out)
      file.exists(paste0(global$data_out,"/results.csv"))
    })
   
    output$result <- renderText({
      if (x2() == TRUE) {
        message = paste0("Procesing succesful")
      } else if (x2() == FALSE) {
        message = paste0("Procesing unsuccesful")
      }
    })
  }
  
  
  # Run the application 
  shinyApp(ui, server)
}