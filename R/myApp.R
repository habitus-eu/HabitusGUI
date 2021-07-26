#' myApp
#'
#' @param ... No input needed, function runs the app
#' @return no object is returned, just an app
#' @export

# library(shiny)
# library(shinyFiles)

# pkgload::load_all("."); HabitusGUI::myApp()
# myApp()
# OR AFTER BUILDING IT
# HabitusGUI::myApp()
# roxygen2::roxygenise()
# create_test_files(dir="~/projects/fontys/testfolder", Nfiles=10, Nobs = 10)
# mytool(inputdir="~/projects/fontys/testfolder", outputdir="~/projects/fontys", config=c())
# Old namespace file content: export(myApp)

myApp <- function(...) {
  options(shiny.launch.browser = .rs.invokeShinyWindowExternal)  
  ui <- fluidPage( # Application title
    titlePanel("HabitusGUI"),
    fluidRow(
      column(6,
             shiny::selectInput("tool", label = "Select processing tool: ", choices=c("myRTool", "myPyTool"))
      )
    ),
    fluidRow(
      column(6,
        tags$h5(strong("Select folder with data to be processed:")),
        shinyDirButton("inputdir", label = "Input directory", title = "Select folder with data to be processed"),
        verbatimTextOutput("inputdir", placeholder = TRUE),
        textOutput("nfilesin"),
      )
    ),
    headerPanel(""),
    fluidRow(
      column(6,
             tags$h5(strong("Select folder where output should be stored:")),
             shinyDirButton("outputdir", "Output directory", "Select folder where output should be stored"),
             verbatimTextOutput("outputdir", placeholder = TRUE),
             textOutput("nfilesout"),
      )
    ),
    headerPanel(""),
    fluidRow(
      column(6,
             tags$h5(strong("Ready to analyse data?")),
             actionButton("analyse", "Analyse data"),
             textOutput("result")
      )
    )
  )
  server <- function(input, output) {
    timer = reactiveTimer(500)
    shinyDirChoose(input, 'inputdir',  roots = c(home = '~/projects/fontys'))
    shinyDirChoose(input, 'outputdir',  roots = c(home = '~/projects/fontys'))
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
                   home <- normalizePath("~/projects/fontys")
                   global$data_in <-
                     file.path(home, paste(unlist(inputdir()$path[-1]), collapse = .Platform$file.sep))
                 })
    
    observeEvent(ignoreNULL = TRUE,
                 eventExpr = {
                   input$outputdir
                 },
                 handlerExpr = {
                   if (!"path" %in% names(outputdir())) return()
                   home <- normalizePath("~/projects/fontys")
                   global$data_out <-
                     file.path(home, paste(unlist(outputdir()$path[-1]), collapse = .Platform$file.sep))
                 })
    
    x1 <- reactive({
      timer()
      length(grep(pattern = "[.]csv", x = dir(global$data_in)))
    })
    
    output$nfilesin <- renderText({
      paste0("There are ",x1()," .csv files in this folder")
    })
    
    x3 <- reactive({
      timer()
      length(grep(pattern = "[.]csv", x = dir(global$data_out)))
    })
    output$nfilesout <- renderText({
      paste0("There are ",x3()," .csv files in this folder")
    })
    
    x2 <- eventReactive(input$analyse, {
      if (input$tool == "myRTool") {
        myRTool(inputdir = global$data_in, outputdir=global$data_out)
        test = file.exists(paste0(global$data_out,"/results.csv"))
      }
      if (input$tool == "myPyTool") {
        myPyTool(inputdir = global$data_in, outputdir=global$data_out)
        test = file.exists(paste0(global$data_out,"/testpython.csv"))
      }
      return(test)
    })
    output$result <- renderText({
      if (x2() == TRUE) {
        message = paste0("Procesing succesful ",Sys.time())
      } else if (x2() == FALSE) {
        message = paste0("Procesing unsuccesful ",Sys.time())
      }
    })
  }
  
  
  # Run the application 
  shinyApp(ui, server)
}