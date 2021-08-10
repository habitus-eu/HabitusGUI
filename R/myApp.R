#' myApp
#'
#'@param homedir character to specify home directory
#' @param ... No input needed, function runs the app
#' @return no object is returned, just an app
#' @import shiny
#' @import shinyFiles
#' @export

# library(shiny)
# library(shinyFiles)
# pkgload::load_all("."); HabitusGUI::myApp(homedir="~/projects/fontys")
# pkgload::load_all("."); myApp(homedir="~/projects/fontys")
# roxygen2::roxygenise()
# Old namespace file content: export(myApp)

myApp <- function(homedir=getwd(), ...) {
  ui <- fluidPage(
    tabsetPanel(
      id = "wizard",
      type= "hidden",
      tabPanel("page_1",
               titlePanel("HabitusGUI"),
               # Select tool -----------------------------------------
               fluidRow(
                 column(6,
                        shiny::selectInput("tool", label = "Select processing tool: ", choices=c("myRTool", "myPyTool"))
                 )
               ),
               # Select input folder -----------------------------------
               fluidRow(
                 column(6,
                        tags$h5(strong("Select folder with data to be processed:")),
                        shinyDirButton("inputdir", label = "Input directory", title = "Select folder with data to be processed"),
                        verbatimTextOutput("inputdir", placeholder = TRUE),
                        textOutput("nfilesin"),
                 )
               ),
               headerPanel(""),
               # Option create dummy files in input directory ---------------------------
               conditionalPanel(condition = "input.tool==`myRTool` || input.tool==`myPyTool`",
                                tags$h5(strong("Create dummy file?")),
                                actionButton("simdata", "Create dummy files"),
                                textOutput("sim_message"),
               ),
               headerPanel(""),
               # Upload configuration file -----------------------------------------------
               conditionalPanel(condition = "input.tool==`myRTool` || input.tool==`myPyTool`",
                                fileInput("configfile", "Upload configuration file"),
                                textOutput("configext"),
               ),
               # Upload sleep diary ----------------------------------------------------
               conditionalPanel(condition = "input.tool==`myPyTool`",
                                fileInput("sleepdiaryfile", "Upload sleepdiary file"),
                                textOutput("sleepdiaryext")
               ),
               # Specify output directory ----------------------------------------------
               fluidRow(
                 column(6,
                        tags$h5(strong("Select folder where output should be stored:")),
                        shinyDirButton("outputdir", "Output directory", "Select folder where output should be stored"),
                        verbatimTextOutput("outputdir", placeholder = TRUE),
                        textOutput("nfilesout"),
                 )
               ),
               actionButton("page_12", "next")
      ),
      tabPanel("page_2",
               titlePanel("Configuration check"),
               actionButton("page_21", "prev"),
               actionButton("page_23", "next")
      ),
      tabPanel("page_3",
               # Button to start analysis ---------------------------------------------
               titlePanel("Analyse"),
               tags$h5(strong("Ready to analyse data?")),
               actionButton("analyse", "Analyse data"),
               textOutput("analyse_message"),
               headerPanel(""),
               actionButton("page_32", "prev")
      )
    )
  )
  
  server <- function(input, output) {
    switch_page <- function(i) {
      updateTabsetPanel(inputId = "wizard",
                        selected = paste0("page_", i))
    }
    observeEvent(input$page_12, switch_page(2))
    observeEvent(input$page_21, switch_page(1))
    observeEvent(input$page_23, switch_page(3))
    observeEvent(input$page_32, switch_page(2))
    
    # Defined time to ensure file count is only checked twice per second ---------
    timer = reactiveTimer(500) 
    # Extract directories ---------------
    shinyDirChoose(input, 'inputdir',  roots = c(home = homedir))
    shinyDirChoose(input, 'outputdir',  roots = c(home = homedir))
    
    # Capture provided directories in reactive object ----------------------------
    inputdir <- reactive(input$inputdir)
    outputdir <- reactive(input$outputdir)
    sleepdiaryfile <- reactive(input$sleepdiaryfile$datapath)
    configfile <- reactive(input$configfile$datapath)
    # Create global with directories and give it default values -------
    global <- reactiveValues(data_in = homedir, data_out = homedir)
    
    # Update global when input changes
    observeEvent(ignoreNULL = TRUE,
                 eventExpr = {
                   input$inputdir # every time input$inputdir updates ...
                 },
                 handlerExpr = { # ... we re-assign global$data_in
                   if (!"path" %in% names(inputdir())) return()
                   home <- normalizePath(homedir)
                   global$data_in <-
                     file.path(home, paste(unlist(inputdir()$path[-1]), collapse = .Platform$file.sep))
                 })
    
    observeEvent(ignoreNULL = TRUE,
                 eventExpr = {
                   input$outputdir # every time input$outputdir updates ...
                 },
                 handlerExpr = { # ... we re-assign global$data_out
                   if (!"path" %in% names(outputdir())) return()
                   home <- normalizePath(homedir)
                   global$data_out <-
                     file.path(home, paste(unlist(outputdir()$path[-1]), collapse = .Platform$file.sep))
                 })
    # Send directories to UI --------------------------------------------
    output$inputdir <- renderText({
      global$data_in
    })
    output$outputdir <- renderText({
      global$data_out
    })
    
    # Count files in input directory and send to UI ------------------------------
    x1 <- reactive({
      timer()
      length(grep(pattern = "[.]csv", x = dir(global$data_in)))
    })
    output$nfilesin <- renderText({
      paste0("There are ",x1()," .csv files in this folder")
    })
    # Count files in output directory and send to UI ------------------------------
    x3 <- reactive({
      timer()
      length(grep(pattern = "[.]csv", x = dir(global$data_out)))
    })
    output$nfilesout <- renderText({
      paste0("There are ",x3()," .csv files in this folder")
    })
    # Extract file extension of configuration file and send to UI ----------------
    configdata <- reactive({
      req(input$configfile)
      ext <- tools::file_ext(input$configfile$name)
    })
    output$configext <- renderText({
      configdata()
    })
    # Extract file extension of sleep diary file and send to UI ------------------
    sleepdiarydata <- reactive({
      req(input$sleepdiaryfile)
      ext <- tools::file_ext(input$sleepdiaryfile$name)
    })
    output$sleepdiaryext <- renderText({
      sleepdiarydata()
    })
    # Create simulated data files after button is pressed ------------------------
    x4 <- eventReactive(input$simdata, {
      print("simulatedata")
      Nbefore = length(dir(path = global$data_in, full.names = FALSE))
      create_test_files(dir = global$data_in, Nfiles = 10, Nobs = 10)
      Nafter = length(dir(path = global$data_in, full.names = FALSE))
      test = Nafter > Nbefore
      return(test)
    })
    # Update message on whether simulated files were created ---------------------
    output$sim_message <- renderText({
      if (x4() == TRUE) {
        message = paste0("New files created ",Sys.time())
      } else if (x4() == FALSE) {
        message = paste0("No files created ",Sys.time())
      }
    })
    # Apply tool after analyse-button is pressed ---------------------------------
    x2 <- eventReactive(input$analyse, {
      print("analyse")
      if (input$tool == "myRTool") {
        myRTool(inputdir = global$data_in, outputdir=global$data_out, config=configfile())
        test = file.exists(paste0(global$data_out,"/results.csv"))
      }
      if (input$tool == "myPyTool") {
        myPyTool(inputdir = global$data_in, outputdir=global$data_out, sleepdiary=sleepdiaryfile())
        test = file.exists(paste0(global$data_out,"/testpython.csv"))
      }
      return(test)
    })
    # If analyse-button pressed send message to UI about success ----------------
    output$analyse_message <- renderText({
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