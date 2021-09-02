#' myApp
#'
#' @param homedir character to specify home directory
#' @param ... No input needed, function runs the app
#' @return no object is returned, just an app
#' @import shiny
#' @import shinyFiles
#' @import bslib
#' @export

# pkgload::load_all("."); HabitusGUI::myApp(homedir="~/projects/fontys")
# pkgload::load_all("."); myApp(homedir="~/projects/fontys")
# roxygen2::roxygenise()

myApp <- function(homedir=getwd(), ...) {
  ONames <- OlsonNames()
  ONames <- ONames[c(which(ONames == Sys.timezone()), which(ONames != Sys.timezone()))]
  ui <- fluidPage(
    theme = bslib::bs_theme(bootswatch = NULL),
    tabsetPanel(
      id = "wizard",
      type= "hidden",
      
      tabPanel("page_1",
               titlePanel("Welcome to Habitus"),
               
               checkboxGroupInput("availabledata", label = "Which type(s) of data would you like to analyse? ", 
                                  choiceNames = list("Raw acceleration", "ActiGraph counts", "GPS", "GIS"),
                                  choiceValues = list("AccRaw", "ACount", "GPS", "GIS"), width = '100%'),
               # If there is AccRaw or ACount data then show second text box that asks user about research goals
               conditionalPanel(condition = paste0("input.availabledata.indexOf(`AccRaw`) > -1  || ",
                                                   "input.availabledata.indexOf(`ACount`) > -1"),
                                checkboxGroupInput("researchgoals", label = "", 
                                                   choiceNames = "", choiceValues = "", width = '100%')
                                ), 
               # Show possible pipelines:
               textOutput("pipeline"),
               
               hr(),
               actionButton("page_12", "next")
      ),
      tabPanel("page_2",
               titlePanel("Data selection"),
               # Select tool -----------------------------------------
               fluidRow(
                 column(6,
                        selectInput("tool", label = "Select processing tool: ", 
                                    choices=c("GGIR", "myRTool", "myPyTool", "PALMSpy", "PALMSplus"))
                 )
               ),
               # Select input folder -----------------------------------
               fluidRow(
                 column(12,
                        # tags$h5(strong("Select folder that has the accelerometer data:")),
                        shinyFiles::shinyDirButton("inputdir", label = "Accelerometer data directory...", title = "Select folder with accelerometer data"),
                        verbatimTextOutput("inputdir", placeholder = TRUE),
                 )
               ),
               # Specify output directory ----------------------------------------------
               fluidRow(
                 column(12,
                        # tags$h5(strong("Select folder where output should be stored:")),
                        shinyFiles::shinyDirButton("outputdir", "Output directory...", "Select folder where output should be stored"),
                        verbatimTextOutput("outputdir", placeholder = TRUE),

                 )
               ),
               # Option create dummy files in input directory ---------------------------
               conditionalPanel(condition = "input.tool==`myRTool` || input.tool==`myPyTool`",
                                # tags$h5(strong("Create dummy file?")),
                                actionButton("simdata", "Create dummy files for testing the app", class="btn-danger"),
                                textOutput("sim_message"),
                                headerPanel(""),
               ),
               # Upload configuration file -----------------------------------------------
               conditionalPanel(condition = "input.tool==`myRTool` || input.tool==`GGIR`",
                                div(fileInput("configfile", label="(optional)", buttonLabel = "Configuration file..."), 
                                    style="font-size:80%"
                                ), #"Upload configuration file"
                                # textOutput("configext"),
               ),
               # Upload sleep diary ----------------------------------------------------
               conditionalPanel(condition = "input.tool==`myPyTool` || input.tool==`GGIR`",
                                div(fileInput("sleepdiaryfile", label="(optional)", buttonLabel = "Sleep diary file..."),
                                    style="font-size:80%"
                                ), #"Upload sleepdiary file"
                                # textOutput("sleepdiaryext")
               ),
               hr(),
               actionButton("page_21", "prev"),
               actionButton("page_23", "next")
      ),
      tabPanel("page_3",
               titlePanel("Check and update configuration"),
               headerPanel(""),
               textOutput("nfilesin"),
               textOutput("nfilesout"),
               headerPanel(""),
               conditionalPanel(condition = "input.tool==`myRTool` || input.tool==`GGIR`",
                                # Show current timezone in configuration file -------------------
                                textOutput("tz_message"),
                                # Ask user whether to update timezone?
                                checkboxInput("select_timezone", "Change timezone?", value=FALSE),
                                # If yes, show option select button
                                conditionalPanel(condition = "input.select_timezone == 1",
                                                 # Select timezone -----------------------------------------
                                                 shiny::selectInput("timezone", 
                                                                    label = "Select or type the timezone where the data was collected: ", 
                                                                    choices=ONames),
                                                 conditionalPanel(condition = "output.config_file_ready",
                                                                  actionButton("update_timezone", "Update timezone in configuration file?"),
                                                                  textOutput("tzupdate_message")
                                                 )
                                )
               ),
               hr(),
               actionButton("page_32", "prev"),
               actionButton("page_34", "next")
      ),
      tabPanel("page_4",
               # Button to start analysis ---------------------------------------------
               
               titlePanel("Analysis"),
               actionButton("analyse", "Run analysis"),
               textOutput("analyse_message"),
               headerPanel(""),
               hr(),
               actionButton("page_43", "prev")
      )
    )
  )
  
  server <- function(input, output, session) {
    switch_page <- function(i) {
      updateTabsetPanel(inputId = "wizard",
                        selected = paste0("page_", i))
    }
    observeEvent(input$page_12, switch_page(2))
    observeEvent(input$page_21, switch_page(1))
    observeEvent(input$page_23, switch_page(3))
    observeEvent(input$page_32, switch_page(2))
    observeEvent(input$page_34, switch_page(4))
    observeEvent(input$page_43, switch_page(3))
    
    # Defined time to ensure file count is only checked twice per second ---------
    timer = reactiveTimer(500) 
    
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
      ifelse(length(x123()) == 0, yes="Select data types and research interest above.", no=message)
    })
    
    # Extract directories ---------------
    shinyDirChoose(input, 'inputdir',  roots = c(home = homedir))
    shinyDirChoose(input, 'outputdir',  roots = c(home = homedir))
    
    # Capture provided directories in reactive object ----------------------------
    inputdir <- reactive(input$inputdir)
    outputdir <- reactive(input$outputdir)
    sleepdiaryfile <- reactive(input$sleepdiaryfile$datapath)
    configfile <- reactive(input$configfile$datapath)
    # Create global with directories and give it default values -------
    global <- reactiveValues(data_in = homedir, data_out = homedir, desiredtz=Sys.timezone)
    
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
    observeEvent(ignoreNULL = TRUE,
                 eventExpr = {
                   input$timezone # every time input$timezone updates ...
                 },
                 handlerExpr = { # ... we re-assign global$desiredtz
                   global$desiredtz <-input$timezone
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
      length(grep(pattern = "[.]csv|[.]cwa|[.]bin", x = dir(global$data_in)))
    })
    output$nfilesin <- renderText({
      paste0("There are ",x1()," .csv files in the data folder")
    })
    # Count files in output directory and send to UI ------------------------------
    x3 <- reactive({
      timer()
      length(grep(pattern = "[.]csv", x = dir(global$data_out)))
    })
    output$nfilesout <- renderText({
      paste0("There are ",x3()," data files in the output folder")
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
      print("Creating test files...")
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
    # Load config file and check desiredtz ---------------------------------------
    x5 <- eventReactive(input$page_12, {
      desiredtz = checkGGIRconfig(configfile())
      return(desiredtz)
    })
    
    # Check whether configuration file was uploaded, because this defines whether 
    # the configfile update button should be visible -----------------------------
    output$config_file_ready <- reactive({
      return(!is.null(input$configfile))
    })
    outputOptions(output, "config_file_ready", suspendWhenHidden = FALSE)
    # Show current desiredtz -----------------------------------------------------
    output$tz_message <- renderText({
      if (is.null(configfile()) == FALSE) {
        message = paste0("Timezone in configuration file: ", x5())
      } else {
        message = paste0("Default system timezone: ", Sys.timezone())
      }
    })
    
    # Update timezone in config file or provide timezone to analys step ------------
    x6 <- eventReactive(input$update_timezone, {
      if (is.null(configfile()) == FALSE) { # if configile exists
        updateGGIRconfig(configfile(), new_desiredtz=global$desiredtz)
        tz_in_file = TRUE
      } else {# if configfile does not exists
        # create desiredtz object and give it to analyse
        tz_in_file = FALSE
      }
      return(tz_in_file)
    })
    
    
    # If analyse-button pressed send message to UI about success ----------------
    output$tzupdate_message <- renderText({
      if (x6() == TRUE) {
        message = paste0("Tz update succesful ",Sys.time())
      } else if (x6() == FALSE) {
        message = paste0("Tz update unsuccesful ",Sys.time())
      }
    })
    
    # Apply tool after analyse-button is pressed ---------------------------------
    x2 <- eventReactive(input$analyse, {
      print("Running analysis...")
      if (input$tool == "myRTool") {
        if (is.null(configfile())) { # no configfile specified
          myRTool(inputdir = global$data_in, outputdir=global$data_out, desiredtz=global$desiredtz)
        } else { # config file specified and possible updated
          myRTool(inputdir = global$data_in, outputdir=global$data_out, config=configfile())
        }
        test = file.exists(paste0(global$data_out,"/results.csv"))
      }
      if (input$tool == "myPyTool") {
        myPyTool(inputdir = global$data_in, outputdir=global$data_out, sleepdiary=sleepdiaryfile())
        test = file.exists(paste0(global$data_out,"/testpython.csv"))
      }
      if (input$tool == "GGIR") {
        if (is.null(configfile())) { # no configfile specified
          GGIRshiny(inputdir = global$data_in, outputdir=global$data_out, 
                    sleepdiary=sleepdiaryfile(), desiredtz=global$desiredtz)
        } else { # config file specified and optionally updated by user
          if (!is.null(sleepdiaryfile())) {
            GGIRshiny(inputdir = global$data_in, outputdir=global$data_out, configfile=configfile(), 
                      sleepdiary=sleepdiaryfile())
          } else {
            GGIRshiny(inputdir = global$data_in, outputdir=global$data_out, configfile=configfile())
          }
        }
        expected_output_file = paste0(global$data_out,"/output_",basename(global$data_in),"/results/part2_summary.csv")
        test = file.exists(expected_output_file)
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