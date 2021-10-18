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
      type = "hidden",
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
               # Select input folder accelerometer data -----------------------------------
               fluidRow(
                 column(12,
                        # tags$h5(strong("Select folder that has the accelerometer data:")),
                        shinyFiles::shinyDirButton("accdir", label = "Accelerometer data directory...", title = "Select folder with accelerometer data"),
                        verbatimTextOutput("accdir", placeholder = TRUE),
                 )
               ),
               # Select input folder gps data -----------------------------------
               conditionalPanel(condition = "input.tool==`PALMSpy`",
                                fluidRow(
                                  column(12,
                                         # tags$h5(strong("Select folder that has the accelerometer data:")),
                                         shinyFiles::shinyDirButton("gpsdir", label = "GPS data directory...",
                                                                    title = "Select folder with GPS data"),
                                         verbatimTextOutput("gpsdir", placeholder = TRUE),
                                  )
                                ),
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
               textOutput("naccfiles"),
               textOutput("ngpsfiles"),
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
    shinyDirChoose(input, 'accdir',  roots = c(home = homedir))
    shinyDirChoose(input, 'gpsdir',  roots = c(home = homedir))
    shinyDirChoose(input, 'outputdir',  roots = c(home = homedir))
    
    # Capture provided directories in reactive object ----------------------------
    accdir <- reactive(input$accdir)
    gpsdir <- reactive(input$gpsdir)
    outputdir <- reactive(input$outputdir)
    sleepdiaryfile <- reactive(input$sleepdiaryfile$datapath)
    configfile <- reactive(input$configfile$datapath)
    # Create global with directories and give it default values -------
    global <- reactiveValues(data_in = homedir, data_out = homedir, desiredtz=Sys.timezone)
    
    # Update global when input changes
    observeEvent(ignoreNULL = TRUE,
                 eventExpr = {
                   input$accdir # every time input$accdir updates ...
                 },
                 handlerExpr = { # ... we re-assign global$acc_in
                   if (!"path" %in% names(accdir())) return()
                   home <- normalizePath(homedir)
                   global$acc_in <-
                     file.path(home, paste(unlist(accdir()$path[-1]), collapse = .Platform$file.sep))
                 })
    observeEvent(ignoreNULL = TRUE,
                 eventExpr = {
                   input$gpsdir # every time input$gpsdir updates ...
                 },
                 handlerExpr = { # ... we re-assign global$gps_in
                   if (!"path" %in% names(gpsdir())) return()
                   home <- normalizePath(homedir)
                   global$gps_in <-
                     file.path(home, paste(unlist(gpsdir()$path[-1]), collapse = .Platform$file.sep))
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
    output$accdir <- renderText({
      global$acc_in
    })
    output$gpsdir <- renderText({
      global$gps_in
    })
    output$outputdir <- renderText({
      global$data_out
    })
    
    # Count files in input acc directory and send to UI ------------------------------
    acc_file_count <- reactive({
      timer()
      length(grep(pattern = "[.]csv|[.]cwa|[.]bin", x = dir(global$acc_in)))
    })
    output$naccfiles <- renderText({
      paste0("There are ", acc_file_count(), " .csv files in the acc data folder")
    })
    gps_file_count <- reactive({
      timer()
      length(grep(pattern = "[.]csv", x = dir(global$gps_in)))
    })
    output$ngpsfiles <- renderText({
      paste0("There are ", gps_file_count(), " .csv files in the gps data folder")
    })
    
    # Count files in output directory and send to UI ------------------------------
    x3 <- reactive({
      timer()
      length(grep(pattern = "[.]csv", x = dir(global$data_out)))
    })
    output$nfilesout <- renderText({
      paste0("There are ", x3(), " data files in the output folder")
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
    simdata <- eventReactive(input$simdata, {
      print("Creating test files...")
      CountFiles = function(path) {
        return(length(dir(path = path, full.names = FALSE)))
      }
      Nbefore = CountFiles(path = global$acc_in)
      create_test_files(dir = global$acc_in, Nfiles = 10, Nobs = 10)
      Nafter = CountFiles(path = global$acc_in)
      test = Nafter > Nbefore
      return(test)
    })
    # Update message on whether simulated files were created ---------------------
    output$sim_message <- renderText({
      message = ifelse(simdata() == TRUE,
                       yes = paste0("New files created ",Sys.time()),
                       no = paste0("No files created ",Sys.time()))
    })
    # Load config file and check desiredtz ---------------------------------------
    extract_tz <- eventReactive(input$page_12, {
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
      message = ifelse(is.null(configfile()) == FALSE,
                       yes = paste0("Timezone in configuration file: ", extract_tz()),
                       no = paste0("Default system timezone: ", Sys.timezone()))
    })
    
    # Update timezone in config file or provide timezone to analys step ------------
    update_tz <- eventReactive(input$update_timezone, {
      tz_in_file = FALSE
      if (is.null(configfile()) == FALSE) { # if configile exists
        updateGGIRconfig(configfile(), new_desiredtz=global$desiredtz)
        tz_in_file = TRUE
      }
      return(tz_in_file)
    })
    
    # If analyse-button pressed send message to UI about success ----------------
    output$tzupdate_message <- renderText({
      message = ifelse(update_tz() == TRUE,
                       yes = paste0("Tz update succesful ",Sys.time()),
                       no =  paste0("Tz update unsuccesful ",Sys.time()))
    })
    
    # Apply tool after analyse-button is pressed ---------------------------------
    runpipeline <- eventReactive(input$analyse, {
      print("Running analysis...")
      if (input$tool == "myRTool") {
        if (is.null(configfile())) { # no configfile specified
          myRTool(accdir = global$acc_in, outputdir = global$data_out, desiredtz = global$desiredtz)
        } else { # config file specified and possible updated
          myRTool(accdir = global$acc_in, outputdir = global$data_out, config = configfile())
        }
        test = file.exists(paste0(global$data_out,"/results.csv"))
      }
      if (input$tool == "myPyTool") {
        myPyTool(accdir = global$acc_in, outputdir = global$data_out, sleepdiary = sleepdiaryfile())
        test = file.exists(paste0(global$data_out,"/testpython.csv"))
      }
      if (input$tool == "PALMSpy") {
        PALMSpy_R(gps_path = global$gps_in, acc_path = global$acc_in,
                  output_path = global$data_out, config_file = configfile(), parameters = c())
        test = file.exists(paste0(global$data_out,"/testpython.csv"))
      }
      if (input$tool == "GGIR") {
        if (is.null(configfile())) { # no configfile specified
          GGIRshiny(accdir = global$acc_in, outputdir = global$data_out, 
                    sleepdiary = sleepdiaryfile(), desiredtz = global$desiredtz)
        } else { # config file specified and optionally updated by user
          if (!is.null(sleepdiaryfile())) {
            GGIRshiny(accdir = global$acc_in, outputdir = global$data_out, configfile = configfile(), 
                      sleepdiary = sleepdiaryfile())
          } else {
            GGIRshiny(accdir = global$acc_in, outputdir = global$data_out, configfile = configfile())
          }
        }
        expected_output_file = paste0(global$data_out, "/output_", basename(global$acc_in), "/results/part2_summary.csv")
        test = file.exists(expected_output_file)
      }
      return(test)
    })
    
    # If analyse-button pressed send message to UI about success ----------------
    output$analyse_message <- renderText({
      message = ifelse(runpipeline() == TRUE,
                       yes = paste0("Procesing succesful ",Sys.time()),
                       no = paste0("Procesing unsuccesful ",Sys.time()))
    })
  }
  
  # Run the application 
  shinyApp(ui, server)
}