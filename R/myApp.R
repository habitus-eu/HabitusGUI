#' myApp
#'
#' @param homedir character to specify home directory
#' @param ... No input needed, function runs the app
#' @return no object is returned, just an app
#' @import shiny
#' @import shinyFiles
#' @import bslib
#' @import waiter
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
               # modBuildPipelineUI("build_pipeline"),
               titlePanel("Welcome to Habitus"),
               checkboxGroupInput("availabledata", label = "Which type(s) of data would you like to analyse? ",
                                  choiceNames = list("Raw acceleration", "ActiGraph counts", "GPS", "GIS", "Sleep Diary"),
                                  choiceValues = list("AccRaw", "ACount", "GPS", "GIS", "SleepDiary"), width = '100%'),
               
               # If there is AccRaw or ACount data then show second text box that asks user about research goals
               conditionalPanel(condition = paste0("input.availabledata.indexOf(`AccRaw`) > -1  || ",
                                                   "input.availabledata.indexOf(`ACount`) > -1"),
                                hr(),
                                checkboxGroupInput("researchgoals", label = "", 
                                                   choiceNames = "", choiceValues = "", width = '100%')
               ), 
               # Show possible pipelines:
               textOutput("pipeline"),
               hr(),
               checkboxGroupInput("tools", label = "Select the tools you would like to use.",
                                  choiceNames = list("GGIR (R package)",
                                                     "BrondCounts (R packages activityCounts + GGIR)",
                                                     "PALMSpy (Python library)", "PALMSplus (R package)"),
                                  choiceValues = list("GGIR", "BrondCounts", "PALMSpy", "PALMSplus"), width = '100%'),
               
               actionButton("page_12", "next")
      ),
      tabPanel("page_2",
               titlePanel("Data selection"),
               # Select input folder raw accelerometer data if raw data is available and GGIR is planned------------------
               conditionalPanel(condition = "input.availabledata.indexOf(`AccRaw`) > -1  && input.tools.includes(`GGIR`)",
                                shinyFiles::shinyDirButton("rawaccdir", label = "Raw accelerometry data directory...",
                                                           title = "Select folder with raw accelerometer data"),
                                verbatimTextOutput("rawaccdir", placeholder = TRUE)
               ),
               # Select input folder count accelerometer data if count data is available and PALMSpy is planned------------------
               # if not then count data will have to be estimated from the raw data, but we do not bother user
               # with questions where it should be stored
               conditionalPanel(condition = "input.availabledata.indexOf(`ACount`) > -1 && input.tools.includes(`PALMSpy`)",
                                shinyFiles::shinyDirButton("countaccdir", label = "Count accelerometry data directory...",
                                                           title = "Select folder with count accelerometer data"),
                                verbatimTextOutput("countaccdir", placeholder = TRUE)
               ),
               # Select input folder gps data -----------------------------------
               conditionalPanel(condition = "input.availabledata.indexOf(`GPS`) > -1 && input.tools.includes('PALMSpy')",
                                shinyFiles::shinyDirButton("gpsdir", label = "GPS data directory...",
                                                           title = "Select folder with GPS data"),
                                verbatimTextOutput("gpsdir", placeholder = TRUE)
               ),
               # Specify output directory ----------------------------------------------
               fluidRow(
                 column(12,
                        shinyFiles::shinyDirButton("outputdir", "Output directory...",
                                                   title = "Select folder where output should be stored"),
                        verbatimTextOutput("outputdir", placeholder = TRUE)
                 )
               ),
               # Upload sleep diary ----------------------------------------------------
               conditionalPanel(condition = "input.availabledata.indexOf(`SleepDiary`) > -1",
                                div(fileInput("sleepdiaryfile", label = "",
                                              buttonLabel = "Select sleep diary file...", width = '100%'),
                                    style = "font-size:80%")),
               hr(),
               actionButton("page_21", "prev"),
               actionButton("page_23", "next")
      ),
      tabPanel("page_3",
               titlePanel("Configuration"),
               conditionalPanel(condition = "input.tools.includes('GGIR')",
                                h2("GGIR"),
                                modConfigUI("edit_ggir_config"),
                                hr()
               ),
               conditionalPanel(condition = "input.tools.includes('BrondCounts')",
                                h2("BrondCounts"),
                                p("No parameters are needed for the BrondCounts"),
                                hr()
               ),
               conditionalPanel(condition = "input.tools.includes('PALMSpy')",
                                h2("PALMSpy"),
                                modConfigUI("edit_palmspy_config"),
                                hr()
               ),
               actionButton("page_32", "prev"),
               actionButton("page_34", "next")
      ),
      tabPanel("page_4",
               # Button to start analysis ---------------------------------------------
               titlePanel("Analysis"),
               hr(),
               conditionalPanel(condition = paste0("input.tools.includes('GGIR') || ",
                                "input.tools.includes('BrondCounts')"),
                                conditionalPanel(condition =
                                                   paste0("input.tools.indexOf(`GGIR`) > -1  && ",
                                                          "input.tools.indexOf(`BrondCounts`) > -1"), 
                                                 h3("GGIR and BrondCounts:")
                                ),
                                conditionalPanel(condition = "input.tools.indexOf(`BrondCounts`) == -1", 
                                                 h3("GGIR:")
                                ),
                                waiter::use_waiter(),
                                actionButton("start_ggir", "Start analysis", width = '300px'),
                                textOutput("ggir_end_message"),
                                hr()
               ),
               conditionalPanel(condition = "input.tools.includes('PALMSpy')",
                                h3("PALMSpy:"),
                                waiter::use_waiter(),
                                actionButton("start_palmspy", "Start analysis", width = '300px'),
                                textOutput("palmspy_end_message"),
                                hr()
               ),
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
    
    # Ask user questions about available and research interests
    # and use the answers to identify a suitable pipeline
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
    proposed_pipeline <- reactive(identify_tools(datatypes = input$availabledata, goals = input$researchgoals)$tools_needed)
    output$pipeline <- renderText({
      message = paste0("Proposed software pipeline: ",paste0(proposed_pipeline(), collapse = " + "))
      ifelse(length(proposed_pipeline()) == 0, yes = "Select data types and research interest above.", no = message)
    })
    
    # check whether GGIR is in the pipeline, and send to UI,
    # such that it can condition the UI on this.
    
    # Extract directories ---------------
    shinyDirChoose(input, 'rawaccdir',  roots = c(home = homedir))
    shinyDirChoose(input, 'countaccdir',  roots = c(home = homedir))
    shinyDirChoose(input, 'gpsdir',  roots = c(home = homedir))
    shinyDirChoose(input, 'outputdir',  roots = c(home = homedir))
    
    # Capture provided directories in reactive object ----------------------------
    rawaccdir <- reactive(input$rawaccdir)
    countaccdir <- reactive(input$countaccdir)
    gpsdir <- reactive(input$gpsdir)
    outputdir <- reactive(input$outputdir)
    sleepdiaryfile <- reactive(input$sleepdiaryfile$datapath)
    
    
    # Create global with directories and give it default values -------
    global <- reactiveValues(data_in = homedir, data_out = homedir) #, pipeline = NULL)
    
    # Update global when input changes
    observeEvent(ignoreNULL = TRUE,
                 eventExpr = {
                   input$rawaccdir # every time input$rawaccdir updates ...
                 },
                 handlerExpr = { # ... we re-assign global$acc_in
                   if (!"path" %in% names(rawaccdir())) return()
                   home <- normalizePath(homedir)
                   global$raw_acc_in <-
                     file.path(home, paste(unlist(rawaccdir()$path[-1]), collapse = .Platform$file.sep))
                 })
    observeEvent(ignoreNULL = TRUE,
                 eventExpr = {
                   input$countaccdir 
                 },
                 handlerExpr = {
                   if (!"path" %in% names(countaccdir())) return()
                   home <- normalizePath(homedir)
                   global$count_acc_in <-
                     file.path(home, paste(unlist(countaccdir()$path[-1]), collapse = .Platform$file.sep))
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
    
    
    # Send directories to UI --------------------------------------------
    output$rawaccdir <- renderText({
      global$raw_acc_in
    })
    output$countaccdir <- renderText({
      global$count_acc_in
    })
    output$gpsdir <- renderText({
      global$gps_in
    })
    output$outputdir <- renderText({
      global$data_out
    })
    
    # Check and Edit config files ---------------------------------------
    configfilePALMSpy <- modConfigServer("edit_palmspy_config", tool = reactive("PALMSpy"))
    configfileGGIR <- modConfigServer("edit_ggir_config", tool = reactive("GGIR"))
 
    
    # Apply GGIR after button is pressed ---------------------------------
    runGGIR <- eventReactive(input$start_ggir, {
      if ("GGIR" %in% input$tools | "BrondCounts" %in% input$tools) {
        waiter <- waiter::Waiter$new(id = "start_ggir", html = waiter::spin_throbber())$show()
        on.exit(waiter$hide())
        if ("BrondCounts" %in% input$tools) {
          print("Starting GGIR and BrondCounts ...")
          do.BrondCounts = TRUE
        } else {
          print("Starting GGIR...")
          do.BrondCounts = FALSE
        }
        GGIRshiny(rawaccdir = global$raw_acc_in, outputdir = global$data_out, 
                  sleepdiary = isolate(sleepdiaryfile()), configfile = isolate(configfileGGIR()),
                  do.BrondCounts = do.BrondCounts)
        expected_output_file = paste0(global$data_out, "/output_", basename(global$raw_acc_in), "/results/part2_summary.csv")
        for (i in seq_len(10)){
          Sys.sleep(1)
        }
        test = file.exists(expected_output_file)
      }
      return(test)
    })
    
    # Apply PALMSpy after button is pressed ---------------------------------
    runPALMSpy <- eventReactive(input$start_palmspy, {
      if ("PALMSpy" %in% input$tools) {
        waiter <- waiter::Waiter$new(id = "start_palmspy", html = waiter::spin_throbber())$show()
        on.exit(waiter$hide())
        PALMSpy_R(gps_path = global$gps_in, acc_path = global$count_acc_in,
                  output_path = global$data_out, config_file = isolate(configfilePALMSpy()))
        for (i in seq_len(10)){
          Sys.sleep(1)
        }
        test = file.exists(paste0(global$data_out,"/testpython.csv"))
      }
      return(test)
    })
    
    # If button pressed send message to UI about success ----------------
    output$ggir_end_message <- renderText({
      message = ifelse(runGGIR() == TRUE,
                       yes = paste0("Procesing succesful ", Sys.time()),
                       no = paste0("Procesing unsuccesful ", Sys.time()))
    })
    output$palmspy_end_message <- renderText({
      message = ifelse(runPALMSpy() == TRUE,
                       yes = paste0("Procesing succesful ", Sys.time()),
                       no = paste0("Procesing unsuccesful ", Sys.time()))
    })
  }
  # Run the application 
  shinyApp(ui, server)
}