#' myApp
#'
#' @param homedir character to specify home directory
#' @param ... No input needed, function runs the app
#' @return no object is returned, just an app
#' @import shiny
#' @import shinyFiles
#' @importFrom callr r_bg
#' @export

# pkgload::load_all("."); HabitusGUI::myApp(homedir="~/projects/fontys") HabitusGUI::myApp(homedir="~/projects")
# pkgload::load_all("."); myApp(homedir="~/projects/fontys")
# roxygen2::roxygenise()
#/Member Files: LineMatthiesen#8897


# create temp log file

myApp <- function(homedir=getwd(), ...) {
  stdout_GGIR_tmp <- tempfile(fileext = ".log")
  stdout_palmsplusr_tmp <- tempfile(fileext = ".log")
  stdout_PALMSpy_tmp <- tempfile(fileext = ".log")
  mylog_GGIR <- shiny::reactiveFileReader(500, NULL, stdout_GGIR_tmp, readLines, warn = FALSE)
  mylog_palmsplusr <- shiny::reactiveFileReader(500, NULL, stdout_palmsplusr_tmp, readLines, warn = FALSE)
  mylog_PALMSpy <- shiny::reactiveFileReader(500, NULL, stdout_PALMSpy_tmp, readLines, warn = FALSE)

  ui <- fluidPage(
    theme = bslib::bs_theme(bootswatch = "litera"), #,"sandstone"), "sketchy" "pulse"
    # preview examples: https://bootswatch.com/
    # “cerulean”, “cosmo”, “cyborg”, “darkly”, “flatly”, “journal”, “litera”, “lumen”, 
    # “lux”, “materia”, “minty”, “morph”, “pulse”, “quartz”, “sandstone”, “simplex”, 
    #     “sketchy”, “slate”, “solar”, “spacelab”, “superhero”, “united”, “vapor”, “yeti”, “zephyr” 
    tabsetPanel(
      id = "wizard",
      type = "hidden",
      tabPanel("page_1",
               fluidRow(column(8, div(h1("Habitus"), style = "height:50px")),
                        column(4, div(imageOutput("logo_page1"), style = "height:50px;float:right"))
               ),
               p("\n"),
               checkboxGroupInput("availabledata", label = "Which type(s) of data would you like to analyse? ",
                                  choiceNames = list("Raw acceleration (at least ten values per second per axis)", 
                                                     "Counts (in ActiGraph .csv format)",
                                                     "GPS (in .csv format)", 
                                                     "GIS (shape files + linkage file)", 
                                                     "PALMS(py) output previously generated",
                                                     "Sleep Diary (in GGIR compatible .csv format)"),
                                  choiceValues = list("AccRaw", "ACount", "GPS", "GIS", "PALMSpy_out", "SleepDiary"), width = '100%'),
               conditionalPanel(condition = paste0("input.availabledata.indexOf(`AccRaw`) > -1  || ", # GGIR
                                                   "(input.availabledata.indexOf(`ACount`) > -1 && ", # PALMSpy
                                                   "input.availabledata.indexOf(`GPS`) > -1) || ",
                                                   "(input.availabledata.indexOf(`ACount`) > -1 && ", # PALMSplus variant 1
                                                   "input.availabledata.indexOf(`GPS`) > -1 && ",
                                                   "input.availabledata.indexOf(`GIS`) > -1) || ", 
                                                   "(input.availabledata.indexOf(`PALMSpy_out`) > -1 && ", #PALMSplus variant 2
                                                   "input.availabledata.indexOf(`GIS`) > -1)"),
                                hr(),
                                # If there is enough input data then show second check box to ask user about their research goals
                                checkboxGroupInput("researchgoals", label = "", 
                                                   choiceNames = "", choiceValues = "", width = '100%'),
                                # Show possible pipelines:
                                textOutput("pipeline"),
                                hr(),
                                checkboxGroupInput("tools", label = "Select the tools you would like to use:",
                                                   choiceNames = list("GGIR (R package)",
                                                                      "BrondCounts (R packages activityCounts + GGIR)",
                                                                      "PALMSpy (Python library)",
                                                                      "PALMSplus (R package)"),
                                                   choiceValues = list("GGIR", "BrondCounts", "PALMSpy", "PALMSplus"), width = '100%')
               ), 
               actionButton("page_12", "next")
      ),
      tabPanel("page_2",
               fluidRow(column(8, div(h1("Habitus - Data selection"), style = "height:50px")),
                        column(4, div(imageOutput("logo_page2"), style = "height:50px;float:right"))
               ),
               p("\n"),
               # Select input folder raw accelerometer data if raw data is available and GGIR is planned------------------
               conditionalPanel(condition = paste0("input.availabledata.indexOf(`AccRaw`) > -1 && ",
                                                   "(input.tools.includes(`GGIR`) || ",
                                                   "input.tools.includes(`BrondCounts`))"),
                                shinyFiles::shinyDirButton("rawaccdir", label = "Raw accelerometry data directory...",
                                                           title = "Select raw accelerometer data directory"),
                                verbatimTextOutput("rawaccdir", placeholder = TRUE)
               ),
               # Select input folder count accelerometer data if count data is available and PALMSpy is planned------------------
               # if not then count data will have to be estimated from the raw data, but we do not bother user
               # with questions where it should be stored
               conditionalPanel(condition = "input.availabledata.indexOf(`ACount`) > -1 && input.tools.includes(`PALMSpy`)",
                                shinyFiles::shinyDirButton("countaccdir", label = "Count accelerometry data directory...",
                                                           title = "Select count accelerometer data directory"),
                                verbatimTextOutput("countaccdir", placeholder = TRUE)
               ),
               # Select input folder gps data -----------------------------------
               conditionalPanel(condition = "input.availabledata.indexOf(`GPS`) > -1 && input.tools.includes(`PALMSpy`)",
                                shinyFiles::shinyDirButton("gpsdir", label = "GPS data directory...",
                                                           title = "Select GPS data directory"),
                                verbatimTextOutput("gpsdir", placeholder = TRUE)
               ),
               # Select input folder GIS data and GIS linkage file -----------------------------------
               conditionalPanel(condition = "input.availabledata.indexOf(`GIS`) > -1 && input.tools.includes(`PALMSplus`)",
                                shinyFiles::shinyDirButton("gisdir", label = "GIS data directory...",
                                                           title = "Select GIS data directory"),
                                verbatimTextOutput("gisdir", placeholder = TRUE),
                                # strong(textInput("dataset_name", label = "Give your dataset a name:", value = "", width = '100%')),
                                shinyFiles::shinyFilesButton("gislinkfile", label = "GIS linkage file...",
                                                             title = "Select GIS linkage file", multiple = FALSE),
                                verbatimTextOutput("gislinkfile", placeholder = TRUE)
               ),
               # Select input folder PALMSpy output data -----------------------------------
               conditionalPanel(condition = paste0("input.availabledata.indexOf(`PALMSpy_out`) > -1 && ",
                                                   "input.tools.includes(`PALMSplus`) && !input.tools.includes(`PALMSpy`)"),
                                shinyFiles::shinyDirButton("palmspyoutdir", label = "Previously generated PALMS(py) output directory...",
                                                           title = "Select PALMS(py) output directory"),
                                verbatimTextOutput("palmspyoutdir", placeholder = TRUE)
               ),
               # Upload sleep diary ----------------------------------------------------
               conditionalPanel(condition = "input.availabledata.indexOf(`SleepDiary`) > -1 && input.tools.includes(`GGIR`)",
                                shinyFiles::shinyFilesButton("sleepdiaryfile", label = "Sleepdiary file...",
                                                             title = "Select sleep diary file", multiple = FALSE),
                                verbatimTextOutput("sleepdiaryfile", placeholder = TRUE)
               ),
               # Specify output directory ----------------------------------------------
               fluidRow(
                 column(12,
                        shinyFiles::shinyDirButton("outputdir", "Output directory...",
                                                   title = "Select directory where output should be stored"),
                        verbatimTextOutput("outputdir", placeholder = TRUE)
                 )
               ),
               # Provide dataset name (only needed when working with GIS data ---------------------------------
               conditionalPanel(condition = "input.availabledata.indexOf(`GIS`) > -1 && input.tools.includes(`PALMSplus`)",
                                strong(textInput("dataset_name", label = "Give your dataset a name:", value = "", width = '100%')),
               ),
               
               hr(),
               actionButton("page_21", "prev"),
               actionButton("page_23", "next")
      ),
      tabPanel("page_3",
               fluidRow(column(8, div(h1("Habitus - Parameter Configuration"), style = "height:50px")),
                        column(4, div(imageOutput("logo_page3"), style = "height:50px;float:right"))
               ),
               p("\n"),
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
               conditionalPanel(condition = "input.tools.includes('PALMSplus')",
                                h2("PALMSplus"),
                                p("No parameters are needed for the PALMSplus"),
                                hr()
               ),
               actionButton("page_32", "prev"),
               actionButton("page_34", "next")
      ),
      tabPanel("page_4",
               # Button to start analysis ---------------------------------------------
               fluidRow(column(8, div(h1("Habitus - Analyses"), style = "height:50px")),
                        column(4, div(imageOutput("logo_page4"), style = "height:50px;float:right"))
               ),
               p("\n"),
               p("\n"),
               span(h4(textOutput("recommendorder")), style="color:purple"),
               # hr(),
               p("\n"),
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
                                shinyjs::useShinyjs(),
                                actionButton("start_ggir", "Start analysis", width = '300px'),
                                p("\n"),
                                verbatimTextOutput("mylog_GGIR"),
                                tags$head(tags$style("#mylog_GGIR{color:darkblue; font-size:12px; font-style:italic; 
overflow-y:scroll; max-height: 150px; background: ghostwhite;}")),
                                p("\n"),
                                htmlOutput("ggir_end_message"),
                                p("\n"),
                                DT::dataTableOutput("GGIRpart2"),
                                hr()
               ),
               conditionalPanel(condition = "input.tools.includes('PALMSpy')",
                                h3("PALMSpy:"),
                                shinyjs::useShinyjs(),
                                actionButton("start_palmspy", "Start analysis", width = '300px'),
                                p("\n"),
                                verbatimTextOutput("mylog_PALMSpy"),
                                tags$head(tags$style("#mylog_PALMSpy{color:darkblue; font-size:12px; font-style:italic; 
overflow-y:scroll; max-height: 150px; background: ghostwhite;}")),
                                p("\n"),
                                htmlOutput("palmspy_end_message"),
                                p("\n"),
                                DT::dataTableOutput("PALMSpy_file1"),
                                hr()
               ),
               conditionalPanel(condition = "input.tools.includes('PALMSplus')",
                                h3("PALMSplus:"),
                                shinyjs::useShinyjs(),
                                actionButton("start_palmsplus", "Start analysis", width = '300px'),
                                p("\n"),
                                verbatimTextOutput("mylog_palmsplusr"),
                                p("\n"),
                                htmlOutput("palmsplus_end_message"),
                                p("\n"),
                                DT::dataTableOutput("PALMSplus_file1"),
                                tags$head(tags$style("#mylog_palmsplusr{color:darkblue; font-size:12px; font-style:italic; 
overflow-y:scroll; max-height: 150px; background: ghostwhite;}")),
                                hr()
               ),
               actionButton("page_43", "prev")
      )
    )
  )
  
  server <- function(input, output, session) {
    getlogo = function() {
      renderImage({
        list(src = system.file("www/logos_merged.png", package = "HabitusGUI")[1],
             contentType = "image/png",
             width = 280, height = 70)
      }, deleteFile = FALSE)
    }
    output$logo_page1 = getlogo()
    output$logo_page2 = getlogo()
    output$logo_page3 = getlogo()
    output$logo_page4 = getlogo()
    switch_page <- function(i) {
      updateTabsetPanel(inputId = "wizard",
                        selected = paste0("page_", i))
    }
    observeEvent(input$page_12, {
      if (length(input$availabledata) == 0 & length(input$tools) == 0) {
        showNotification("Select data type(s) to be analysed", type = "error")
      } else {
        if (length(input$tools) == 0) {
          showNotification("Select at least one tool", type = "error")
        } else {
          if ("GGIR" %in% input$tools == TRUE & "AccRaw" %in% input$availabledata == FALSE) {
            showNotification("GGIR not possible without access to raw accelerometer data", type = "error")
          } else {
            if ("PALMSpy" %in% input$tools == TRUE & "GPS" %in% input$availabledata == FALSE) {
              showNotification("PALMSpy not possible without access to GPS data", type = "error")
            } else {
              if ("PALMSpy" %in% input$tools == TRUE & all(c("AccRaw", "ACount") %in% input$availabledata == FALSE)) {
                showNotification("PALMSpy not possible without access to Accelerometer data", type = "error")
              } else {
                if ("PALMSplus" %in% input$tools == TRUE & "GIS" %in% input$availabledata == FALSE) {
                  showNotification("PALMSplus not possible without access to GIS data", type = "error")
                } else {
                  if ("PALMSplus" %in% input$tools == TRUE & ("PALMSpy_out" %in% input$availabledata == FALSE &
                                                              "GPS" %in% input$availabledata == FALSE & all(c("AccRaw", "ACount") %in% input$availabledata == FALSE))) {
                    showNotification("PALMSplus requires either previously generated PALMS(py) output or GPS and Accelerometer data", type = "error")
                  } else {
                    if ("BrondCounts" %in% input$tools == TRUE & "AccRaw" %in% input$availabledata == FALSE) {
                      showNotification("BrondCounts not possible without access to raw accelerometer data", type = "error")
                    } else {
                      switch_page(2)
                    }
                  }
                }
              }
            }
          }
        }
      }
    })
    observeEvent(input$page_21, switch_page(1))
    observeEvent(input$page_23, {
      if ("AccRaw" %in% input$availabledata & "GGIR" %in% input$tools & as.character(input$rawaccdir)[1] == "0") {
        showNotification("Select raw accelerometer data directory", type = "error")
      } else {
        if ("AccRaw" %in% input$availabledata & "BrondCounts" %in% input$tools & as.character(input$rawaccdir)[1] == "0") {
          showNotification("Select raw accelerometer data directory", type = "error")
        } else {
          if ("ACount" %in% input$availabledata & "PALMSpy" %in% input$tools & as.character(input$countaccdir)[1] == "0") {
            showNotification("Select count accelerometer data directory", type = "error")
          } else {
            if ("GPS" %in% input$availabledata & "PALMSpy" %in% input$tools & as.character(input$gpsdir)[1] == "0") {
              showNotification("Select GPS data directory", type = "error")
            } else {
              current_sleepdiary = as.character(parseFilePaths(c(home = homedir), input$sleepdiaryfile)$datapath)
              if ("SleepDiary" %in% input$availabledata & "GGIR" %in% input$tools &
                  length(current_sleepdiary) == 0) { 
                showNotification("Select sleepdiary file", type = "error")
              } else {
                current_gislinkfile = as.character(parseFilePaths(c(home = homedir), input$gislinkfile)$datapath)
                if ("GIS" %in% input$availabledata &
                    "PALMSplus" %in% input$tools &
                    (as.character(input$gisdir)[1] == "0" |
                     length(current_gislinkfile) == 0)) {
                    showNotification("Select GIS data directory and GIS linkage file", type = "error")
                } else {
                  if ("PALMSpy_out" %in% input$availabledata & "PALMSplus" %in% input$tools & as.character(input$palmspyoutdir)[1] == "0") {
                    showNotification("Select previously generated PALMS(py) output directory", type = "error")
                  } else {
                    switch_page(3)
                  }
                }
                
              }
            }
          }
        }
      }
    })
    observeEvent(input$page_32, switch_page(2))
    observeEvent(input$page_34, {
      configs_ready = TRUE
      if ("PALMSpy" %in% input$tools) {
        if (length(paste0(configfilePALMSpy())) == 0) {
          configs_ready = FALSE
        }
      }
      if ("GGIR" %in% input$tools) {
        if (length(paste0(configfileGGIR())) == 0) {
          configs_ready = FALSE
        }
      }
      if (configs_ready == TRUE) {
        showNotification("Saving configuration file(s) to output folder", type = "message", duration = 2)
        if ("GGIR" %in% input$tools) {
          if (configfileGGIR() != paste0(global$data_out, "/config.csv")) {
            file.copy(from = configfileGGIR(), to = paste0(global$data_out, "/config.csv"), 
                      overwrite = TRUE, recursive = FALSE, copy.mode = TRUE)
          }
          current_sleepdiaryfile = as.character(parseFilePaths(c(home = homedir), sleepdiaryfile())$datapath)
          if (length(current_sleepdiaryfile) > 0) {
            if (current_sleepdiaryfile != paste0(global$data_out, "/sleepdiary.csv")) {
              file.copy(from = current_sleepdiaryfile, to = paste0(global$data_out, "/sleepdiary.csv"), 
                        overwrite = TRUE, recursive = FALSE, copy.mode = TRUE)
            }
            sleepdiaryfile_local = paste0(global$data_out, "/sleepdiary.csv")
          } else  {
            sleepdiaryfile_local = c()
          }
        }
        if ("PALMSpy" %in% input$tools) {
          if (configfilePALMSpy() != paste0(global$data_out, "/config.json")) {
            file.copy(from = configfilePALMSpy(), to = paste0(global$data_out, "/config.json"), 
                      overwrite = TRUE, recursive = FALSE, copy.mode = TRUE)
          }
        }
        switch_page(4)
      } else {
        showNotification("Select configuration file(s)", type = "error")
      }
    })
    observeEvent(input$page_43, switch_page(3))
    
    # Defined time to ensure file count is only checked twice per second ---------
    timer = reactiveTimer(500) 
    
    # Ask user questions about available and research interests
    # and use the answers to identify a suitable pipeline
    # Update checkbox possible research goals depending on available data
    observeEvent(input$availabledata, {
      x <- input$availabledata
      # Can use character(0) to remove all choices
      if (is.null(x)) x <- character(0)
      researchgoals = c()
      if ("GPS" %in% x & any(c("AccRaw", "ACount") %in% x)) researchgoals = c(researchgoals, "Trips", "QC")
      if (all(c("GPS", "GIS") %in% x) & any(c("AccRaw", "ACount") %in% x)) researchgoals = c(researchgoals, "Environment", "QC")
      if (all(c("PALMSpy_out", "GIS") %in% x)) researchgoals = c(researchgoals, "Environment", "QC")
      if ("AccRaw" %in% x | all(c("AccCount", "GPS")  %in% x)) researchgoals = c(researchgoals, "PB", "QC")
      if ("AccRaw" %in% x) researchgoals = c(researchgoals, "QC")
      if ("ACount" %in% x == TRUE & "GPS" %in% x == FALSE & "AccRaw" %in% x == FALSE) researchgoals = c()
      reasearchgoalsNames = c("Data quality assessment", "Physical activity, sedentary behaviour & sleep",
                              "Trips (displacements)", "Relation between behaviour and environment")
      reasearchgoalsValues = c("QC", "PB", "Trips", "Environment")
      
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
      ifelse(length(proposed_pipeline()) == 0, yes = "--> Tick boxes above according to the analysis you would like to do", no = message)
    })
    
    # check whether GGIR is in the pipeline, and send to UI,
    # such that it can condition the UI on this.
    
    # Extract directories ---------------
    shinyDirChoose(input, 'rawaccdir',  roots = c(home = homedir))
    shinyDirChoose(input, 'countaccdir',  roots = c(home = homedir))
    shinyDirChoose(input, 'gpsdir',  roots = c(home = homedir))
    shinyDirChoose(input, 'gisdir',  roots = c(home = homedir))
    shinyFileChoose(input, 'gislinkfile',  roots = c(home = homedir))
    shinyDirChoose(input, 'palmspyoutdir',  roots = c(home = homedir)) # Allow for old output to be used as input
    shinyDirChoose(input, 'outputdir',  roots = c(home = homedir))
    shinyFileChoose(input, 'sleepdiaryfile',  roots = c(home = homedir))
    
    
    # Capture provided directories in reactive object ----------------------------
    rawaccdir <- reactive(input$rawaccdir)
    countaccdir <- reactive(input$countaccdir)
    gpsdir <- reactive(input$gpsdir)
    gisdir <- reactive(input$gisdir)
    gislinkfile <- reactive(input$gislinkfile)
    palmspyoutdir <- reactive(input$palmspyoutdir) # Allow for old output to be used as input
    outputdir <- reactive(input$outputdir)
    sleepdiaryfile <- reactive(input$sleepdiaryfile) #$datapath
    
    # Create global with directories and give it default values -------
    global <- reactiveValues(data_out = homedir) #, pipeline = NULL)
    
    
    
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
                   input$gisdir # every time input$gisdir updates ...
                 },
                 handlerExpr = { # ... we re-assign global$gis_in
                   if (!"path" %in% names(gisdir())) return()
                   home <- normalizePath(homedir)
                   global$gis_in <-
                     file.path(home, paste(unlist(gisdir()$path[-1]), collapse = .Platform$file.sep))
                   
                   # send gisdir to textInput field
                   # Can also set the label, this time for input$inText2
                   parent_folder = global$gis_in
                   parent_folder = gsub(pattern = "\\", replacement =  "/", x = parent_folder, fixed = TRUE)
                   parent_folder = unlist(strsplit(parent_folder, "/"))
                   if (length(parent_folder) > 1) {
                     parent_folder = rev(parent_folder)[2]
                     parent_folder = gsub(pattern = " ", replacement =  "_", x = parent_folder, fixed = TRUE)
                   } else {
                     parent_folder = ""
                   }
                   updateTextInput(session, "dataset_name",
                                   # label = paste("New label", parent_folder),
                                   value = paste(parent_folder))
                   
                 })
    observeEvent(ignoreNULL = TRUE,
                 eventExpr = {
                   input$gislinkfile # every time input$gislinkfile updates ...
                 },
                 handlerExpr = { # ... we re-assign global$gis_in
                   if (!"files" %in% names(gislinkfile())) return()
                   home <- normalizePath(homedir)
                   global$gislinkfile_in <-
                     as.character(parseFilePaths(c(home = homedir), gislinkfile())$datapath)
                 })
    observeEvent(ignoreNULL = TRUE,
                 eventExpr = {
                   input$palmspyoutdir # every time input$palmspyoutdir updates ...
                 },
                 handlerExpr = { # ... we re-assign global$palmspyout_in
                   if (!"path" %in% names(palmspyoutdir())) return()
                   home <- normalizePath(homedir)
                   global$palmspyout_in <-
                     file.path(home, paste(unlist(palmspyoutdir()$path[-1]), collapse = .Platform$file.sep))
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
                   input$sleepdiaryfile # every time input$sleepdiaryfile updates ...
                 },
                 handlerExpr = { # ... we re-assign global$sleepdiaryfile
                   if (!"files" %in% names(sleepdiaryfile())) return()
                   home <- normalizePath(homedir)
                   global$sleepdiaryfile <-
                     as.character(parseFilePaths(c(home = homedir), sleepdiaryfile())$datapath)
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
    output$gisdir <- renderText({
      global$gis_in
    })
    output$gislinkfile <- renderText({
      global$gislinkfile_in
    })
    output$palmspyoutdir <- renderText({
      global$palmspyout_in
    })
    output$outputdir <- renderText({
      global$data_out
    })
    output$sleepdiaryfile <- renderText({
      global$sleepdiaryfile
    })
    
    
    
    
    # Check and Edit config files ---------------------------------------
    configfilePALMSpy <- modConfigServer("edit_palmspy_config", tool = reactive("PALMSpy"), homedir = homedir)
    configfileGGIR <- modConfigServer("edit_ggir_config", tool = reactive("GGIR"), homedir = homedir)
    
    #========================================================================
    # Apply GGIR / BrondCounts after button is pressed
    #========================================================================
    runGGIR <- eventReactive(input$start_ggir, {
      GIRBrondCounts_message = ""
      
      if ("GGIR" %in% input$tools | "BrondCounts" %in% input$tools) {
        GGIRBrondCounts_message = ""
        # Basic check before running function:
        ready_to_run_ggirbrondcounts = FALSE
        if (dir.exists(global$raw_acc_in)) {
          acc_files_available = length(dir(path = global$raw_acc_in, pattern = "csv|bin|gt3x|bin|cwa|wav", recursive = FALSE, full.names = FALSE)) > 0
          if (acc_files_available == TRUE) {
            ready_to_run_ggirbrondcounts = TRUE
          } else {
            GGIRBrondCounts_message = paste0("No count files found in ", global$raw_acc_in)
          }
        } else {
          GGIRBrondCounts_message = paste0("Folder that is supposed to hold acceleration files does not exist: ", global$raw_acc_in)
        }
        # Only run function when checks are met:
        if (ready_to_run_ggirbrondcounts == TRUE) {
          shinyjs::hide(id = "start_ggir")
          if ("BrondCounts" %in% input$tools) {
            id_ggir = showNotification("GGIR and BrondCounts in progress ...", type = "message", duration = NULL, closeButton = FALSE)
            do.BrondCounts = TRUE
          } else {
            id_ggir = showNotification("GGIR in progress ...", type = "message", duration = NULL, closeButton = FALSE)
            do.BrondCounts = FALSE
          }
          
          
          if (file.exists(paste0(global$data_out, "/sleepdiary.csv"))) { # because this is not a global variable
            sleepdiaryfile_local = paste0(global$data_out, "/sleepdiary.csv")
          } else {
            sleepdiaryfile_local = c()
          }

          output$mylog_GGIR <- renderText({
            paste(mylog_GGIR(), collapse = '\n')
          })
          
          # Start GGIR
          x_ggir <- r_bg(func = function(GGIRshiny, rawaccdir, outputdir, 
                                         sleepdiary, configfile, do.BrondCounts){
            GGIRshiny(rawaccdir, outputdir, 
                      sleepdiary, configfile, do.BrondCounts)
          },
          args = list(GGIRshiny = GGIRshiny,
                      rawaccdir = isolate(global$raw_acc_in),
                      outputdir = global$data_out, 
                                sleepdiary = sleepdiaryfile_local,
                                configfile = paste0(global$data_out, "/config.csv"), #isolate(configfileGGIR()),
                                do.BrondCounts = do.BrondCounts),
                    stdout = stdout_GGIR_tmp,
                    stderr = "2>&1")
          
          # GGIRshiny(rawaccdir = isolate(global$raw_acc_in), outputdir = global$data_out, 
          #           sleepdiary = sleepdiaryfile_local, configfile = paste0(global$data_out, "/config.csv"), #isolate(configfileGGIR()),
          #           do.BrondCounts = do.BrondCounts)
          
          # Copy tmp log file to actual log file for user to see
          logfile = paste0(isolate(global$data_out), "/GGIR.log")
          observe({
            if (x_ggir$poll_io(0)[["process"]] != "ready") {
              invalidateLater(5000)
            } else {
              on.exit(removeNotification(id_ggir), add = TRUE)
              file.copy(from = stdout_GGIR_tmp, to = logfile, overwrite = TRUE)
              # Now check whether results are correctly generated:
              expected_outputdir_ggir = paste0(global$data_out, "/output_", basename(global$raw_acc_in))
              expected_ggiroutput_file = paste0(global$data_out, "/output_", basename(global$raw_acc_in), "/results/part2_daysummary.csv")
              if (file.exists(expected_ggiroutput_file) == TRUE) { # checks whether ggir output was created
                if ("BrondCounts" %in% input$tools) { # if BrondCounts was suppoed to run
                  expected_outputdir_brondcounts = paste0(global$data_out, "/actigraph")
                  if (dir.exists(expected_outputdir_brondcounts) == TRUE) { # checks whether output dir was created
                    if (length(dir(expected_outputdir_brondcounts) > 0)) { # checks whether it has been filled with results
                      GGIRBrondCounts_message = paste0("BrondCounts and GGIR successfully completed at ", Sys.time(), " Output is stored in ", 
                                                       expected_outputdir_brondcounts, " and ",
                                                       expected_outputdir_ggir, ". The table below shows the content of part2_daysummary.csv")
                      GGIRpart2 = read.csv(expected_ggiroutput_file)
                      output$GGIRpart2 <- DT::renderDataTable(GGIRpart2, options = list(scrollX = TRUE))
                    } else {
                      GGIRBrondCounts_message = paste0("BrondCounts unsuccessful. No file found inside ", expected_outputdir_brondcounts)
                    }
                  } else {
                    GGIRBrondCounts_message = paste0("BrondCounts unsuccessful. Dir ",expected_outputdir_brondcounts, " not found")
                  }
                } else {
                  GGIRBrondCounts_message = paste0("GGIR successfully completed at ", Sys.time(), 
                                                   "<br/>Output is stored in: ", 
                                                   expected_outputdir_ggir,
                                                   "<br/>The table below shows the content of part2_daysummary.csv:")
                  GGIRpart2 = read.csv(expected_ggiroutput_file, nrow = 100)
                  output$GGIRpart2 <- DT::renderDataTable(GGIRpart2, options = list(scrollX = TRUE))
                }
                output$ggir_end_message <- renderUI({
                  HTML(paste0(GGIRBrondCounts_message))
                })
              }
            }
          })
        }
      }
      return()
    })
    
    #========================================================================
    # Apply PALMSpy after button is pressed
    #========================================================================
    runPALMSpy <- eventReactive(input$start_palmspy, {
      PALMSpy_message = ""
      if ("PALMSpy" %in% input$tools) {
        PALMSpy_message = "Error: Contact maintainer"
        ready_to_run_palsmpy = FALSE
        # Check whether input files exist:
        gps_files_available = length(dir(path = global$gps_in, pattern = ".csv", recursive = FALSE, full.names = FALSE)) > 0
        # Get location of count data:
        if ("ACount" %in% input$availabledata) { # if available data includes counts then find them there
          count_file_location = global$count_acc_in
        } else {  # if available data does not include counts then look for simulated data
          count_file_location = paste0(global$data_out, "/actigraph")
        }
        # Check whether both count and gps data exist
        if (dir.exists(count_file_location) == TRUE) {
          cnt_files_available = length(dir(path = count_file_location, pattern = ".csv", 
                                           recursive = FALSE, full.names = FALSE)) > 0
          if (cnt_files_available == TRUE) {
            if (gps_files_available ==  TRUE) {
              ready_to_run_palsmpy = TRUE
            }
          } else {
            PALMSpy_message = paste0("No count files found in ", count_file_location)
          }
        } else {
          PALMSpy_message = paste0("Folder that is supposed to hold count files does not exist: ", 
                                   count_file_location, " First run GGIR and BrondCounts.")
        }
      }
      if (ready_to_run_palsmpy == TRUE) {
        id_palmspy = showNotification("PALMSpy in progress ...", type = "message", duration = NULL, closeButton = FALSE)
        shinyjs::hide(id = "start_palmspy")
        # on.exit(removeNotification(id_palmspy), add = TRUE)
        
        output$mylog_PALMSpy <- renderText({
          paste(mylog_PALMSpy(), collapse = '\n')
        })
        
        
        # basecommand = paste0("cd ",global$data_out," ; palmspy --gps-path ", global$gps_in,
        #                      " --acc-path ", count_file_location,
        #                      " --config-file ", paste0(global$data_out, "/config.json"))
        
        # # system2(command = "cd", args = gsub(pattern = "cd ", replacement = "", x = basecommand),
        # #         stdout = "stdout_palmspy.log", stderr = "stderr_palmspy.log", wait = TRUE)
        # 
        # # Start PALMSpy
        # x_palmspy <- r_bg(func = function(system2, command, args, stdout, stderr, wait){
        #   system2(command, args, stdout, stderr, wait)
        # },
        # args = list(system2 = system2,
        #             command = "cd",
        #             args = gsub(pattern = "cd ", replacement = "", x = basecommand),
        #             stdout = "", 
        #             stderr = "",
        #             wait = TRUE),
        # stdout = stdout_PALMSpy_tmp,
        # stderr = "2>&1")
        # 
        
        # # Start PALMSpy
        x_palmspy <- r_bg(func = function(PALMSpyshiny, outputdir, gpsdir, count_file_location){
          PALMSpyshiny(outputdir, gpsdir, count_file_location)
        },
        args = list(PALMSpyshiny = PALMSpyshiny,
                   outputdir = global$data_out,
                   gpsdir = global$gps_in,
                   count_file_location = count_file_location),
        stdout = stdout_PALMSpy_tmp,
        stderr = "2>&1")
        
        # basecommand = paste0("cd ",global$data_out," ; palmspy --gps-path ", global$gps_in,
        #                      " --acc-path ", count_file_location,
        #                      " --config-file ", paste0(global$data_out, "/config.json"))
        # # stdout = system(command = basecommand, intern = TRUE) # old command
        # system2(command = "cd", args = gsub(pattern = "cd ", replacement = "", x = basecommand),
        #         stdout = "stdout_palmspy.log", stderr = "stderr_palmspy.log", wait = TRUE)
        
        # Copy tmp log file to actual log file for user to see
        logfile = paste0(isolate(global$data_out), "/PALMSpy.log")
        observe({
          if (x_palmspy$poll_io(0)[["process"]] != "ready") {
            invalidateLater(5000)
          } else {
            on.exit(removeNotification(id_palmspy), add = TRUE)
            file.copy(from = stdout_PALMSpy_tmp, to = logfile, overwrite = TRUE)     
            # Now check whether results are correctly generated:
            expected_palmspy_results_dir = paste0(global$data_out,"/PALMSpy_output")
            
            if (dir.exists(expected_palmspy_results_dir)) {
              PALMSpy_message = paste0("PALMSpy completed at ", Sys.time(),
                                       "<br/>Output is stored in: ", expected_palmspy_results_dir) 
              # Now send content of 1 output file to UI
              expected_palmspyoutput_file = dir(expected_palmspy_results_dir, recursive = TRUE, full.names = TRUE, pattern = "csv")[1]
              if (length(expected_palmspyoutput_file) > 0) {
                PALMSpy_message = paste0(PALMSpy_message, "<br/>The table below shows the top of ", basename(expected_palmspyoutput_file))
                PALMSpy_file1 = read.csv(file = expected_palmspyoutput_file, nrow = 100)
                output$PALMSpy_file1 <- DT::renderDataTable(PALMSpy_file1, options = list(scrollX = TRUE))
              }
            } else {
              PALMSpy_message = "PALMSpy unsuccessful: see PALMSpy.log in your output folder"
            }
            output$palmspy_end_message <- renderUI({
              HTML(paste0(PALMSpy_message))
            })
          }
          
        })
      }
      return()
    })
    #========================================================================
    # Apply PALMSplus after button is pressed
    #========================================================================
    runPALMSplus <- eventReactive(input$start_palmsplus, {
      PALMSplus_message = ""
      
      if ("PALMSplus" %in% input$tools) {
        PALMSplus_message = "Error: Contact maintainer"
        # Basic check before running function:
        ready_to_run_palmsplus = FALSE
        # Check for PALMSpy output (two possible sources either from this run or from a previous run)
        if (dir.exists(paste0(global$data_out,"/PALMSpy_output"))) {
          expected_palmspy_results_dir = paste0(global$data_out,"/PALMSpy_output")
        } else {
          expected_palmspy_results_dir = global$palmspyout_in
        }
        if (dir.exists(expected_palmspy_results_dir)) {
          Nfiles_in_dir = length(dir(path = expected_palmspy_results_dir, pattern = "csv", recursive = FALSE, full.names = FALSE))
          if (Nfiles_in_dir > 0) {
            # also check for GIS files
            if (dir.exists(global$gis_in)) {
              Nfiles_in_gisdir = length(dir(path = global$gis_in, recursive = FALSE, full.names = FALSE))
              if (Nfiles_in_gisdir > 0) {
                if (file.exists(global$gislinkfile_in)) {
                  ready_to_run_palmsplus = TRUE
                } else {
                  PALMSplus_message = paste0("GIS link file not found: ", global$gislinkfile_in)
                }
              } else {
                PALMSplus_message = paste0("No files found in GIS folder: ", global$gis_in)
              }
            } else {
              PALMSplus_message = paste0("Folder that is supposed to hold  GIS files does not exist: ", global$gis_in)
            }
          } else {
            PALMSplus_message = paste0("No files found in PALMSpy output folder: ", expected_palmspy_results_dir)
          }
        } else {
          PALMSplus_message = paste0("Folder that is supposed to hold acceleration files does not exist: ", expected_palmspy_results_dir)
        }
        # Only run function when checks are met:
        if (ready_to_run_palmsplus == TRUE) {
          shinyjs::hide(id = "start_palmsplus")
          id_palmsplusr = showNotification("PALMSplusR in progress ...", type = "message", duration = NULL, closeButton = FALSE)
          on.exit(removeNotification(id_palmsplusr), add = TRUE)
          
          output$mylog_palmsplusr <- renderText({
            paste(mylog_palmsplusr(), collapse = '\n')
          })
          
          PALMSplusRshiny(#country_name = "BA", # <= Discuss, extract from GIS foldername?
            # participant_exclude_list, # <= Discuss, leave out from linkfile?
            gisdir = global$gis_in,
            palmsdir = expected_palmspy_results_dir,
            gislinkfile = global$gislinkfile_in,
            outputdir = isolate(global$data_out),
            dataset_name = input$dataset_name)
          
          # Start palmsplusr
          x_palmsplusr <- r_bg(func = function(PALMSplusRshiny, gisdir, palmsdir, gislinkfile, outputdir, dataset_name){
            PALMSplusRshiny(gisdir, palmsdir, gislinkfile, outputdir, dataset_name)
          },
          args = list(PALMSplusRshiny = PALMSplusRshiny,
                      gisdir = global$gis_in,
                      palmsdir = expected_palmspy_results_dir,
                      gislinkfile = global$gislinkfile_in,
                      outputdir = isolate(global$data_out),
                      dataset_name = input$dataset_name),
          stdout = stdout_palmsplusr_tmp,
          stderr = "2>&1")
          #   # Start PALMSplusR
          #   PALMSplusRshiny(#country_name = "BA", # <= Discuss, extract from GIS foldername?
          #     # participant_exclude_list, # <= Discuss, leave out from linkfile?
          #     gisdir = global$gis_in,
          #     palmsdir = expected_palmspy_results_dir,
          #     gislinkfile = global$gislinkfile_in,
          #     outputdir = isolate(global$data_out),
          #     dataset_name = input$dataset_name)
          # Copy tmp log file to actual log file for user to see
          logfile = paste0(isolate(global$data_out), "/palmsplusr.log")
          observe({
            if (x_palmsplusr$poll_io(0)[["process"]] != "ready") {
              invalidateLater(5000)
            } else {
              file.copy(from = stdout_palmsplusr_tmp, to = logfile, overwrite = TRUE)     
              # Now check whether results are correctly generated:
              expected_palmsplus_folder = paste0(isolate(global$data_out), "/PALMSplus_output")
              if (dir.exists(expected_palmsplus_folder) == TRUE) {
                csv_files_palmsplus = dir(expected_palmsplus_folder,pattern = "csv", recursive = TRUE)
                if (length(csv_files_palmsplus) > 0) {
                  PALMSplus_message = paste0("PALMSplusR successfully completed at ", Sys.time(),
                                             "<br/>Output is stored in: ", expected_palmsplus_folder,
                                             "<br/>The table below shows the content of ", basename(csv_files_palmsplus),
                                             "<br/>Log file: ", logfile)
                  first_csv_file_palmsplus = read.csv(csv_files_palmsplus)
                  output$PALMSpluscsv <- DT::renderDataTable(first_csv_file_palmsplus, options = list(scrollX = TRUE))
                } else {
                  PALMSplus_message = paste0("PALMSplusR unsuccessful",
                                             "<br/>No file found inside: ", expected_palmsplus_folder,
                                             "<br/>Log file: ", logfile)
                }
              } else {
                PALMSplus_message = paste0("PALMSplusR unsuccessful",
                                           "<br/>No file found inside: ", expected_palmsplus_folder,
                                           "<br/>Log file: ", logfile)
              }
              output$palmsplus_end_message <- renderUI({
                HTML(paste0(PALMSplus_message))
              })
            }
          })
        }
      }
      return()
    })
    output$recommendorder <- renderText({
      pipeline = proposed_pipeline()
      if ("GGIR" %in% pipeline & "BrondCounts" %in% pipeline) {
        pipeline = pipeline[-which(pipeline %in% c("GGIR", "BrondCounts"))]
        pipeline = c("GGIR & Brondcounts", pipeline)
      }
      if (length(pipeline) > 1) {
        message = paste0("Recommended order of analyses: ", paste0(pipeline, collapse = " -> "))
      } else {
        message = ""
      }
      return(message)
    })
    # Only used to initialise the value
    output$ggir_end_message <- renderText({
      message = runGGIR()
    })
    output$palmspy_end_message <- renderText({
      message = runPALMSpy()
    })
    output$palmsplus_end_message <- renderText({
      message = runPALMSplus()
    })
  }
  # Run the application 
  shinyApp(ui, server)
}