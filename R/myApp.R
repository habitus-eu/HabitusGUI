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

# pkgload::load_all("."); HabitusGUI::myApp(homedir="~/projects/fontys") HabitusGUI::myApp(homedir="~/projects")
# pkgload::load_all("."); myApp(homedir="~/projects/fontys")
# roxygen2::roxygenise()

myApp <- function(homedir=getwd(), ...) {
  ui <- fluidPage(
    theme = bslib::bs_theme(bootswatch = "sketchy"), #,"sandstone"),
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
               checkboxGroupInput("availabledata", label = "Which type(s) of data would you like to analyse? ",
                                  choiceNames = list("Raw acceleration (at least ten values per second per axis)", 
                                                     "Counts (in ActiGraph .csv format)",
                                                     "GPS (in .csv format)", "GIS", "Sleep Diary (in GGIR compatible .csv format)"),
                                  choiceValues = list("AccRaw", "ACount", "GPS", "GIS", "SleepDiary"), width = '100%'),
               
               # If there is AccRaw or ACount data then show second text box that asks user about research goals
               conditionalPanel(condition = paste0("input.availabledata.indexOf(`AccRaw`) > -1  || ",
                                                   "(input.availabledata.indexOf(`ACount`) > -1 &&",
                                                   "input.availabledata.indexOf(`GPS`) > -1)"),
                                hr(),
                                checkboxGroupInput("researchgoals", label = "", 
                                                   choiceNames = "", choiceValues = "", width = '100%')
               ), 
               # Show possible pipelines:
               textOutput("pipeline"),
               conditionalPanel(condition = paste0("input.availabledata.indexOf(`AccRaw`) > -1  || ",
                                                   "(input.availabledata.indexOf(`ACount`) > -1 &&",
                                                   "input.availabledata.indexOf(`GPS`) > -1)"),
                                hr(),
                                checkboxGroupInput("tools", label = "Select the tools you would like to use:",
                                                   choiceNames = list("GGIR (R package)",
                                                                      "BrondCounts (R packages activityCounts + GGIR)",
                                                                      "PALMSpy (Python library)", "PALMSplus (R package)"),
                                                   choiceValues = list("GGIR", "BrondCounts", "PALMSpy", "PALMSplus"), width = '100%'),
               ),
               actionButton("page_12", "next")
      ),
      tabPanel("page_2",
               fluidRow(column(8, div(h1("Habitus - Data selection"), style = "height:50px")),
                        column(4, div(imageOutput("logo_page2"), style = "height:50px;float:right"))
               ),
               # Select input folder raw accelerometer data if raw data is available and GGIR is planned------------------
               conditionalPanel(condition = paste0("input.availabledata.indexOf(`AccRaw`) > -1  && ",
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
               conditionalPanel(condition = "input.availabledata.indexOf(`GPS`) > -1 && input.tools.includes('PALMSpy')",
                                shinyFiles::shinyDirButton("gpsdir", label = "GPS data directory...",
                                                           title = "Select GPS data directory"),
                                verbatimTextOutput("gpsdir", placeholder = TRUE)
               ),
               # Specify output directory ----------------------------------------------
               fluidRow(
                 column(12,
                        shinyFiles::shinyDirButton("outputdir", "Output directory...",
                                                   title = "Select directory where output should be stored"),
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
               fluidRow(column(8, div(h1("Habitus - Parameter Configuration"), style = "height:50px")),
                        column(4, div(imageOutput("logo_page3"), style = "height:50px;float:right"))
               ),
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
               fluidRow(column(8, div(h1("Habitus - Analyses"), style = "height:50px")),
                        column(4, div(imageOutput("logo_page4"), style = "height:50px;float:right"))
               ),
               span(h4(textOutput("recommendorder")), style="color:purple"),
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
    
    
    getlogo = function() {
      renderImage({
        list(src = system.file("www/fontys_logo.png", package = "HabitusGUI")[1],
             contentType = "image/png",
             width = 100, height = 100)
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
              if ("PALMSplus" %in% input$tools == TRUE & "GIS" %in% input$availabledata == FALSE) {
                showNotification("PALMSplus not possible without access to GIS data", type = "error")
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
              if ("SleepDiary" %in% input$availabledata & "GGIR" %in% input$tools & length(as.character(sleepdiaryfile())) == 0) {
                showNotification("Select sleepdiary file", type = "error")
              } else {
                switch_page(3)
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
          file.copy(from = configfileGGIR(), to = paste0(global$data_out, "/config.csv"), 
                    overwrite = TRUE, recursive = FALSE, copy.mode = TRUE)
        }
        if ("PALMSpy" %in% input$tools) {
          file.copy(from = configfilePALMSpy(), to = paste0(global$data_out, "/config.json"), 
                    overwrite = TRUE, recursive = FALSE, copy.mode = TRUE)
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
      GGIRBrondCounts_message = ""
      if ("GGIR" %in% input$tools | "BrondCounts" %in% input$tools) {
        GGIRBrondCounts_message = "Error: Contact maintainer"
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
          waiter <- waiter::Waiter$new(id = "start_ggir", html = waiter::spin_throbber())$show()
          on.exit(waiter$hide())
          if ("BrondCounts" %in% input$tools) {
            id_ggir = showNotification("GGIR and BrondCounts in progress ...", type = "message", duration = NULL, closeButton = FALSE)
            do.BrondCounts = TRUE
          } else {
            id_ggir = showNotification("GGIR in progress ...", type = "message", duration = NULL, closeButton = FALSE)
            do.BrondCounts = FALSE
          }
          on.exit(removeNotification(id_ggir), add = TRUE)
          GGIRshiny(rawaccdir = global$raw_acc_in, outputdir = global$data_out, 
                    sleepdiary = isolate(sleepdiaryfile()), configfile = isolate(configfileGGIR()),
                    do.BrondCounts = do.BrondCounts)
          
          # Now check whether results are correctly generated:
          expected_ggiroutput_file = paste0(global$data_out, "/output_", basename(global$raw_acc_in), "/results/part2_summary.csv")
          if (file.exists(expected_ggiroutput_file) == TRUE) { # checks whether ggir output was created
            if ("BrondCounts" %in% input$tools) { # if BrondCounts was suppoed to run
              expected_outputdir_brondcounts = paste0(global$data_out, "/actigraph")
              if (dir.exists(expected_outputdir_brondcounts) == TRUE) { # checks whether output dir was created
                if (length(dir(expected_outputdir_brondcounts) > 0)) { # checks whether it has been filled with results
                  GGIRBrondCounts_message = paste0("BrondCounts and GGIR successfully completed at ", Sys.time(), " For example, see ", 
                                                   expected_outputdir_brondcounts, " and ",
                                                   expected_ggiroutput_file)
                } else {
                  GGIRBrondCounts_message = "BrondCounts unsuccessful"
                }
              } else {
                GGIRBrondCounts_message = paste0("GGIR successfully completed at ", Sys.time(), " For example, see ", expected_ggiroutput_file)
              }
            } else {
              GGIRBrondCounts_message = paste0("GGIR successfully completed at ", Sys.time(), " For example, see ", expected_ggiroutput_file)
            }
          } 
        }
      }
      return(GGIRBrondCounts_message)
    })
    
    # Apply PALMSpy after button is pressed ---------------------------------
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
          cnt_files_available = length(dir(path = count_file_location, pattern = ".csv", recursive = FALSE, full.names = FALSE)) > 0
          if (cnt_files_available == TRUE) {
            if (gps_files_available ==  TRUE) {
              ready_to_run_palsmpy = TRUE
            }
          } else {
            PALMSpy_message = paste0("No count files found in ", count_file_location)
          }
        } else {
          PALMSpy_message = paste0("Folder that is supposed to hold count files does not exist: ", 
                                   count_file_location, "First run GGIR and BrondCounts.")
        }
      }
      if (ready_to_run_palsmpy == TRUE) {
        id_palmspy = showNotification("PALMSpy in progress ...", type = "message", duration = NULL, closeButton = FALSE)
        waiter <- waiter::Waiter$new(id = "start_palmspy", html = waiter::spin_throbber())$show()
        on.exit(waiter$hide())
        on.exit(removeNotification(id_palmspy), add = TRUE)
        PALMSpy_R(gps_path = global$gps_in, acc_path = count_file_location,
                  output_path = global$data_out, config_file = configfilePALMSpy())
      
        # Now check whether results are correctly generated:
        expected_palmspy_results_dir = paste0(global$data_out,"/PALMSpy_output")
        # inputcheck = paste0("check gps, ",dir.exists(global$gps_in), " cnt ", dir.exists(count_file_location),
        #                     " out ", dir.exists(global$data_out), " conf ", file.exists(configfilePALMSpy()),
        #                     " palmspyoutput", dir.exists(expected_palmspy_results_dir))
        if (dir.exists(expected_palmspy_results_dir) == TRUE) {
          PALMSpy_message = paste0("PALMSpy completed at ", Sys.time(), " See ", expected_palmspy_results_dir)
        } else {
          PALMSpy_message = paste0("PALMSpy unsuccessful? Not able to find ", expected_palmspy_results_dir)
        }
      }
      return(PALMSpy_message)
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
    # If button pressed send message to UI about success ----------------
    output$ggir_end_message <- renderText({
      message = runGGIR() #ifelse(runGGIR() == TRUE,
                       # yes = paste0("Processing succesful ", Sys.time()),
                       # no = paste0("Processing unsuccesful ", Sys.time()))
    })
    output$palmspy_end_message <- renderText({
      message = runPALMSpy() #ifelse(runPALMSpy() == TRUE,
                       # yes = paste0("Processing succesful ", Sys.time()),
                       # no = paste0("Processing unsuccesful ", Sys.time()))
    })
  }
  # Run the application 
  shinyApp(ui, server)
}