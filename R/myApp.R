#' myApp
#'
#' @param homedir character to specify path to home directory
#' @param envConda character to specify path conda environment used for PALMSpy
#' @param ... No input needed, function runs the app
#' @return no object is returned, just an app
#' @import shiny
#' @import shinyFiles
#' @importFrom callr r_bg
#' @export

# pkgload::load_all("."); HabitusGUI::myApp(homedir="~/projects/fontys") 
# HabitusGUI::myApp(homedir="~/projects")
# pkgload::load_all("."); myApp(homedir="D:/Dropbox/Work/sharedfolder/DATA/Habitus")
# roxygen2::roxygenise()


# create temp log file

myApp <- function(homedir=getwd(), envConda = "~/miniconda3/bin/conda", ...) {
  stdout_GGIR_tmp <- tempfile(fileext = ".log")
  stdout_palmsplusr_tmp <- tempfile(fileext = ".log")
  stdout_hbGPS_tmp <- tempfile(fileext = ".log")
  stdout_PALMSpy_tmp <- tempfile(fileext = ".log")
  mylog_GGIR <- shiny::reactiveFileReader(500, NULL, stdout_GGIR_tmp, readLines, warn = FALSE)
  mylog_palmsplusr <- shiny::reactiveFileReader(500, NULL, stdout_palmsplusr_tmp, readLines, warn = FALSE)
  mylog_hbGPS <- shiny::reactiveFileReader(500, NULL, stdout_hbGPS_tmp, readLines, warn = FALSE)
  mylog_PALMSpy <- shiny::reactiveFileReader(500, NULL, stdout_PALMSpy_tmp, readLines, warn = FALSE)
  
  ui <- function() {
    fluidPage(
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
                                                       "Sleep Diary (in GGIR compatible .csv format)",
                                                       "previously generated PALMS(py) output",
                                                       "previously generated GGIR time series output",
                                                       "previously generated hbGPS output"),
                                    choiceValues = list("AccRaw", "ACount", "GPS", "GIS", "SleepDiary",
                                                        "PALMSpy_out", "GGIR_out", "hbGPS_out"), width = '100%'),
                 # Only show more check boxs if user specified available data sufficient for any of the tools
                 conditionalPanel(condition = paste0("input.availabledata.indexOf(`AccRaw`) > -1  || ", # GGIR
                                                     "(input.availabledata.indexOf(`ACount`) > -1 && ", # PALMSpy
                                                     "input.availabledata.indexOf(`GPS`) > -1) || ",
                                                     "(input.availabledata.indexOf(`GGIR_out`) > -1 && ", # hbGPS variant 1
                                                     "input.availabledata.indexOf(`GPS`) > -1) || ",
                                                     "(input.availabledata.indexOf(`AccRaw`) > -1 && ", # hbGPS variant 2
                                                     "input.availabledata.indexOf(`GPS`) > -1) || ",
                                                     "(input.availabledata.indexOf(`ACount`) > -1 && ", # palmsplusr variant 1
                                                     "input.availabledata.indexOf(`GPS`) > -1 && ",
                                                     "input.availabledata.indexOf(`GIS`) > -1) || ", 
                                                     "(input.availabledata.indexOf(`AccRaw`) > -1 && ", # palmsplusr variant 2
                                                     "input.availabledata.indexOf(`GPS`) > -1 && ",
                                                     "input.availabledata.indexOf(`GIS`) > -1) || ", 
                                                     "((input.availabledata.indexOf(`hbGPS_out`) > -1 || ", #palmsplusr variant 3
                                                     "input.availabledata.indexOf(`PALMSpy_out`) > -1)  && ", 
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
                                                                        "CountConverter (R package GGIR + actilifecounts)",
                                                                        "PALMSpy (Python library)",
                                                                        "hbGPS (R package)",
                                                                        "palmsplusr (R package)"),
                                                     choiceValues = list("GGIR", "CountConverter", "PALMSpy", "hbGPS", "palmsplusr"), width = '100%')
                 ), 
                 hr(),
                 actionButton("page_12", "next",  style = "position:absolute;right:1em;"),
                 p("\n"),
                 fluidRow(
                   column(1, offset = 0,
                          actionButton("restart_1", "restart", class = "btn-danger")  
                   )
                 )
        ),
        tabPanel("page_2",
                 fluidRow(column(8, div(h1("Habitus - Data selection"), style = "height:50px")),
                          column(4, div(imageOutput("logo_page2"), style = "height:50px;float:right"))
                 ),
                 p("\n"),
                 # Select input folder raw accelerometer data if raw data is available and GGIR is planned------------------
                 conditionalPanel(condition = paste0("input.availabledata.indexOf(`AccRaw`) > -1 && ",
                                                     "(input.tools.includes(`GGIR`) || ",
                                                     "input.tools.includes(`CountConverter`))"),
                                  shinyFiles::shinyDirButton("rawaccdir", label = "Raw accelerometry data directory...",
                                                             title = "Select raw accelerometer data directory"),
                                  verbatimTextOutput("rawaccdir", placeholder = TRUE),
                                  hr()
                 ),
                 # Select input folder count accelerometer data if count data is available and PALMSpy is planned------------------
                 # if not then count data will have to be estimated from the raw data, but we do not bother user
                 # with questions where it should be stored
                 conditionalPanel(condition = "input.availabledata.indexOf(`ACount`) > -1 && input.tools.includes(`PALMSpy`)",
                                  shinyFiles::shinyDirButton("countaccdir", label = "Count accelerometry data directory...",
                                                             title = "Select count accelerometer data directory"),
                                  verbatimTextOutput("countaccdir", placeholder = TRUE),
                                  hr()
                 ),
                 # Select input folder gps data -----------------------------------
                 conditionalPanel(condition = paste0("input.availabledata.indexOf(`GPS`) > -1 && ",
                                                     "(input.tools.includes(`PALMSpy`) || ",
                                                     "input.tools.includes(`hbGPS`))"),
                                  shinyFiles::shinyDirButton("gpsdir", label = "GPS data directory...",
                                                             title = "Select GPS data directory"),
                                  verbatimTextOutput("gpsdir", placeholder = TRUE),
                                  hr()
                 ),
                 # Select input folder GIS data and GIS linkage file -----------------------------------
                 conditionalPanel(condition = "input.availabledata.indexOf(`GIS`) > -1 && input.tools.includes(`palmsplusr`)",
                                  shinyFiles::shinyDirButton("gisdir", label = "GIS data directory...",
                                                             title = "Select GIS data directory"),
                                  verbatimTextOutput("gisdir", placeholder = TRUE),
                                  uiOutput("uiSelectedGisdir"),
                                  hr(),
                                  shinyFiles::shinyFilesButton("gislinkfile", label = "GIS linkage file...",
                                                               title = "Select GIS linkage file", multiple = FALSE),
                                  verbatimTextOutput("gislinkfile", placeholder = TRUE),
                                  hr()
                 ),
                 # Select input folder PALMSpy output data -----------------------------------
                 conditionalPanel(condition = paste0("input.availabledata.indexOf(`PALMSpy_out`) > -1 && ",
                                                     "input.tools.includes(`palmsplusr`) && !input.tools.includes(`PALMSpy`)"),
                                  shinyFiles::shinyDirButton("palmspyoutdir", label = "Previously generated PALMS(py) output directory...",
                                                             title = "Select PALMS(py) output directory"),
                                  verbatimTextOutput("palmspyoutdir", placeholder = TRUE),
                                  hr()
                 ),
                 # Select input folder GGIR output data -----------------------------------
                 conditionalPanel(condition = paste0("input.availabledata.indexOf(`GGIR_out`) > -1 && ",
                                                     "input.tools.includes(`hbGPS`)"),
                                  shinyFiles::shinyDirButton("ggiroutdir", label = "Previously generated GGIR output directory...",
                                                             title = "Select GGIR output directory"),
                                  verbatimTextOutput("ggiroutdir", placeholder = TRUE),
                                  hr()
                 ),
                 # Select input folder hbGPS output data -----------------------------------
                 conditionalPanel(condition = paste0("input.availabledata.indexOf(`hbGPS_out`) > -1 && ",
                                                     "input.tools.includes(`palmsplusr`) && !input.tools.includes(`hbGPS`)"),
                                  shinyFiles::shinyDirButton("hbgpsoutdir", label = "Previously generated hbGPS output directory...",
                                                             title = "Select hbGPS output directory"),
                                  verbatimTextOutput("hbgpsoutdir", placeholder = TRUE),
                                  hr()
                 ),
                 # Upload sleep diary ----------------------------------------------------
                 conditionalPanel(condition = "input.availabledata.indexOf(`SleepDiary`) > -1 && input.tools.includes(`GGIR`)",
                                  shinyFiles::shinyFilesButton("sleepdiaryfile", label = "Sleepdiary file...",
                                                               title = "Select sleep diary file", multiple = FALSE),
                                  verbatimTextOutput("sleepdiaryfile", placeholder = TRUE),
                                  hr()
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
                 conditionalPanel(condition = "input.availabledata.indexOf(`GIS`) > -1 && input.tools.includes(`palmsplusr`)",
                                  strong(textInput("dataset_name", label = "Give your dataset a name:", value = "", width = '100%')),
                 ),
                 hr(),
                 actionButton("page_21", "prev"),
                 actionButton("page_23", "next",  style = "position:absolute;right:1em;"),
                 p("\n"),
                 fluidRow(
                   column(1, offset = 0,
                          actionButton("restart_2", "restart", class = "btn-danger")  
                   )
                 )
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
                 conditionalPanel(condition = "input.tools.includes('CountConverter')",
                                  h2("CountConverter"),
                                  p("No parameters are needed for the CountConverter"),
                                  hr()
                 ),
                 conditionalPanel(condition = "input.tools.includes('PALMSpy')",
                                  h2("PALMSpy"),
                                  modConfigUI("edit_palmspy_config"),
                                  hr()
                 ),
                 conditionalPanel(condition = "input.tools.includes('hbGPS')",
                                  h2("hbGPS"),
                                  modConfigUI("edit_hbGPS_config"),
                                  hr()
                 ),
                 conditionalPanel(condition = "input.tools.includes('palmsplusr')",
                                  h2("palmsplusr"),
                                  modConfigUI("edit_palmsplusr_config"),
                                  hr()
                 ),
                 hr(),
                 actionButton("page_32", "prev"),
                 actionButton("page_34", "next", style = "position:absolute;right:1em;"),
                 p("\n"),
                 fluidRow(
                   column(1, offset = 0,
                          actionButton("restart_3", "restart", class = "btn-danger")  
                   )
                 )
        ),
        tabPanel("page_4",
                 # Button to start analysis ---------------------------------------------
                 fluidRow(column(8, div(h1("Habitus - Analyses"), style = "height:50px")),
                          column(4, div(imageOutput("logo_page4"), style = "height:50px;float:right"))
                 ),
                 p("\n"),
                 p("\n"),
                 span(h4(textOutput("recommendorder")), style = "color:purple"),
                 # hr(),
                 p("\n"),
                 conditionalPanel(condition = paste0("input.tools.includes('GGIR') || ",
                                                     "input.tools.includes('CountConverter')"),
                                  conditionalPanel(condition =
                                                     paste0("input.tools.indexOf(`GGIR`) > -1  && ",
                                                            "input.tools.indexOf(`CountConverter`) > -1"), 
                                                   h3("GGIR and CountConverter:")
                                  ),
                                  conditionalPanel(condition = "input.tools.indexOf(`CountConverter`) == -1", 
                                                   h3("GGIR:")
                                  ),
                                  shinyjs::useShinyjs(),
                                  actionButton("start_ggir", "Start analysis", width = '300px'),
                                  p("\n"),
                                  verbatimTextOutput("mylog_GGIR"),
                                  tags$head(tags$style("#mylog_GGIR{color:darkblue; font-size:12px; font-style:italic; 
overflow-y:scroll; max-height: 300px; background: ghostwhite;}")),
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
overflow-y:scroll; max-height: 300px; background: ghostwhite;}")),
                                  p("\n"),
                                  htmlOutput("palmspy_end_message"),
                                  p("\n"),
                                  DT::dataTableOutput("PALMSpy_file1"),
                                  hr()
                 ),
                 conditionalPanel(condition = "input.tools.includes('hbGPS')",
                                  h3("hbGPS:"),
                                  shinyjs::useShinyjs(),
                                  actionButton("start_hbGPS", "Start analysis", width = '300px'),
                                  p("\n"),
                                  verbatimTextOutput("mylog_hbGPS"),
                                  tags$head(tags$style("#mylog_hbGPS{color:darkblue; font-size:12px; font-style:italic; 
overflow-y:scroll; max-height: 300px; background: ghostwhite;}")),
                                  p("\n"),
                                  htmlOutput("hbGPS_end_message"),
                                  p("\n"),
                                  DT::dataTableOutput("hbGPS_file1"),
                                  hr()
                 ),
                 conditionalPanel(condition = "input.tools.includes('palmsplusr')",
                                  h3("palmsplusr:"),
                                  shinyjs::useShinyjs(),
                                  actionButton("start_palmsplusr", "Start analysis", width = '300px'),
                                  p("\n"),
                                  verbatimTextOutput("mylog_palmsplusr"),
                                  tags$head(tags$style("#mylog_palmsplusr{color:darkblue; font-size:12px; font-style:italic; 
overflow-y:scroll; max-height: 300px; background: ghostwhite;}")),
                                  p("\n"),
                                  htmlOutput("palmsplusr_end_message"),
                                  p("\n"),
                                  DT::dataTableOutput("palmsplusr_file1"),
                                  hr()
                 ),
                 actionButton("page_43", "prev"),
                 p("\n"),
                 fluidRow(
                   column(1, offset = 0,
                          actionButton("restart_4", "restart app", class = "btn-danger")  
                   )
                 )
                 
        )
      )
    )
  }
  
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
    
    # load previous values if available
    values = c()
    if (file.exists("./HabitusGUIbookmark.RData")) load("./HabitusGUIbookmark.RData")
    if (length(values) > 0) {
      values$wizard = "page_1"
      lapply(names(values),
             function(x) session$sendInputMessage(x, list(value = values[[x]]))
      )
    }
    
    # previously selected directories -----
    if (length(values$rawaccdir) < 2) selectedRawaccdir = c() else selectedRawaccdir = paste(values$rawaccdir$path, collapse = .Platform$file.sep)
    if (length(values$countaccdir) < 2) selectedCountaccdir = c() else selectedCountaccdir = paste(values$countaccdir$path, collapse = .Platform$file.sep)
    if (length(values$gpsdir) < 2) selectedGpsdir = c() else selectedGpsdir = paste(values$gpsdir$path, collapse = .Platform$file.sep)
    if (length(values$gisdir) < 2) selectedGisdir = c() else selectedGisdir = paste(values$gisdir$path, collapse = .Platform$file.sep)
    if (length(values$gislinkfile) < 2) selectedGislinkfile = c() else selectedGislinkfile = paste(values$gislinkfile$path, collapse = .Platform$file.sep)
    if (length(values$palmspyoutdir) < 2) selectedPalmspyoutdir = c() else selectedPalmspyoutdir = paste(values$palmspyoutdir$path, collapse = .Platform$file.sep)
    if (length(values$ggiroutdir) < 2) selectedGgiroutdir = c() else selectedGgiroutdir = paste(values$ggiroutdir$path, collapse = .Platform$file.sep)
    if (length(values$hbgpsoutdir) < 2) selectedHbgpsoutdir = c() else selectedHbgpsoutdir = paste(values$hbgpsoutdir$path, collapse = .Platform$file.sep)
    if (length(values$sleepdiaryfile) < 2) selectedSleepdiaryfile = c() else selectedSleepdiaryfile = paste(values$sleepdiaryfile$path, collapse = .Platform$file.sep)
    if (length(values$outputdir) < 2) selectedOutputdir = c() else selectedOutputdir = paste(values$outputdir$path, collapse = .Platform$file.sep)

    observeEvent(input$page_12, {
      values_tmp = lapply(reactiveValuesToList(input), unclass)
      if (exists("values") & length(values) > 10) {
        # in order not to overwrite previous definition of directories
        overwrite = which(names(values) %in% names(values_tmp))
        not_overwrite = grep("dir", names(values))
        overwrite = overwrite[-which(overwrite %in% not_overwrite)]
        for (i in 1:length(values[overwrite])) {
          values[overwrite[i]] = values_tmp[grep(names(values[overwrite[i]]),
                                                 names(values_tmp))]
        }
      } else {
        values = values_tmp
      }
      save(values, file = "./HabitusGUIbookmark.RData")
      if (length(input$availabledata) == 0 & length(input$tools) == 0) {
        showNotification("Select data type(s) to be analysed", type = "error")
      } else {
        if (length(input$tools) == 0) {
          showNotification("Select at least one tool", type = "error")
        } else {
          if ("GGIR" %in% input$tools == TRUE & "AccRaw" %in% input$availabledata == FALSE) {
            showNotification("GGIR not possible without access to raw accelerometer data", type = "error")
          } else {
            if ("PALMSpy" %in% input$tools == TRUE & all(c("AccRaw", "ACount", "GPS") %in% input$availabledata == FALSE)) {
              showNotification("PALMSpy not possible without access to Accelerometer and GPS data", type = "error")
            } else {
              if ("hbGPS" %in% input$tools == TRUE & all(c("GPS", "GGIR_out") %in% input$availabledata == FALSE)) {
                showNotification("hbGPS not possible without access to GPS data and GGIR times series output", type = "error")
              } else {
                if ("palmsplusr" %in% input$tools == TRUE & "GIS" %in% input$availabledata == FALSE) {
                  showNotification("palmsplusr not possible without access to GIS data", type = "error")
                } else {
                  if ("palmsplusr" %in% input$tools == TRUE & ("PALMSpy_out" %in% input$availabledata == FALSE &
                                                               "hbGPS_out" %in% input$availabledata == FALSE &
                                                               "GPS" %in% input$availabledata == FALSE & all(c("AccRaw", "ACount") %in% input$availabledata == FALSE))) {
                    showNotification(paste0("palmsplusr requires either previously",
                                            " generated PALMS(py) or hbGPS output,",
                                            " or GPS and Accelerometer data such",
                                            " that either PALMSpyor hbGPS can be",
                                            " run"), type = "error")
                  } else {
                    if ("CountConverter" %in% input$tools == TRUE & "AccRaw" %in% input$availabledata == FALSE) {
                      showNotification("CountConverter not possible without access to raw accelerometer data", type = "error")
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
      # Previous selection of directories
      prevPathNames = c("rawaccdir", "countaccdir", "sleepdiaryfile",
                        "configfileGGIR", "configfilepalmsplusr", "configfilehbGPS",
                        "gpsdir", "gisdir", "gislinkfile",
                        "palmspyoutdir", "hbgpsoutdir", "ggiroutdir", "outputdir")
      prevPathNames = prevPathNames[which(prevPathNames %in% names(values))]
      prevPaths = values[prevPathNames]
      values_tmp = lapply(reactiveValuesToList(input), unclass)
      if (!is.null(prevPaths)) { # if first run, skip this part of the code
        for (i in 1:length(prevPaths)) {
          if (length(values_tmp[[prevPathNames[i]]]) == 1) {
            if (values_tmp[[prevPathNames[i]]] == 0) {
              values_tmp[[prevPathNames[i]]] = prevPaths[[i]]
            }
          }
        }
      }
      # do not remove values previously available
      extra_values = names(values)[which(!names(values) %in% names(values_tmp))]
      if (length(extra_values) > 0) {
        for (i in 1:length(extra_values)) {
          values_tmp[[length(values_tmp) + 1]] = values[[extra_values[i]]]
          names(values_tmp)[length(values_tmp)] = extra_values[i]
        }
      }
      values = values_tmp
      save(values, file = "./HabitusGUIbookmark.RData")
      # -----
      if ("AccRaw" %in% input$availabledata & "GGIR" %in% input$tools & as.character(input$rawaccdir)[1] == "0" & is.null(selectedRawaccdir)) {
        showNotification("Select raw accelerometer data directory", type = "error")
      } else {
        if ("AccRaw" %in% input$availabledata & "CountConverter" %in% input$tools & as.character(input$rawaccdir)[1] == "0" & is.null(selectedRawaccdir)) {
          showNotification("Select raw accelerometer data directory", type = "error")
        } else {
          if ("ACount" %in% input$availabledata & "PALMSpy" %in% input$tools & as.character(input$countaccdir)[1] == "0" & is.null(selectedCountaccdir)) {
            showNotification("Select count accelerometer data directory", type = "error")
          } else {
            if ("GPS" %in% input$availabledata & "PALMSpy" %in% input$tools & as.character(input$gpsdir)[1] == "0" & is.null(selectedGpsdir)) {
              showNotification("Select GPS data directory", type = "error")
            } else {
              current_sleepdiary = as.character(parseFilePaths(c(home = homedir), input$sleepdiaryfile)$datapath)
              if ("SleepDiary" %in% input$availabledata & "GGIR" %in% input$tools &
                  length(current_sleepdiary) == 0 & is.null(selectedSleepdiaryfile)) { 
                showNotification("Select sleepdiary file", type = "error")
              } else {
                current_gislinkfile = as.character(parseFilePaths(c(home = homedir), input$gislinkfile)$datapath)
                if ("GIS" %in% input$availabledata &
                    "palmsplusr" %in% input$tools &
                    (as.character(input$gisdir)[1] == "0" |
                     length(current_gislinkfile) == 0) & is.null(selectedGisdir)) {
                  showNotification("Select GIS data directory and GIS linkage file", type = "error")
                } else {
                  if ("PALMSpy_out" %in% input$availabledata & "palmsplusr" %in% input$tools & as.character(input$palmspyoutdir)[1] == "0" & is.null(selectedPalmspyoutdir)) {
                    showNotification("Select previously generated PALMS(py) output directory", type = "error")
                  } else {
                    if ("hbGPS_out" %in% input$availabledata & "palmsplusr" %in% input$tools & as.character(input$hbgpsoutdir)[1] == "0" & is.null(selectedHbgpsoutdir)) {
                      showNotification("Select previously generated hbGPS output directory", type = "error")
                    } else {
                      if ("GGIR_out" %in% input$availabledata & "hbGPS" %in% input$tools & as.character(input$ggiroutdir)[1] == "0" & is.null(selectedHbgpsoutdir)) {
                        showNotification("Select previously generated hbGPS output directory", type = "error")
                      } else {
                        switch_page(3)
                      }
                    }
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
      # Previous selection of directories
      prevPathNames = c("rawaccdir", "countaccdir", "sleepdiaryfile",
                        "gpsdir", "gisdir", "gislinkfile",
                        "palmspyoutdir", "hbgpsoutdir", "ggiroutdir", "outputdir")
      prevPathNames = prevPathNames[which(prevPathNames %in% names(values))]
      prevPaths = values[prevPathNames]
      values_tmp = lapply(reactiveValuesToList(input), unclass)
      if (!is.null(prevPaths)) { # if first run, skip this part of the code
        for (i in 1:length(prevPaths)) {
          if (length(values_tmp[[prevPathNames[i]]]) == 1) {
            if (values_tmp[[prevPathNames[i]]] == 0) {
              values_tmp[[prevPathNames[i]]] = prevPaths[[i]]
            }
          }
        }
      }
      # do not remove values previously available
      extra_values = names(values)[which(!names(values) %in% names(values_tmp))]
      if (length(extra_values) > 0) {
        for (i in 1:length(extra_values)) {
          values_tmp[[length(values_tmp) + 1]] = values[[extra_values[i]]]
          names(values_tmp)[length(values_tmp)] = extra_values[i]
        }
      }
      values = values_tmp
      # -----
      configs_ready = TRUE
      config_from = config_to = NULL
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
      if ("hbGPS" %in% input$tools) {
        if (length(paste0(configfilehbGPS())) == 0) {
          configs_ready = FALSE
        }
      }
      if ("palmsplusr" %in% input$tools) {
        if (length(paste0(configfilepalmsplusr())) == 0) {
          configs_ready = FALSE
        }
      }
      if (configs_ready == TRUE) {
        showNotification("Saving configuration file(s) to output folder", type = "message", duration = 2)
        
        copyFile = function(from, to) {
          # Copies configuration file to output folder if:
          # - from and to are not the same path
          # - filesize of from is larger than 0
          if (from != to) {
            fileSize = file.info(from)$size
            if (fileSize > 0) { # only copy if filesize is more than 0
              file.copy(from = from, to = to, 
                        overwrite = TRUE, recursive = FALSE, copy.mode = TRUE)
            }
          }
          return(to)
        }
        if ("GGIR" %in% input$tools) {
          config_from = cleanPath(configfileGGIR())
          config_to = cleanPath(paste0(global$data_out, "/config.csv"))
          if (length(config_from) > 0) {
            values$configfileGGIR = copyFile(from = config_from, to = config_to)
          } 
          if (length(sleepdiaryfile()) > 0) {
            if (sleepdiaryfile() != 0) {
              diary_from = cleanPath(as.character(parseFilePaths(c(home = homedir), sleepdiaryfile())$datapath))
              diary_to = cleanPath(paste0(global$data_out, "/sleepdiary.csv"))
              if (length(config_from) > 0) {
                sleepdiaryfile_local = copyFile(from = diary_from, to = diary_to)
              } else  {
                sleepdiaryfile_local = c()
              }
            } else {
              sleepdiaryfile_local = NULL
            }
            if (!is.null(sleepdiaryfile_local)) {
              values$sleepdiaryfile = paste0(global$data_out, "/sleepdiary.csv")
            }
          }
        }
        if ("PALMSpy" %in% input$tools) {
          config_from = cleanPath(configfilePALMSpy())
          config_to = cleanPath(paste0(global$data_out, "/config.json"))
          if (length(config_from) > 0) {
            values$configfilePALMSpy =  copyFile(from = config_from, to = config_to)
          }
        }
        if ("hbGPS" %in% input$tools) {
          config_from = cleanPath(configfilehbGPS())
          config_to = cleanPath(paste0(global$data_out, "/config_hbGPS.csv"))
          if (length(config_from) > 0) {
            values$configfilehbGPS = copyFile(from = config_from, to = config_to)
          }
        }
        if ("palmsplusr" %in% input$tools) {
          config_from = cleanPath(configfilepalmsplusr())
          config_to = cleanPath(paste0(global$data_out, "/config_palmsplusr.csv"))
          if (length(config_from) > 0) {
            values$configfilepalmsplusr = copyFile(from = config_from, to = config_to)
          }
        }
        save(values, file = "./HabitusGUIbookmark.RData")
        switch_page(4)
      } else {
        showNotification("Select configuration file(s)", type = "error")
      }
    })
    observeEvent(input$page_43, switch_page(3))
    
    # restart app ----------------
    observeEvent(input$restart_1, {
      switch_page(1)
      if (file.exists("./HabitusGUIbookmark.RData")) file.remove("./HabitusGUIbookmark.RData")
      session$reload()
    })
    observeEvent(input$restart_2, {
      switch_page(1)
      values = c()
      if (file.exists("./HabitusGUIbookmark.RData")) file.remove("./HabitusGUIbookmark.RData")
      session$reload()
    })
    observeEvent(input$restart_3, {
      switch_page(1)
      values = c()
      if (file.exists("./HabitusGUIbookmark.RData")) file.remove("./HabitusGUIbookmark.RData")
      session$reload()
    })
    observeEvent(input$restart_4, {
      switch_page(1)
      values = c()
      if (file.exists("./HabitusGUIbookmark.RData")) file.remove("./HabitusGUIbookmark.RData")
      session$reload()
    })
    
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
      
      if ("GPS" %in% x & any(c("AccRaw", "ACount", "GGIR_out") %in% x)) researchgoals = c(researchgoals, "Trips", "QC")
      if (all(c("GPS", "GIS") %in% x) & any(c("AccRaw", "ACount") %in% x)) researchgoals = c(researchgoals, "Environment", "QC")
      if (all(c("PALMSpy_out", "GIS") %in% x)) researchgoals = c(researchgoals, "Environment", "QC")
      if (all(c("hbGPS_out", "GIS") %in% x)) researchgoals = c(researchgoals, "Environment", "QC")
      if (all(c("GGIR_out", "GIS") %in% x)) researchgoals = c(researchgoals, "Environment", "QC")
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
        researchgoalsLabel = "What is your research interest?"
      }
      
      # check previously selected
      researchgoalsSelected = c()
      if (exists("values")) {
        researchgoals = values$researchgoals
        researchgoalsSelected = researchgoals[which(researchgoals %in% reasearchgoalsValues)]
      }
      
      # Update checkbox
      updateCheckboxGroupInput(session, "researchgoals",
                               label = researchgoalsLabel,
                               choiceNames = researchgoalsNames,
                               choiceValues = reasearchgoalsValues,
                               selected = researchgoalsSelected)
    })
    
    # Identify pipeline with tools to be used and send to UI
    proposed_pipeline <- reactive(identify_tools(datatypes = input$availabledata, goals = input$researchgoals)$tools_needed)
    output$pipeline <- renderText({
      if (all(c("hbGPS", "PALMSpy") %in% proposed_pipeline()) == TRUE) {
        message = paste0("Proposed software pipeline: ",
                         paste0(grep(pattern = "PALMSpy|CountConverter", x = proposed_pipeline(), invert = TRUE, value = TRUE), collapse = " + "),
                         " OR replace hbGPS by ", 
                         paste0(grep(pattern = "PALMSpy|CountConverter", x = proposed_pipeline(), invert = FALSE, value = TRUE), collapse = " + "))
      } else {
        message = paste0("Proposed software pipeline: ", paste0(proposed_pipeline(), collapse = " + "))
      }
      ifelse(length(proposed_pipeline()) == 0,
             yes = "--> Tick boxes above according to the analysis you would like to do",
             no = message)
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
    shinyDirChoose(input, 'hbgpsoutdir',  roots = c(home = homedir)) # Allow for old output to be used as input
    shinyDirChoose(input, 'ggiroutdir',  roots = c(home = homedir)) # Allow for old output to be used as input
    shinyDirChoose(input, 'outputdir',  roots = c(home = homedir))
    shinyFileChoose(input, 'sleepdiaryfile',  roots = c(home = homedir))
    
    
    # Capture provided directories in reactive object ----------------------------
    rawaccdir <- reactive(input$rawaccdir)
    countaccdir <- reactive(input$countaccdir)
    gpsdir <- reactive(input$gpsdir)
    gisdir <- reactive(input$gisdir)
    gislinkfile <- reactive(input$gislinkfile)
    palmspyoutdir <- reactive(input$palmspyoutdir) # Allow for old output to be used as input
    hbgpsoutdir <- reactive(input$hbgpsoutdir) # Allow for old output to be used as input
    ggiroutdir <- reactive(input$ggiroutdir) # Allow for old output to be used as input
    outputdir <- reactive(input$outputdir)
    sleepdiaryfile <- reactive(input$sleepdiaryfile) #$datapath
    
    # Create global with directories and give it default values -------
    if (exists("values")) {
      getPrevPath = function(dirname, ifEmpty = NULL, homedir, values) {
        out = NULL
        if (length(values[[dirname]]) == 1) {
          if (is.null(ifEmpty)) {
            out = NULL
          } else {
            out = homedir
          }
        } else if ("path" %in% names(values[[dirname]])) {
          out_tmp = paste(values[[dirname]]$path, collapse = .Platform$file.sep)
          out = paste(homedir, out_tmp, sep = .Platform$file.sep)
        } else if ("files" %in% names(values[[dirname]])) {
          out_tmp = paste(unlist(values[[dirname]]$files), collapse = .Platform$file.sep)
          out = paste(homedir, out_tmp, sep = .Platform$file.sep)
        }
        return(out)
      }
      # get previous directories if exist
      data_out = getPrevPath(dirname = "outputdir", ifEmpty = "homedir", homedir = homedir, values)
      raw_acc_in = getPrevPath(dirname = "rawaccdir", ifEmpty = NULL, homedir = homedir, values)
      count_acc_in = getPrevPath(dirname = "countaccdir", ifEmpty = NULL, homedir = homedir, values)
      gps_in = getPrevPath(dirname = "gpsdir", ifEmpty = NULL, homedir = homedir, values)
      gis_in = getPrevPath(dirname = "gisdir", ifEmpty = NULL, homedir = homedir, values)
      gislinkfile_in = getPrevPath(dirname = "gislinkfile", ifEmpty = NULL, homedir = homedir, values)
      palmspyout_in = getPrevPath(dirname = "palmspyoutdir", ifEmpty = NULL, homedir = homedir, values)
      hbgpsout_in = getPrevPath(dirname = "hbgpsoutdir", ifEmpty = NULL, homedir = homedir, values)
      ggirout_in = getPrevPath(dirname = "ggiroutdir", ifEmpty = NULL, homedir = homedir, values)
      sleepdiary_file = getPrevPath(dirname = "sleepdiaryfile", ifEmpty = NULL, homedir = homedir, values)
    } else {
      data_out = homedir
      raw_acc_in = count_acc_in = gps_in = gis_in = gislinkfile_in = palmspyout_in = hbgpsout_in = sleepdiary_file = NULL
    }
    global <- reactiveValues(data_out = cleanPath(data_out),
                             raw_acc_in = cleanPath(raw_acc_in),
                             count_acc_in = cleanPath(count_acc_in),
                             gps_in = cleanPath(gps_in),
                             gis_in = cleanPath(gis_in),
                             gislinkfile_in = cleanPath(gislinkfile_in),
                             palmspyout_in = cleanPath(palmspyout_in),
                             hbgpsout_in = cleanPath(hbgpsout_in),
                             sleepdiaryfile = cleanPath(sleepdiary_file)) #, pipeline = NULL)
    
    
    
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
                   input$hbgpsoutdir # every time input$hbgpsoutdir updates ...
                 },
                 handlerExpr = { # ... we re-assign global$hbgpsout_in
                   if (!"path" %in% names(hbgpsoutdir())) return()
                   home <- normalizePath(homedir)
                   global$hbgpsout_in <-
                     file.path(home, paste(unlist(hbgpsoutdir()$path[-1]), collapse = .Platform$file.sep))
                 })
    observeEvent(ignoreNULL = TRUE,
                 eventExpr = {
                   input$ggiroutdir # every time input$ggiroutdir updates ...
                 },
                 handlerExpr = { # ... we re-assign global$ggirout_in
                   if (!"path" %in% names(ggiroutdir())) return()
                   home <- normalizePath(homedir)
                   global$ggirout_in <-
                     file.path(home, paste(unlist(ggiroutdir()$path[-1]), collapse = .Platform$file.sep))
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
    output$hbgpsoutdir <- renderText({
      global$hbgpsout_in
    })
    output$ggiroutdir <- renderText({
      global$ggirout_in
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
    configfilehbGPS <- modConfigServer("edit_hbGPS_config", tool = reactive("hbGPS"), homedir = homedir)
    configfilepalmsplusr <- modConfigServer("edit_palmsplusr_config", tool = reactive("palmsplusr"), homedir = homedir)
    
    
    #========================================================================
    # Apply GGIR / CountConverter after button is pressed
    #========================================================================
    runGGIR <- eventReactive(input$start_ggir, {

      GGIRCounts_message = ""
      
      if ("GGIR" %in% input$tools | "CountConverter" %in% input$tools) {
        GGIRCounts_message = ""
        # Basic check before running function:
        ready_to_run_ggirCounts = FALSE
        if (dir.exists(global$raw_acc_in)) {
          acc_files_available = length(dir(path = global$raw_acc_in, pattern = "csv|bin|gt3x|bin|cwa|wav", recursive = FALSE, full.names = FALSE)) > 0
          if (acc_files_available == TRUE) {
            ready_to_run_ggirCounts = TRUE
          } else {
            GGIRCounts_message = paste0("No count files found in ", global$raw_acc_in)
          }
        } else {
          GGIRCounts_message = paste0("Folder that is supposed to hold acceleration files does not exist: ", global$raw_acc_in)
        }
        # Only run function when checks are met:
        if (ready_to_run_ggirCounts == TRUE) {
          shinyjs::hide(id = "start_ggir")
          if ("CountConverter" %in% input$tools) {
            id_ggir = showNotification("GGIR and CountConverter in progress ...", type = "message", duration = NULL, closeButton = FALSE)
            do.Counts = TRUE
          } else {
            # this line makes that if user is trying to use a config defined in a previous
            # run, the data path is correctly defined
            config = read.csv(as.character(configfileGGIR()))
            config.Counts = config$value[which(config$argument == "do.neishabouricounts")]
            if (as.logical(config.Counts) == TRUE) {
              # if counts was not selected as a tool, but acc.metric was defined as 
              # NeishabouriCount, then turn do.Counts to TRUE
              id_ggir = showNotification("GGIR and CountConverter in progress ...", type = "message", duration = NULL, closeButton = FALSE)
              do.Counts = TRUE
            } else {
              id_ggir = showNotification("GGIR in progress ...", type = "message", duration = NULL, closeButton = FALSE)
              do.Counts = FALSE
            }
          }
          if (file.exists(paste0(global$data_out, "/sleepdiary.csv"))) { # because this is not a global variable
            sleepdiaryfile_local = paste0(global$data_out, "/sleepdiary.csv")
          } else {
            sleepdiaryfile_local = c()
          }
          # on.exit(file.copy(from = stdout_GGIR_tmp, to = logfile, overwrite = TRUE))
          
          write.table(x = NULL, file = stdout_GGIR_tmp) # initialise empty file
          output$mylog_GGIR <- renderText({
            paste(mylog_GGIR(), collapse = '\n')
          })
          # Start GGIR
          x_ggir <- r_bg(func = function(GGIRshiny, rawaccdir, outputdir, 
                                         sleepdiary, configfile, do.Counts){
            GGIRshiny(rawaccdir, outputdir, 
                      sleepdiary, configfile, do.Counts)
          },
          args = list(GGIRshiny = GGIRshiny,
                      rawaccdir = isolate(global$raw_acc_in),
                      outputdir = global$data_out, 
                      sleepdiary = sleepdiaryfile_local,
                      configfile = cleanPath(paste0(global$data_out, "/config.csv")),
                      do.Counts = do.Counts),
          stdout = "",
          stderr = "")
          
          # Expected location of log file
          logfile = paste0(isolate(global$data_out), "/GGIR.log")
          
          observe({
            if (x_ggir$poll_io(0)[["process"]] != "ready") {
              # Copy local log file to server to update progress log
              if (file.exists(logfile)) file.copy(from = logfile, to = stdout_GGIR_tmp, overwrite = TRUE)
              invalidateLater(2000)
            } else {
              # Copy local log file to server to update progress log
              if (file.exists(logfile)) file.copy(from = logfile, to = stdout_GGIR_tmp, overwrite = TRUE)
              on.exit(removeNotification(id_ggir), add = TRUE)
              
              # Delete Rscript that is created by GGIRshiny because user does not need this
              ggir_cmdline = paste0(global$data_out, "/ggir_cmdline.R")
              if (file.exists(ggir_cmdline)) {
                file.remove(ggir_cmdline)
              }
              
              # Now check whether results are correctly generated:
              expected_outputdir_ggir = paste0(global$data_out, "/output_", basename(global$raw_acc_in))
              expected_ggiroutput_file = paste0(global$data_out, "/output_", basename(global$raw_acc_in), "/results/part2_daysummary.csv")
              if (file.exists(expected_ggiroutput_file) == TRUE) { # checks whether ggir output was created
                if ("CountConverter" %in% input$tools) { # if CountConverter was suppoed to run
                  expected_outputdir_Counts = paste0(global$data_out, "/actigraph")
                  if (dir.exists(expected_outputdir_Counts) == TRUE) { # checks whether output dir was created
                    if (length(dir(expected_outputdir_Counts) > 0)) { # checks whether it has been filled with results
                      GGIRCounts_message = paste0(#"CountConverter and GGIR successfully completed at ", Sys.time(), 
                        "Output is stored in ", expected_outputdir_Counts, " and ",
                        expected_outputdir_ggir, 
                        "<br/>The table below shows the content of part2_daysummary.csv")
                      GGIRpart2 = read.csv(expected_ggiroutput_file)
                      output$GGIRpart2 <- DT::renderDataTable(GGIRpart2, options = list(scrollX = TRUE))
                    } else {
                      GGIRCounts_message = paste0("CountConverter unsuccessful. No file found inside ", expected_outputdir_Counts)
                    }
                  } else {
                    GGIRCounts_message = paste0("CountConverter unsuccessful. Dir ",expected_outputdir_Counts, " not found")
                  }
                } else {
                  GGIRCounts_message = paste0(#"GGIR successfully completed at ", Sys.time(), 
                    "Output is stored in: ",  #<br/>
                    expected_outputdir_ggir,
                    "<br/>The table below shows the content of part2_daysummary.csv:")
                  GGIRpart2 = read.csv(expected_ggiroutput_file, nrow = 100)
                  output$GGIRpart2 <- DT::renderDataTable(GGIRpart2, options = list(scrollX = TRUE))
                }
                output$ggir_end_message <- renderUI({
                  HTML(paste0(GGIRCounts_message))
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
                                   count_file_location, " First run GGIR and CountConverter.")
        }
      }
      if (ready_to_run_palsmpy == TRUE) {
        id_palmspy = showNotification("PALMSpy in progress ...", type = "message", duration = NULL, closeButton = FALSE)
        shinyjs::hide(id = "start_palmspy")
        
        write.table(x = NULL, file = stdout_PALMSpy_tmp) # initialise empty file
        
        output$mylog_PALMSpy <- renderText({
          paste(mylog_PALMSpy(), collapse = '\n')
        })
        
        
        # # Start PALMSpy
        x_palmspy <- r_bg(func = function(PALMSpyshiny, outputdir, gpsdir, count_file_location, envConda) {
          PALMSpyshiny(outputdir, gpsdir, count_file_location, envConda)
        },
        args = list(PALMSpyshiny = PALMSpyshiny,
                    outputdir = global$data_out,
                    gpsdir = global$gps_in,
                    count_file_location = count_file_location,
                    envConda = envConda),
        stdout = "",
        stderr = "")
        
        logfile = paste0(isolate(global$data_out), "/PALMSpy.log")
        
        observe({
          if (x_palmspy$poll_io(0)[["process"]] != "ready") {
            if (file.exists(logfile)) file.copy(from = logfile, to = stdout_PALMSpy_tmp, overwrite = TRUE)
            invalidateLater(2000)
          } else {
            file.copy(from = logfile, to = stdout_PALMSpy_tmp, overwrite = TRUE)
            on.exit(removeNotification(id_palmspy), add = TRUE)
            # When process is finished copy tmp log file to actual log file for user to see
            file.copy(from = stdout_PALMSpy_tmp, to = logfile, overwrite = TRUE)     
            # Now check whether results are correctly generated:
            expected_palmspy_results_dir = paste0(global$data_out,"/PALMSpy_output")
            if (dir.exists(expected_palmspy_results_dir)) {
              PALMSpy_message = paste0(#"PALMSpy completed at ", Sys.time(),
                "Output is stored in: ", expected_palmspy_results_dir) #<br/>
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
    # Apply hbGPS after button is pressed
    #========================================================================
    runhbGPS <- eventReactive(input$start_hbGPS, {
      hbGPS_message = ""
      if ("hbGPS" %in% input$tools) {
        hbGPS_message = "Error: Contact maintainer"
        # Basic check before running function:
        ready_to_run_hbGPS = FALSE
        # Check for GGIR output (two possible sources either from this run or from a previous run)
        if (dir.exists(global$ggirout_in)) {
          Nfiles_in_dir = length(dir(path = expected_ggir_results_dir, pattern = "csv", recursive = FALSE, full.names = FALSE))
          if (Nfiles_in_dir > 0) {
            # also check for GPS files
            if (dir.exists(global$gps_in)) {
              Nfiles_in_gpsdir = length(dir(path = global$gps_in, recursive = FALSE, full.names = FALSE))
              if (Nfiles_in_gpsdir == 0) {
                hbGPS_message = paste0("No files found in GPS folder: ", global$gps_in)
              } else {
                ready_to_run_hbGPS = TRUE
              }
            } else {
              hbGPS_message = paste0("Folder that is supposed to hold GPS files does not exist: ", global$gps_in)
            }
          } else {
            hbGPS_message = paste0("No files found in GGIR output folder: ", global$ggirout_in)
          }
        } else {
          hbGPS_message = paste0("Folder that is supposed to hold GGIR output files does not exist: ", global$ggirout_in)
        }
        # Only run function when checks are met:
        if (ready_to_run_hbGPS == TRUE) {
          shinyjs::hide(id = "start_hbGPS")
          id_palmsplusr = showNotification("hbGPS in progress ...", type = "message", duration = NULL, closeButton = FALSE)
          
          write.table(x = NULL, file = stdout_hbGPS_tmp) # initialise empty file
          output$mylog_hbGPS <- renderText({
            paste(mylog_hbGPS(), collapse = '\n')
          })
          
          # If process somehow unexpectedly terminates, always copy tmp log 
          # file to actual log file for user to see
          logfile = paste0(isolate(global$data_out), "/hbGPS.log")
          on.exit(file.copy(from = stdout_hbGPS_tmp, to = logfile, overwrite = TRUE), add = TRUE)
          
          # Start hbGPS
          x_hbGPS <- r_bg(func = function(hbGPS_shiny, ggiroutdir, gpsdir,
                                               outputdir, dataset_name,
                                               configfile){
            hbGPS_shiny(ggiroutdir, gpsdir,
                             outputdir, dataset_name,
                             configfile)
          },
          args = list(hbGPS_shiny = hbGPS_shiny,
                      ggiroutdir = global$ggirout_in,
                      gpsdir = global$gps_in,
                      outputdir = isolate(global$data_out),
                      dataset_name = input$dataset_name,
                      configfile =  paste0(global$data_out, "/config_hbGPS.csv")),
          stdout = stdout_hbGPS_tmp,
          stderr = "2>&1")

          observe({
            if (x_hbGPS$poll_io(0)[["process"]] != "ready") {
              invalidateLater(5000)
            } else {
              on.exit(removeNotification(id_hbGPS), add = TRUE)
              # When process is finished copy tmp log file to actual log file for user to see
              if (file.exists(stdout_hbGPS_tmp)) {
                file.copy(from = stdout_hbGPS_tmp, to = logfile, overwrite = TRUE)     
              }
              # Now check whether results are correctly generated:
              expected_hbGPS_folder = paste0(isolate(global$data_out), "/hbGPS_output")
              if (dir.exists(expected_hbGPS_folder) == TRUE) {
                csv_files_hbGPS = dir(expected_hbGPS_folder, pattern = "csv", recursive = TRUE, full.names = TRUE)
                if (length(csv_files_hbGPS) > 0) {
                  palmsplusr_message = paste0(
                    "Output is stored in: ", expected_hbGPS_folder,
                    paste0("<br/>The table below shows the content of ", basename(csv_files_hbGPS)[1]),
                    "<br/>Log file: ", logfile)
                  Sys.sleep(3)
                  hbGPS_file1 = read.csv(file = csv_files_hbGPS[1])
                  if (length(hbGPS_file1) > 0) {
                    output$hbGPS_file1 <- DT::renderDataTable(hbGPS_file1, options = list(scrollX = TRUE))
                  }
                } else {
                  hbGPS_message = paste0("hbGPS unsuccessful",
                                              "<br/>No file found inside: ", expected_hbGPS_folder,
                                              "<br/>Log file: ", logfile)
                }
              } else {
                hbGPS_message = paste0("hbGPS unsuccessful",
                                            "<br/>No file found inside: ", expected_hbGPS_folder,
                                            "<br/>Log file: ", logfile)
              }
              output$hbGPS_end_message <- renderUI({
                HTML(paste0(hbGPS_message))
              })
            }
          })
        }
      }
      return()
    })
    
    
    #========================================================================
    # Apply palmsplusr after button is pressed
    #========================================================================
    runpalmsplusr <- eventReactive(input$start_palmsplusr, {
      palmsplusr_message = ""
      
      if ("palmsplusr" %in% input$tools) {
        palmsplusr_message = "Error: Contact maintainer"
        # Basic check before running function:
        ready_to_run_palmsplusr = FALSE
        # Check for PALMSpy output (two possible sources either from this run or from a previous run)
        if (dir.exists(global$palmspyout_in)) {
          expected_palmspy_results_dir = global$palmspyout_in
        } else {
          expected_palmspy_results_dir = paste0(global$data_out,"/PALMSpy_output")
        }
        if (dir.exists(expected_palmspy_results_dir)) {
          Nfiles_in_dir = length(dir(path = expected_palmspy_results_dir, pattern = "csv", recursive = FALSE, full.names = FALSE))
          if (Nfiles_in_dir > 0) {
            # also check for GIS files
            if (dir.exists(global$gis_in)) {
              Nfiles_in_gisdir = length(dir(path = global$gis_in, recursive = FALSE, full.names = FALSE))
              if (Nfiles_in_gisdir > 0) {
                if (file.exists(global$gislinkfile_in)) {
                  ready_to_run_palmsplusr = TRUE
                } else {
                  palmsplusr_message = paste0("GIS link file not found: ", global$gislinkfile_in)
                }
              } else {
                palmsplusr_message = paste0("No files found in GIS folder: ", global$gis_in)
              }
            } else {
              palmsplusr_message = paste0("Folder that is supposed to hold  GIS files does not exist: ", global$gis_in)
            }
          } else {
            palmsplusr_message = paste0("No files found in PALMSpy output folder: ", expected_palmspy_results_dir)
          }
        } else {
          palmsplusr_message = paste0("Folder that is supposed to hold acceleration files does not exist: ", expected_palmspy_results_dir)
        }
        # Only run function when checks are met:
        if (ready_to_run_palmsplusr == TRUE) {
          shinyjs::hide(id = "start_palmsplusr")
          id_palmsplusr = showNotification("palmsplusr in progress ...", type = "message", duration = NULL, closeButton = FALSE)
          
          write.table(x = NULL, file = stdout_palmsplusr_tmp) # initialise empty file
          output$mylog_palmsplusr <- renderText({
            paste(mylog_palmsplusr(), collapse = '\n')
          })
          
          # If process somehow unexpectedly terminates, always copy tmp log 
          # file to actual log file for user to see
          logfile = paste0(isolate(global$data_out), "/palmsplusr.log")
          on.exit(file.copy(from = stdout_palmsplusr_tmp, to = logfile, overwrite = TRUE), add = TRUE)
          
          # palmsplusr_shiny(#country_name = "BA", # <= Discuss, extract from GIS foldername?
          #   # participant_exclude_list, # <= Discuss, leave out from linkfile?
          #   gisdir = global$gis_in,
          #   palmsdir = expected_palmspy_results_dir,
          #   gislinkfile = global$gislinkfile_in,
          #   outputdir = isolate(global$data_out),
          #   dataset_name = input$dataset_name)
          
          
          # print(list(palmsplusr_shiny = palmsplusr_shiny,
          #            gisdir = global$gis_in,
          #            palmsdir = expected_palmspy_results_dir,
          #            gislinkfile = global$gislinkfile_in,
          #            outputdir = isolate(global$data_out),
          #            dataset_name = input$dataset_name,
          #            configfile =  paste0(global$data_out, "/config_palmsplusr.csv")))
          
          # Start palmsplusr
          x_palmsplusr <- r_bg(func = function(palmsplusr_shiny, gisdir, palmsdir,
                                               gislinkfile, outputdir, dataset_name,
                                               configfile){
            palmsplusr_shiny(gisdir, palmsdir, gislinkfile,
                             outputdir, dataset_name, configfile)
          },
          args = list(palmsplusr_shiny = palmsplusr_shiny,
                      gisdir = global$gis_in,
                      palmsdir = expected_palmspy_results_dir,
                      gislinkfile = global$gislinkfile_in,
                      outputdir = isolate(global$data_out),
                      dataset_name = input$dataset_name,
                      configfile =  paste0(global$data_out, "/config_palmsplusr.csv")),
          stdout = stdout_palmsplusr_tmp,
          stderr = "2>&1")
          #   # Start PALMSplusR
          #   palmsplusr_shiny(#country_name = "BA", # <= Discuss, extract from GIS foldername?
          #     # participant_exclude_list, # <= Discuss, leave out from linkfile?
          #     gisdir = global$gis_in,
          #     palmsdir = expected_palmspy_results_dir,
          #     gislinkfile = global$gislinkfile_in,
          #     outputdir = isolate(global$data_out),
          #     dataset_name = input$dataset_name)
          
          observe({
            if (x_palmsplusr$poll_io(0)[["process"]] != "ready") {
              invalidateLater(5000)
            } else {
              on.exit(removeNotification(id_palmsplusr), add = TRUE)
              # When process is finished copy tmp log file to actual log file for user to see
              if (file.exists(stdout_palmsplusr_tmp)) {
                file.copy(from = stdout_palmsplusr_tmp, to = logfile, overwrite = TRUE)     
              }
              # Now check whether results are correctly generated:
              expected_palmsplusr_folder = paste0(isolate(global$data_out), "/palmsplusr_output")
              if (dir.exists(expected_palmsplusr_folder) == TRUE) {
                csv_files_palmsplusr = dir(expected_palmsplusr_folder, pattern = "csv", recursive = TRUE, full.names = TRUE)
                if (length(csv_files_palmsplusr) > 0) {
                  palmsplusr_message = paste0(#"PALMSplusR successfully completed at ", Sys.time(),
                    "Output is stored in: ", expected_palmsplusr_folder, #<br/>
                    paste0("<br/>The table below shows the content of ", basename(csv_files_palmsplusr)[1]),
                    "<br/>Log file: ", logfile)
                  Sys.sleep(3)
                  palmsplusr_file1 = read.csv(file = csv_files_palmsplusr[1])
                  if (length(palmsplusr_file1) > 0) {
                    output$palmsplusr_file1 <- DT::renderDataTable(palmsplusr_file1, options = list(scrollX = TRUE))
                  }
                } else {
                  palmsplusr_message = paste0("palmsplusr unsuccessful",
                                              "<br/>No file found inside: ", expected_palmsplusr_folder, #<br/>
                                              "<br/>Log file: ", logfile)
                }
              } else {
                palmsplusr_message = paste0("palmsplusr unsuccessful",
                                            "<br/>No file found inside: ", expected_palmsplusr_folder,
                                            "<br/>Log file: ", logfile)
              }
              output$palmsplusr_end_message <- renderUI({
                HTML(paste0(palmsplusr_message))
              })
            }
          })
        }
      }
      return()
    })
    output$recommendorder <- renderText({
      pipeline = proposed_pipeline()
      if ("GGIR" %in% pipeline & "CountConverter" %in% pipeline) {
        pipeline = pipeline[-which(pipeline %in% c("GGIR", "CountConverter"))]
        pipeline = c("GGIR & CountConverter", pipeline)
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
    output$palmsplusr_end_message <- renderText({
      message = runpalmsplusr()
    })
  }
  # Run the application 
  shinyApp(ui, server, enableBookmarking = "server")
}