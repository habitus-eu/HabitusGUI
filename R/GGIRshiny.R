#' GGIRshiny
#'
#' @param rawaccdir Path to input directory
#' @param outputdir Path to output directory
#' @param sleepdiary Path to sleep diary
#' @param configfile Configfile path
#' @return no object is returned, only a new file is created in the output directory
#' @import GGIR
#' @importFrom utils write.table
#' @importFrom data.table fread
#' @export
#' 

GGIRshiny = function(rawaccdir, outputdir, sleepdiary = c(), configfile = c()) {
  if (length(sleepdiary) == 0) sleepdiary = c()
  if (length(configfile) == 0) {
    configfile = c()
    modeIS = ""
  } else {
    # extract mode from config file
    config = data.table::fread(file = configfile, data.table = FALSE)
    modeChar = config$value[which(config$argument == "mode")]
    # check that mode is nothing more than a numeric vector
    checkMode = suppressWarnings(as.numeric(gsub(x = modeChar, pattern = "c|[(]|[)]|:|,", replacement = "")))
    if (is.numeric(checkMode) == FALSE) {
      stop("\nUnexpected value for parameter mode")
    } else {
      modeNum = eval(parse(text = modeChar)) 
    }
    if (min(modeNum) > 0 && max(modeNum) <= 6) {
      # use mode value from config file if values are plausible
      modeIS = paste0(", mode = ", modeChar)
    } else {
      # do not use mode value and let GGIR use its default
      modeIS = ""
    }
  }
  
  # create R script with the code to run the data analysis via a command line call
  # in this way turning off or restarting the app will not kill the data analysis
  fileConn <- file(paste0(outputdir, "/ggir_cmdline.R"))
  writeLines(c("#!/usr/bin/env Rscript",
               "args = commandArgs(trailingOnly = TRUE)",
               "if (length(args) < 3) {",
               "stop(\"At least three arguments are expected\", call. = FALSE)",
               "}",
               "if (length(args) == 4) {",
               "GGIR::GGIR(datadir = args[1], outputdir = args[2], ",
               "configfile = args[3], loglocation = args[4],",
               "do.parallel = TRUE", modeIS,")",
               "} else {",
               "GGIR::GGIR(datadir = args[1], outputdir = args[2], ",
               "configfile = args[3], do.parallel = TRUE", modeIS,")",
               "}"),
             fileConn)
  close(fileConn)
  
  if (.Platform$OS.type == "windows") {
    logFile = paste0(outputdir, "/GGIR.log")
    fileConn = file(logFile)
    writeLines(c("Hello, this Shiny app is primarily designed for Unix.",
                 "In Windows OS live progress of the analysis can be followed in the RStudio console.",
                 "In Unix-like systems the progress would be shown here inside this window in the Shiny app."), fileConn)
    close(fileConn)
    
    basecommand = paste0(outputdir, "/ggir_cmdline.R ",
                         rawaccdir, " ",
                         outputdir, " ",
                         configfile, " ",
                         sleepdiary)
    system2(command = "Rscript", args = basecommand,
            stdout = "",
            stderr = "", wait = TRUE)
    write.table(x = paste0(""),
                file = paste0(outputdir, "/GGIR.log"))
    
    fileConn = file(logFile)
    writeLines(c(""), fileConn)
    close(fileConn)
    
  } else {
    basecommand = paste0("cd ", outputdir, " ; nohup Rscript ggir_cmdline.R ",
                         rawaccdir, " ",
                         outputdir, " ",
                         configfile, " ",
                         sleepdiary, " > ", outputdir, "/GGIR.log 2>&1 &")
    system2(command = "cd", args = gsub(pattern = "cd ", replacement = "", x = basecommand),
            stdout = "", stderr = "", wait = TRUE)
  }
}