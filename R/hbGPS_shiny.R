#' hbGPS_shiny
#'
#' @param ggiroutdir GGIR time series output directory
#' @param gpsdir Path to GPS directory
#' @param outputdir Path to output directory
#' @param dataset_name Name of dataset
#' @param configfile Path to configuration file
#' @return no object is returned, only a new file is created in the output directory
#' @import hbGPS
#' @export

hbGPS_shiny = function(ggiroutdir = NULL, gpsdir = NULL, outputdir = NULL,
                       dataset_name = "", configfile = c()) {
  # create R script with the code to run the data analysis via a command line call
  # in this way turning off or restarting the app will not kill the data analysis
  fileConn <- file(paste0(outputdir, "/hbgps_cmdline.R"))
  writeLines(c("#!/usr/bin/env Rscript",
               "args = commandArgs(trailingOnly = TRUE)",
               "if (length(args) < 4) {",
               "stop(\"At least four arguments are expected\", call. = FALSE)",
               "}",
               "hbGPS::hbGPS(gps_file = args[1], ",
                     "GGIRpath = args[2],",
                     "outputDir = args[3],",
                     "configFile = args[4])",
               ),
             fileConn)
  close(fileConn)
  
  if (.Platform$OS.type == "windows") {
    logFile = paste0(outputdir, "/hbGPS.log")
    fileConn = file(logFile)
    writeLines(c("Hello, this Shiny app is primarily designed for Unix.",
                 "In Windows OS live progress of the analysis can be followed in the RStudio console.",
                 "In Unix-like systems the progress would be shown here inside this window in the Shiny app."), fileConn)
    close(fileConn)
    
    basecommand = paste0(outputdir, "/hbgps_cmdline.R ",
                         gps_file, " ",
                         GGIRpath, " ",
                         outputdir, " ",
                         configfile)
    system2(command = "Rscript", args = basecommand,
            stdout = "",
            stderr = "", wait = TRUE)
    write.table(x = paste0(""),
                file = paste0(outputdir, "/hbGPS.log"))
    
    fileConn = file(logFile)
    writeLines(c(""), fileConn)
    close(fileConn)
    
  } else {
    basecommand = paste0("cd ", outputdir, " ; nohup Rscript hbgps_cmdline.R ",
                         gps_file, " ",
                         GGIRpath, " ",
                         outputdir, " ",
                         configfile, " > ", outputdir, "/hbGPS.log 2>&1 &")
    system2(command = "cd", args = gsub(pattern = "cd ", replacement = "", x = basecommand),
            stdout = "", stderr = "", wait = TRUE)
  }
  
}