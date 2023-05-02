#' GGIRshiny
#'
#' @param rawaccdir Path to input directory
#' @param outputdir Path to output directory
#' @param configfile Configfile path
#' @param sleepdiary Path to sleep diary
#' @param do.Counts Boolean to indicate whether BrondCounts should be derived
#' @return no object is returned, only a new file is created in the output directory
#' @import GGIR
#' @export

GGIRshiny = function(rawaccdir, outputdir, sleepdiary = c(), configfile = c(),
                     do.Counts = FALSE) {
  if (length(sleepdiary) == 0) sleepdiary = c()
  if (length(configfile) == 0) configfile = c()
  # create R script with the code to run the data analysis via a command line call
  # in this way turning off or restarting the app will not kill the data analysis
  fileConn <- file(paste0(outputdir, "/ggir_cmdline.R"))
  writeLines(c("#!/usr/bin/env Rscript",
               "args = commandArgs(trailingOnly = TRUE)",
               "if (length(args) < 4) {",
               "stop(\"At least four arguments are expected\", call. = FALSE)",
               "}",
               "if (length(args) == 5) {",
               "GGIR::GGIR(datadir = args[1], outputdir = args[2], ",
               " do.neishabouricounts = as.logical(args[3]),",
               "configfile = args[4], loglocation = args[5],",
               "do.parallel = TRUE)",
               "} else {",
               "GGIR::GGIR(datadir = args[1], outputdir = args[2], ",
               " do.neishabouricounts = as.logical(args[3]),",
               "configfile = args[4], do.parallel = TRUE)",
               "}",
               "if (as.logical(args[3]) ==  TRUE) {",
               "HabitusGUI::Counts2csv(outputdir = paste0(args[2], \"/output_\", basename(args[1])), configfile = args[4])",
               "}"),
             fileConn)
  close(fileConn)
  
  if (.Platform$OS.type == "windows") {
    logFile = paste0(outputdir, "/GGIR.log")
    fileConn = file(logFile)
    writeLines(c("Hello, this Shiny app is primarily designed for Linux.",
                 "In Windows OS live progress of the analysis can be followed in the RStudio console.",
                 "In Unix-like systems the progress would be shown here inside this window in the Shiny app."), fileConn)
    close(fileConn)
    
    basecommand = paste0(outputdir, "/ggir_cmdline.R ",
                         rawaccdir, " ",
                         outputdir, " ",
                         do.Counts, " ",
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
                         do.Counts, " ",
                         configfile, " ",
                         sleepdiary, " > ", outputdir, "/GGIR.log 2>&1 &")
    system2(command = "cd", args = gsub(pattern = "cd ", replacement = "", x = basecommand),
            stdout = "", stderr = "", wait = TRUE)
  }
  
}