#' PALMSpyshiny
#'
#' @param outputdir Path to output directory
#' @param gpsdir Path to GPS files
#' @param envConda character to specify path conda environment used for PALMSpy
#' @param count_file_location Path to count files
#' @return no object is returned, only a new file is created in the output directory
#' @export

PALMSpyshiny = function(outputdir, gpsdir, count_file_location, envConda = "~/miniconda3/bin/conda") {
  if (tolower(Sys.getenv("USERNAME")) %in% c("ucloud", "", "shiny") == FALSE) {
    #assuming palmspy is in a local conda environment"
    # assuming unbuffer is available in Ubuntu(apt install expect)
    palmspypath = paste0(envConda, " run --no-capture-output -n palmspy ")
  } else {
    #assuming palmspy is directly available from command line (ucloud)
    palmspypath = ""
  }
  basecommand = paste0("cd ",outputdir, "; nohup unbuffer ", palmspypath, " palmspy --gps-path ", gpsdir,
                       " --acc-path ", count_file_location,
                       " --config-file ", paste0(outputdir, "/config.json"),
                       " > ", outputdir, "/PALMSpy.log 2>&1 &")
  print(basecommand)
  system2(command = "cd", args = gsub(pattern = "cd ", replacement = "", x = basecommand),
          stdout = "", stderr = "", wait = TRUE)
  return()
}
""
