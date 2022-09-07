#' PALMSpyshiny
#'
#' @param outputdir Path to output directory
#' @param gpsdir Path to GPS files
#' @param count_file_location Path to count files
#' @return no object is returned, only a new file is created in the output directory
#' @export

PALMSpyshiny = function(outputdir, gpsdir, count_file_location) {
  basecommand = paste0("cd ",outputdir," ; nohup palmspy --gps-path ", gpsdir,
                       " --acc-path ", count_file_location,
                       " --config-file ", paste0(outputdir, "/config.json"), " > /work/PALMSpy.log 2>&1 &")
  system2(command = "cd", args = gsub(pattern = "cd ", replacement = "", x = basecommand),
          stdout = "", stderr = "", wait = TRUE)
  return()
}
