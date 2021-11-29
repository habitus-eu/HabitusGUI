#' PALMSpy_R
#'
#' @param gps_path Path to gps data
#' @param acc_path Path to acc data
#' @param output_path Path to output directory, default to working directory
#' @param config_file Path to configuration file
#' @return No object is returned, function only writes message to console and writes file.
#' @export

PALMSpy_R = function(gps_path, acc_path, output_path, config_file =c()) {
  basecommand = paste0("palmspy --gps-path ", gps_path, " --acc-path ", acc_path)
  if (length(config_file) > 0) { # configfile available
    system(paste0(basecommand, " --config-file ",config_file))
  } else { # configfile not available
    system(basecommand)
  }
  if (!dir.exists("./PALMSpy_output")) {
    warning("Folder PALMSpy_output not found")
  } else {
    file.copy(from = "./PALMSpy_output", to = output_path, 
              overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
  }
}