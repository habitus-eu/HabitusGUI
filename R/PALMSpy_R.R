#' PALMSpy_R
#'
#' @param gps_path Path to gps data
#' @param acc_path Path to acc data
#' @param config_file Path to configuration file
#' @return No object is returned, function only writes message to console and writes file.
#' @export

PALMSpy_R = function(gps_path, acc_path, config_file =c()) {
  # Note command assumes that PALMSPy is directly available from command line
  # If you installed PALMSpy in conda environment, make sure you run the HabitusGUI from that conda environment
  
  # basecommand = paste0("/home/vincent/miniconda3/bin/conda run -n palmspy palmspy --gps-path ", gps_path, " --acc-path ", acc_path)
  basecommand = paste0("palmspy --gps-path ", gps_path, " --acc-path ", acc_path)
  if (length(config_file) > 0) {
    print(paste0(basecommand, " --config-file ",config_file))
    system(command = paste0(basecommand, " --config-file ",config_file), wait = TRUE)
  } else {
    system(basecommand)
  }
}
