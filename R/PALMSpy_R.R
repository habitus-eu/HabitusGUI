#' PALMSpy_R
#'
#' @param gps_path Path to gps data
#' @param acc_path Path to acc data
#' @param output_path Path to output directory, default to working directory
#' @param config_file Path to configuration file
#' @param parameters List object with individual parameters to overrule config_file and software defaults
#' @return No object is returned, function only writes message to console and writes file.
#' @export

PALMSpy_R = function(gps_path, acc_path, output_path, config_file, parameters) {
  basecommand = paste0("python3 ./inst/python/PALMSpy_python_wrapper.py ", 
                       gps_path, " ", acc_path, " ", output_path)
  if (length(config_file) > 0) {
    system(paste0(basecommand, " ",config_file))
  } else {
    system(paste0(basecommand, " config_file not available"))
  }
}