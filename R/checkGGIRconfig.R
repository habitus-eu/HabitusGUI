#' checkGGIRconfig
#'
#' @param configfile Config object
#' @return character with desiredtz value or error message
#' @importFrom utils read.csv
#' @export


checkGGIRconfig = function(configfile=c()) {
  conf = read.csv(configfile)
  if (ncol(conf) != 3) { # is it a config file?
    desiredtz = "configuration file format not recognised"
  } else {
    desiredtz_i = which(conf[,1] == "desiredtz")
    if (length(desiredtz_i) == 1) {
      desiredtz = conf[desiredtz_i,2]
    } else {
      desiredtz = "No timezone found in configuration file."
    }
  }
  return(desiredtz)
}