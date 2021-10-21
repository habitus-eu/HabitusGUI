#' updatePALMSpyconfig
#'
#' @param configfile Config object
#' @param new_desiredtz New desiredtz value
#' @return No object return only file updated
#' @importFrom utils read.csv write.csv
#' @export


updatePALMSpyconfig = function(configfile=c(), new_desiredtz) {

  conf = read.csv(configfile)
  if (ncol(conf) != 3) { # is it a config file?
    stop("Configuration file has incorrect dimensions")
  } else {
    desiredtz_i = which(conf[,1] == "desiredtz")
    if (length(desiredtz_i) == 1) {
      conf[desiredtz_i,2] = new_desiredtz
    } else {
      conf = rbind(conf, c("desiredtz", new_desiredtz, ""))
    }
  }
  write.csv(x = conf, file = configfile, row.names = FALSE)
}