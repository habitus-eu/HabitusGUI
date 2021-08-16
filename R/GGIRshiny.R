#' GGIRshiny
#'
#' @param inputdir Path to input directory
#' @param outputdir Path to output directory
#' @param config Config object
#' @param sleepdiary Path to sleep diary
#' @param desiredtz Desired tz databasename
#' @return no object is returned, only a new file is created in the output directory
#' @import GGIR
#' @export

GGIRshiny = function(inputdir, outputdir, config, sleepdiary, desiredtz=c()) {
  if (length(desiredtz) > 0) {
    GGIR::g.shell.GGIR(datadir=inputdir, outputdir=outputdir, configfile = config,
                       loglocation=sleepdiary, desiredtz=desiredtz)
  } else {
    GGIR::g.shell.GGIR(datadir=inputdir, outputdir=outputdir, configfile = config,
                       loglocation=sleepdiary)
  }
}