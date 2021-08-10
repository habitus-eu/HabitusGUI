#' GGIR
#'
#' @param inputdir Path to input directory
#' @param outputdir Path to output directory
#' @param config Config object
#' @param sleepdiary Path to sleep diary
#' @return no object is returned, only a new file is created in the output directory
#' @import GGIR
#' @export

GGIRshiny = function(inputdir, outputdir, config, sleepdiary) {
  GGIR::g.shell.GGIR(datadir=inputdir, outputdir=outputdir, configfile = config, loglocation=sleepdiary)
}