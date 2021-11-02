#' GGIRshiny
#'
#' @param rawaccdir Path to input directory
#' @param outputdir Path to output directory
#' @param configfile Configfile path
#' @param sleepdiary Path to sleep diary
#' @param do.BrondCounts Boolean to indicate whether BrondCounts should be derived
#' @return no object is returned, only a new file is created in the output directory
#' @import GGIR
#' @export

GGIRshiny = function(rawaccdir, outputdir, configfile=c(), sleepdiary=c(), do.BrondCounts = FALSE) {
  if (length(sleepdiary) == 0) sleepdiary = c()
  if (length(configfile) == 0) configfile = c()
  if (do.BrondCounts == FALSE) {
    GGIR::g.shell.GGIR(datadir=rawaccdir, outputdir=outputdir, configfile = configfile, loglocation=sleepdiary)
  } else {
    GGIR::g.shell.GGIR(datadir=rawaccdir, outputdir=outputdir, configfile = configfile, loglocation=sleepdiary)
  }
}