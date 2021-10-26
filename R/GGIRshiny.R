#' GGIRshiny
#'
#' @param rawaccdir Path to input directory
#' @param outputdir Path to output directory
#' @param configfile Configfile path
#' @param sleepdiary Path to sleep diary
#' @return no object is returned, only a new file is created in the output directory
#' @import GGIR
#' @export

GGIRshiny = function(rawaccdir, outputdir, configfile=c(), sleepdiary=c()) {
  if (is.null(sleepdiary)) sleepdiary = c()
  if (length(sleepdiary) > 0) {
    GGIR::g.shell.GGIR(datadir=rawaccdir, outputdir=outputdir, configfile = configfile, loglocation=sleepdiary)
  } else { 
    GGIR::g.shell.GGIR(datadir=rawaccdir, outputdir=outputdir, configfile = configfile)
  }
}