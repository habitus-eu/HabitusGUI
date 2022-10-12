#' GGIRshiny
#'
#' @param rawaccdir Path to input directory
#' @param outputdir Path to output directory
#' @param configfile Configfile path
#' @param sleepdiary Path to sleep diary
#' @param do.Counts Boolean to indicate whether BrondCounts should be derived
#' @return no object is returned, only a new file is created in the output directory
#' @import GGIR
#' @export

GGIRshiny = function(rawaccdir, outputdir, sleepdiary=c(), configfile=c(),  do.Counts = FALSE) {
  if (length(sleepdiary) == 0) sleepdiary = c()
  if (length(configfile) == 0) configfile = c()
  if (do.Counts == FALSE) {
    GGIR::GGIR(datadir = rawaccdir, outputdir = outputdir, overwrite = FALSE, do.neishabouricounts = FALSE,
               configfile = configfile, loglocation = sleepdiary, do.parallel = TRUE)
  } else {
    GGIR::GGIR(datadir = rawaccdir, outputdir = outputdir, overwrite = FALSE, do.neishabouricounts = TRUE,
               configfile = configfile, loglocation = sleepdiary, 
               do.parallel = TRUE, backup.cal.coef = "retrieve")
    Counts2csv(outputdir = paste0(outputdir, "/output_", basename(rawaccdir)), configfile = configfile)
  }
}