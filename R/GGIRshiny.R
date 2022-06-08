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

GGIRshiny = function(rawaccdir, outputdir, sleepdiary=c(), configfile=c(),  do.BrondCounts = FALSE) {
  if (length(sleepdiary) == 0) sleepdiary = c()
  if (length(configfile) == 0) configfile = c()
  if (do.BrondCounts == FALSE) {
      GGIR::GGIR(datadir = rawaccdir, outputdir = outputdir, overwrite = FALSE,
                         configfile = configfile, loglocation = sleepdiary, do.parallel = TRUE)
  } else {
    desired_sf_by_BrondCounts = 30
    myfun =  list(FUN = BrondCounts, # Note BrondCounts is function within HabitusGUI
                  parameters = desired_sf_by_BrondCounts,
                  expected_sample_rate = desired_sf_by_BrondCounts,
                  expected_unit = "g",
                  colnames = c("countx","county","countz"),
                  minlength = 5,
                  outputres = 1,
                  outputtype = "numeric",
                  reporttype = "scalar",
                  aggfunction = sum,
                  timestamp = TRUE)
    GGIR::GGIR(datadir = rawaccdir, outputdir = outputdir, overwrite = FALSE,
                       configfile = configfile, loglocation = sleepdiary, 
                       myfun = myfun, do.parallel = TRUE, backup.cal.coef = "retrieve")
    BrondCounts2csv(outputdir = paste0(outputdir, "/output_", basename(rawaccdir)), configfile = configfile)
  }
}