#' myPyTool
#'
#' @param inputdir Path to input directory
#' @param outputdir Path to output directory
#' @param sleepdiary Path to sleep diary
#' @return no object is returned, function only writes message to console and writes file.
#' @export

myPyTool = function(inputdir, outputdir, sleepdiary) {
  basecommand = paste0("python3 ./inst/python/hello.py ", inputdir, " ", outputdir)
  if (length(sleepdiary) > 0) {
    system(paste0(basecommand," ",sleepdiary))
  } else {
    system(basecommand)
  }
}