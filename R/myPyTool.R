#' myPyTool
#'
#' @param inputdir Path to input directory
#' @param outputdir Path to output directory
#' @return no object is returned, function only writes message to console and writes file.
#' @export

myPyTool = function(inputdir, outputdir) {
  system(paste0("python3 ./inst/python/hello.py ", inputdir, " ", outputdir))
}