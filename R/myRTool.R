#' myRTool 
#'
#' @param inputdir Path to input directory
#' @param outputdir Path to output directory
#' @param config Config object
#' @return no object is returned, only a new file is created in the output directory
#' @export

myRTool = function(inputdir, outputdir, config) {
  filenames = dir(path = inputdir, full.names = TRUE)
  N = length(filenames)
  results = data.frame(ID=character(N), maxx=numeric(N), maxy=numeric(N))
  for (i in 1:N) {
    cat(paste0(i," "))
    D = read.csv(filenames[i])
    results$ID[i] = unlist(strsplit(basename(filenames[i])," "))[1]
    results$maxx[i] = max(D$x)
    results$maxy[i] = max(D$y)
    Sys.sleep(0.5)
  }
  if (!is.null(config)) {
    print(paste0("Configure file: ", config))
  }
  write.csv(x = results, file = paste0(outputdir,"/results.csv"), row.names = FALSE)
}