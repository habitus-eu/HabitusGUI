mytool = function(inputdir, outputdir, config) {
  filenames = dir(path = inputdir, full.names = TRUE)
  N = length(filenames)
  results = data.frame(ID=character(N), maxx=numeric(N), maxy=numeric(N))
  for (i in 1:N) {
    cat(paste0(i," "))
    D = read.csv(filenames[i])
    results$ID[i] = unlist(strsplit(basename(filenames[i])," "))[1]
    results$maxx[i] = max(D$x)
    results$maxy[i] = max(D$y)
    Sys.sleep(1)
  }
  write.csv(x = results, file = paste0(outputdir,"/results.csv"), row.names = FALSE)
}