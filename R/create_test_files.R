#' create_test_files
#'
#' @param dir Directory where test files should be created
#' @param Nfiles Number of files to create
#' @param Nobs Number of observations
#' @return no object is returned, function only writes file.
#' @export

create_test_files = function(dir=c(), Nfiles=10, Nobs = 10) {
  new_file_names = paste0(sample(x = 1:Nfiles, size = Nfiles, replace = FALSE)," recording.csv")
  for (i in 1:length(new_file_names)) {
    print(i)
    data = data.frame(x = rnorm(mean = 1, sd = 1, n = Nobs),
                      y = runif(min = 0, max = 100, n = Nobs))
    data = round(data, digits=4)
    
    write.csv(x = data, file = paste0(dir,"/",new_file_names[i]))
    rm(data)
  }
}