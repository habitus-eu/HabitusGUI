#' create_test_files
#'
#' @param dir Directory where test files should be created
#' @param Nfiles Number of files to create
#' @param Nobs Number of observations
#' @return no object is returned, function only writes file.
#' @importFrom utils write.csv
#' @importFrom stats rnorm runif
#' @export

create_test_files = function(dir=c(), Nfiles=10, Nobs = 10) {
  new_file_names = paste0(sample(x = 1:Nfiles, size = Nfiles, replace = FALSE)," recording.csv")
  for (i in 1:length(new_file_names)) {
    print(i)
    set.seed(300)
    x = rnorm(mean = 1, sd = 1, n = Nobs)
    set.seed(400)
    y = runif(min = 0, max = 100, n = Nobs)
    data = data.frame(x = x,
                      y = y)
    data = round(data, digits = 4)
    
    write.csv(x = data, file = paste0(dir,"/",new_file_names[i]))
    rm(data)
  }
}