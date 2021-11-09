#' BrondCounts
#'
#' @param data Matrix, with 3 columns with raw accelerometer data
#' @param herz numeric, sample rate of the data object 
#' @return no object is returned, only a new file is created in the output directory
#' @export

BrondCounts = function(data=c(), herz=c()) {
  if (ncol(data) == 4) {
    data = data[,2:4]
  }
  mycounts = activityCounts::counts(data = data, hertz = herz, 
                    x_axis = 1, y_axis = 2, z_axis = 3,
                    start_time = Sys.time())
  mycounts = mycounts[,2:4] # Removing timestamps because for GGIR we do not want this
  return(mycounts)
}