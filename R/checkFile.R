#' Function to check that the config files have the expected format
#'
#' @param file data path to config file
#' @param tool either PALMSpy, GGIR, or palmsplusr for now.
#'
#' @return message with the result of the check (either ok or the description of a problem)
#' @export
checkFile = function(file=c(), tool=c()) {
  
  # intialize check object, ok by default, it would be overwritten if 
  # any problem is identified in the config file.
  check = "ok"
  
  if (tool == "PALMSpy") {
    # TO BE DEVELOPED
  } else if (tool == "GGIR") {
    # sanity check 1: is it a csv file? ----
    if (!file.exists(file)) {
      # stop("No config file found at ", path)
      check = "No config file found at "
    } else {
      path_unlist = unlist(strsplit(x = file, split = ".", fixed = TRUE))
      path_ext = path_unlist[length(path_unlist)]
      if (path_ext != "csv") {
        check = "The GGIR config file uploaded is not a csv file"
      } else {
        # read config file if it exists and it is a csv file
        params = read.csv(file = file)
        # sanity check 2: colnames of config file ----
        colnames = colnames(params)
        if (!(colnames[1] == "argument" & colnames[2] == "value" & colnames[3] == "context")) {
          # this automatically also checks that csv is separated by commas, as
          # it would generate just one column (e.g., "argument;value;context") otherwise
          check = "The csv file uploaded is not a GGIR config file"
        } 
      }
    } 
  } else if (format == "palmsplusr") {
    # TO BE DEVELOPED
  }
  return(check)
}