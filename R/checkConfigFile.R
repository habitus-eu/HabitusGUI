#' Function to check that the config files have the expected format
#'
#' @param file data path to config file
#' @param tool either PALMSpy, GGIR, or palmsplusr for now.
#'
#' @return message with the result of the check (either ok or the description of a problem)
#' @export
checkConfigFile = function(file=c(), tool=c()) {
  # intialize check object, ok by default, it would be overwritten if 
  # any problem is identified in the config file.
  check = "ok"
  
  # General check on file existence and extension 
  if (!file.exists(file)) {
    # stop("No config file found at ", path)
    check = "No config file found at "
  } else {
    path_unlist = unlist(strsplit(x = file, split = ".", fixed = TRUE))
    path_ext = tolower(path_unlist[length(path_unlist)])
    if (tool %in% c("GGIR", "hbGPS", "palmsplusr") & path_ext != "csv") {
      check = "The GGIR config file uploaded is not a csv file"
    } else if (tool == "PALMSpy" & path_ext != "json") {
      check = "The GGIR config file uploaded is not a json file"
    }
  }
  
  # Specific file content check
  if (check == "ok") {
    if (tool == "PALMSpy") {
      # TO BE DEVELOPED
    } else if (tool == "GGIR" || tool == "hbGPS") {
      # read config file if it exists and it is a csv file
      params = read.csv(file = file)
      # sanity check 2: colnames of config file ----
      if (ncol(params) == 3 && tool != "palmsplusr") {
        check_colnames = all.equal(colnames(params), c("argument", "value", "context"))
      } else if (ncol(params) == 5 && tool == "palmsplusr") {
        check_colnames = all.equal(colnames(params), c("context", "name", "formula", "domain_field", "after_conversion"))
      } else {
        check_colnames = FALSE
      }
      if (check_colnames == FALSE) {
        # this automatically also checks that csv is separated by commas, as
        # it would generate just one column (e.g., "argument;value;context") otherwise
        check = paste0("The csv file uploaded is not a ", tool, " config file")
      } 
    }
  }
  return(check)
}