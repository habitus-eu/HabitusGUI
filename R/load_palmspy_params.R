#' load_palmspy_params
#'
#' @param file .json file
#' @return list of parameters from the json object
#' @importFrom jsonlite fromJSON
#' @export


load_palmspy_params = function(file=c()) {
  config = fromJSON(txt = file, simplifyDataFrame = TRUE)
  if ("parameters" %in% names(config)) {
    palmspy_params = config$parameters
  } else {
    warning(paste0("\nparameters section not found in ", file))
    palmspy_params = c()
  }
  return(palmspy_params)
}