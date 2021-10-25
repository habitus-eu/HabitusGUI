#' load_palmspy_params
#'
#' @param file .json file
#' @return list of parameters from the json object
#' @importFrom jsonlite fromJSON
#' @export


load_palmspy_params = function(file=c()) {
  # file = load_palmspy_params("~/projects/fontys/HabitusGUI/inst/testfiles_palmspy/palmspy-params.json")
  config = fromJSON(txt = file, simplifyDataFrame = TRUE)
  if ("parameters" %in% names(config)) {
    palmspy_params = config$parameters
    # colnames(palmspy_params) = "Value"
  } else {
    warning(paste0("\nparameters section not found in ", file))
    palmspy_params = c()
  }
  return(palmspy_params)
}