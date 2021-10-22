#' load_palmspy_params
#'
#' @param file .json file
#' @return list of parameters from the json object
#' @importFrom jsonlite fromJSON
#' @export


load_palmspy_params = function(file=c()) {
  # file = load_palmspy_params("~/projects/fontys/HabitusGUI/inst/testfiles_palmspy/palmspy-params.json")
  print("a")
  print(file)
  config = fromJSON(txt = file, simplifyDataFrame = TRUE)
  print("b")
  if ("parameters" %in% names(config)) {
    print("c")
    palmspy_params = t(as.data.frame(config$parameters))
    print("d")
    colnames(palmspy_params) = "Value"
  } else {
    print("e")
    warning(paste0("\nparameters section not found in ", file))
    palmspy_params = c()
  }
  print(palmspy_params)
  return(palmspy_params)
}