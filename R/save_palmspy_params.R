#' save_palmspy_params
#'
#' @param file .json file
#' @param new_params new parameters
#' @return No object returned, function only wrtite data
#' @importFrom jsonlite fromJSON toJSON
#' @export

save_palmspy_params = function(new_params = c(), file = c()) {
  file = "~/projects/fontys/palmspy-params.json"
  config = fromJSON(txt = file, simplifyDataFrame = TRUE)
  if ("parameters" %in% names(config)) {
    config$parameters = as.list(t(new_params))
  } else {
    warning(paste0("\nparameters section not found in ", file))
  }
  toJSON(txt = file, x = config)
}