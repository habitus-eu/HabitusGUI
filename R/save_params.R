#' save_params
#'
#' @param file Character to specify location of original configuration file
#' @param format Character to specify format of configuration file: json_palsmpy or csv_GGIR
#' @param new_params New parameters
#' @return No object returned, function only reads original data, and overwrites parameters and stores it again
#' @importFrom jsonlite fromJSON toJSON
#' @export

save_params = function(new_params = c(), file = c(), format="json_palsmpy") {
  if (format == "json_palsmpy") {
    config = fromJSON(txt = file, simplifyDataFrame = TRUE)
    if ("parameters" %in% names(config)) {
      config$parameters = as.list(t(new_params))
    } else {
      warning(paste0("\nparameters section not found in ", file))
    }
    toJSON(txt = file, x = config)
  } else if (format == "csv_GGIR") {
    params = read.csv(file = file)
  }
}