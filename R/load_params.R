#' load_params
#'
#' @param file Character to specify location of configuration file
#' @param format Character to specify format of configuration file: json_palsmpy or csv_GGIR
#' @return list of parameters extract from the configuration file
#' @importFrom jsonlite fromJSON
#' @importFrom utils read.csv
#' @export


load_params = function(file=c(), format="json_palsmpy") {
  if (format == "json_palsmpy") {
    config = fromJSON(txt = file, simplifyDataFrame = TRUE)
    if ("parameters" %in% names(config)) {
      params = t(as.data.frame(config$parameters))
      colnames(params)[1] = "Value"
    } else {
      warning(paste0("\nparameters section not found in ", file))
      params = c()
    }
  } else if (format == "csv_GGIR") {
    params = read.csv(file = file)
    params = as.data.frame(params[,2:ncol(params)], row.names = params[,1])
    RN = row.names(params)
    dups = duplicated(RN)
    params = params[!dups,]
    print(dim(params))
  }
  return(params)
}