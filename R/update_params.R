#' update_params
#'
#' @param file Character to specify location of original configuration file
#' @param format Character to specify format of configuration file: json_palsmpy or csv_GGIR
#' @param new_params New parameters
#' @return No object returned, function only reads original data, and overwrites parameters and stores it again
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom utils read.csv write.csv
#' @export

update_params = function(new_params = c(), file = c(), format="json_palmspy") {
  if (format == "json_palmspy") {
    config = fromJSON(txt = file, simplifyDataFrame = TRUE)
    if ("parameters" %in% names(config)) {
      new = as.list(t(new_params[,"value"]))
      names(new) = rownames(new_params)
      config$parameters[names(new)] = new # only overwrite the matching fields
    } else {
      warning(paste0("\nparameters section not found in ", file))
    }
    exportJson <- toJSON(config)
    write(exportJson, file = file)
  } else if (format == "csv_GGIR") {
    old_params = read.csv(file = file)
    params[which(old_params[,1] %in% row.names(new_params)), 2] = new_params
    write.csv(x = new_params, file = file)
  }
}