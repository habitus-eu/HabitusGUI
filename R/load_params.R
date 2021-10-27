#' load_params
#'
#' @param file Character to specify location of configuration file
#' @param format Character to specify format of configuration file: json_palsmpy or csv_GGIR
#' @return list of parameters extract from the configuration file
#' @importFrom jsonlite fromJSON
#' @importFrom utils read.csv
#' @export


load_params = function(file=c(), format="json_palmspy") {
  if (format == "json_palmspy") {
    config = fromJSON(txt = file, simplifyDataFrame = TRUE)
    if ("parameters" %in% names(config)) {
      params = as.data.frame(t(as.data.frame(config$parameters)))
      colnames(params)[1] = "value"
      params$parameter = rownames(params)
      # Keep only parameters with a matching description in the description file
      params_info_palmspy_file = system.file("testfiles_palmspy/params_description_palmspy.tsv", package = "HabitusGUI")[1]
      params_info_palmspy = read.table(file = params_info_palmspy_file, sep = "\t", header = TRUE)
      params_merged = merge(params_info_palmspy, params, by = "parameter")
      rownames(params_merged) = params_merged$parameter
      params = params_merged[,c("value", "topic", "description")]
    } else {
      warning(paste0("\nparameters section not found in ", file))
      params = c()
    }
  } else if (format == "csv_ggir") {
    params = read.csv(file = file)
    params = as.data.frame(params[,2:ncol(params)], row.names = params[,1])
    RN = row.names(params)
    dups = duplicated(RN)
    params = params[!dups,]
  }
  return(params)
}