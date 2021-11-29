#' load_params
#'
#' @param file Character to specify location of configuration file
#' @param format Character to specify format of configuration file: json_palsmpy or csv_GGIR
#' @return list of parameters extract from the configuration file
#' @importFrom jsonlite fromJSON
#' @importFrom utils read.csv read.table
#' @export


load_params = function(file=c(), format="json_palmspy") {
  expected_tsv_columns = c("value", "field", "subfield", "display", "class", "minimum", "maximum",	"set", "description")
  if (format == "json_palmspy") {
    config = fromJSON(txt = file, simplifyDataFrame = TRUE)
    params_info_palmspy_file = system.file("testfiles_palmspy/params_description_palmspy.tsv", package = "HabitusGUI")[1]
    params_info_palmspy = read.table(file = params_info_palmspy_file, sep = "\t", header = TRUE)
    cnt = 0
    if ("gps" %in% names(config) & "accelerometer" %in% names(config)) {
      for (field in c("gps", "accelerometer")) {
        if (field == "gps") {
          subfields = c("general", "filter_options", "trip_detection", "mode_of_transportation" )
        } else if (field == "accelerometer") {
          subfields = c("not_wearing_time", "activity_bout", "sedentary_bout", "activity_classification")
        }
        for (subfield in subfields) {
          pars = "parameters"
          params = as.data.frame(t(as.data.frame(config[[field]][[pars]][[subfield]])))
          colnames(params)[1] = "value"
          params$parameter = rownames(params)
          params_merged = merge(params_info_palmspy, params, by = "parameter")
          rownames(params_merged) = params_merged$parameter
          if (cnt == 0) {
            params_keep = params_merged[, expected_tsv_columns]
          } else {
            params_keep = rbind(params_keep, params_merged[, expected_tsv_columns])
          }
          cnt = cnt + 1
        }
      }
      params = params_keep
    } else {
      warning(paste0("\nparameters section not found in ", file))
      params = c()
    }
  } else if (format == "csv_ggir") {
    params = read.csv(file = file)
    # remove duplicates, because sometimes GGIR config files have duplicates
    dups = duplicated(params)
    params = params[!dups,]
    # Keep only parameters with a matching description in the description file
    params_info_ggir_file = system.file("testfiles_ggir/params_description_ggir.tsv", package = "HabitusGUI")[1]
    params_info_ggir = read.table(file = params_info_ggir_file, sep = "\t", header = TRUE)
    params_merged = merge(params_info_ggir, params, by.x = "parameter", by.y = "argument")
    rownames(params_merged) = params_merged$parameter
    params = params_merged[, expected_tsv_columns]
    params = params[,-which(colnames(params) == "subfield")]
  }
  return(params)
}