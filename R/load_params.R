#' load_params
#'
#' @param file Character to specify location of configuration file
#' @param format Character to specify format of configuration file: json_palsmpy, csv_GGIR, csv_hbGIS, csv_hbGPS
#' @return list of parameters extract from the configuration file
#' @importFrom jsonlite fromJSON
#' @importFrom utils read.csv read.table
#' @export


load_params = function(file=c(), format="json_palmspy") {
  expected_tsv_columns = c("value", "field", "subfield", "display", "class", "minimum",
                           "maximum",	"set", "description", "priority")
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
          params = as.data.frame(as.matrix(config[[field]][[pars]][[subfield]]))
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
    dups = duplicated(params_merged)
    params_merged = params_merged[!dups,]
    rownames(params_merged) = params_merged$parameter
    params = params_merged[, expected_tsv_columns]
    params = params[,-which(colnames(params) == "subfield")]
  } else if (format == "csv_hbGPS") {
    params = read.csv(file = file)
    # remove duplicates, in case hbGPS config files have duplicates
    dups = duplicated(params)
    params = params[!dups,]
    # Keep only parameters with a matching description in the description file
    params_info_hbGPS_file = system.file("testfiles_hbGPS/params_description_hbGPS.tsv", package = "HabitusGUI")[1]
    params_info_hbGPS = read.table(file = params_info_hbGPS_file, sep = "\t", header = TRUE)
    params_merged = merge(params_info_hbGPS, params, by.x = "parameter", by.y = "argument")
    dups = duplicated(params_merged)
    params_merged = params_merged[!dups,]
    rownames(params_merged) = params_merged$parameter
    params = params_merged[, expected_tsv_columns]
    params = params[,-which(colnames(params) == "subfield")]
  } else if (format == "csv_hbGIS") {
    params = read.csv(file = file, sep = ",")
    # remove duplicates, because sometimes GGIR config files have duplicates
    dups = duplicated(params)
    params = params[!dups,]
    # Keep only parameters with a matching description in the description file
    params_info_hbGIS_file = system.file("testfiles_hbGIS/params_description_hbGIS.tsv", package = "HabitusGUI")[1]
    params_info_hbGIS = read.table(file = params_info_hbGIS_file, sep = "\t", header = TRUE)
    params_info_hbGIS$id = with(params_info_hbGIS, paste0(field,  "__",  parameter))
    params$id = with(params, paste0(params$context, "__", params$name))
    params_merged = merge(params_info_hbGIS, params, by.x = "id", by.y = "id")
    dups = duplicated(params_merged)
    params_merged = params_merged[!dups,]
    rownames(params_merged) = params_merged$id
    colnames(params_merged)[which(colnames(params_merged) == "formula")] = "value"
    colnames(params_merged)[which(colnames(params_merged) == "name")] = "subfield"
    expected_tsv_columns = c(expected_tsv_columns, "is_where_field", "after_conversion")
    params = params_merged[, expected_tsv_columns]
    params = params[,-which(colnames(params) %in% c("subfield", "id", "field"))]
  }
  return(params)
}