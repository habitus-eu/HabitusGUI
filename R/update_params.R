#' update_params
#'
#' @param file Character to specify location of original configuration file
#' @param format Character to specify format of configuration file: json_palsmpy, csv_GGIR, or csv_palmsplusr
#' @param new_params New parameters
#' @return No object returned, function only reads original data, and overwrites parameters and stores it again
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom utils read.csv write.csv
#' @export

update_params = function(new_params = c(), file = c(), format="json_palmspy") {
  if (format == "json_palmspy") {
    config = fromJSON(txt = file, simplifyDataFrame = TRUE)
    if ("gps" %in% names(config) & "accelerometer" %in% names(config)) {
      for (field in c("gps", "accelerometer")) {
        if (field == "gps") {
          subfields = c("general", "filter_options", "trip_detection", "mode_of_transportation" )
        } else if (field == "accelerometer") {
          subfields = c("not_wearing_time", "activity_bout", "sedentary_bout", "activity_classification")
        }
        for (subfield in subfields) {
          pars = "parameters"
          rowi = which(new_params$field == field & new_params$subfield == subfield)
          if (length(rowi) > 0) {
            for (j in 1:length(rowi)) {
              new = as.list(t(new_params[rowi[j], "value"]))
              names(new) = rownames(new_params[rowi[j],])
              item_to_replace = which(names(config[[field]][[pars]][[subfield]]) == names(new))
              config[[field]][[pars]][[subfield]][item_to_replace] = new
            }
          }
        }
      }
    } else {
      warning(paste0("\nparameters section not found in ", file))
    }
    exportJson <- toJSON(x = config, auto_unbox = TRUE)
    write(exportJson, file = file)
  } else if (format == "csv_ggir") {
    params = read.csv(file = file)
    # remove duplicates, because sometimes GGIR config files have duplicates
    dups = duplicated(params$argument)
    params = params[!dups,]
    rownames(params) = params$argument
    # only overwrite the matching fields
    for (j in 1:nrow(new_params)) {
      ind = which(rownames(params) %in% rownames(new_params)[j] == TRUE)
      params$value[ind] = new_params$value[j]
    }
    write.csv(x = params, file = file, row.names = FALSE)
  } else if (format == "csv_palmsplusr") {
    params = read.csv(file = file, sep = ",")
    # remove duplicates, just in case palmsplusr config files have duplicates
    params$argument = with(params, paste0(params$context, "__",params$name))
    dups = duplicated(params$argument)
    params = params[!dups,]
    rownames(params) = params$argument
    # only overwrite the matching fields
    for (j in 1:nrow(new_params)) {
      ind = which(rownames(params) %in% rownames(new_params)[j] == TRUE)
      if (length(ind) > 0) {
        if (new_params$value[j] != params$formula[ind]) {
          params$formula[ind] = new_params$value[j]
        }
      }
    }
    params = params[,-which(colnames(params) %in% c("argument"))]
    write.csv(x = params, file = file, row.names = FALSE)
  }
}