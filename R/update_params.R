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
    # match acc.metric with do.metric arguments
    do_metrics = c("do.zcx", "do.zcy", "do.zcz", 
                   "do.en", "do.enmo", "do.enmoa",
                   "do.lfen", "do.lfenmo",
                   "do.bfen", "do.hfen", "do.hfenplus",
                   "do.mad", "do.brondcounts", "do.neishabouricounts",
                   "do.roll_med_acc_x", "do.roll_med_acc_y", "do.roll_med_acc_z", 
                   "do.dev_roll_med_acc_x", "do.dev_roll_med_acc_y", "do.dev_roll_med_acc_z",
                   "do.lfx", "do.lfy", "do.lfz", "do.hfx", "do.hfy", "do.hfz",
                   "do.bfx", "do.bfy", "do.bfz")
    acc.metric = params["acc.metric", "value"]
    # if neishabouri counts, remove reference to axis
    if (grepl("Neishabouri", acc.metric)) acc.metric = gsub("_x|_y|_z|_vm", "", acc.metric)
    do_argument = grep(acc.metric, rownames(params), value = TRUE, ignore.case = TRUE)
    # to avoid detecting also enmoa and lfenmo when selecting enmo 
    # (this does not occur with any other metric):
    if (acc.metric == "ENMO") do_argument = "do.enmo"
    # set metric of interest to TRUE and rest to FALSE
    params[do_argument, "value"] = TRUE
    params[do_metrics[-which(do_metrics == do_argument)], "value"] = FALSE 
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