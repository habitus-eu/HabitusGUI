#' modConfigServer
#'
#' @param id ...
#' @param reset ...
#' @param save ...
#' @param tool ...
#' @return No object returned, this is a shiny module
#' @export

modConfigServer = function(id, reset, save, tool) {
  stopifnot(is.reactive(reset))
  stopifnot(is.reactive(save))
  
  moduleServer(id, function(input, output, session) {
    observeEvent(input$configfile, {
      if (tool() == "PALMSpy") {
        params = load_params(file = input$configfile$datapath, format = "json_palmspy")
      } else if (tool() == "GGIR") {
        params = load_params(file = input$configfile$datapath, format = "csv_ggir")
      }
      v <- reactiveValues(params=params)
      # print(is.reactive(v))
      # print(isolate(v$params))
      proxy = DT::dataTableProxy("mod_table")
      # print(v)
      observeEvent(input$mod_table_cell_edit, {
        info = input$mod_table_cell_edit
        # str(info)
        i = info$row
        j = info$col
        k = info$value
        if (tool() == "PALMSpy") {
          modifiable_params = c("gps.path", "acc.path", "interval",
                                "insert.missing_flag", "insert.until_flag", "insert.max.seconds",
                                "filter.invalid.values_flag", "max.speed", "max.ele.change",
                                "include.acc_flag", "minutes.zeros.row", "detect.activity.bouts_flag",
                                "activity.bout.duration", "activity.bout.up", "activity.bout.low",
                                "activity.bout.tol", "hard.cut", "moderate.cut", "light.cut",
                                "merge.acc.to.gps_flag")
        } else if (tool() == "GGIR") {
          modifiable_params = c("windowsizes", "desiredtz", "idloc", "timethreshold",
                                "colid", "coln1","criterror", "def.noc.sleep",
                                "do.visual", "excludefirstlast", "includenightcrit",
                                "nnights", "outliers.only", "relyonsleeplog", "sleeplogidnum",
                                "boutcriter.in", "boutcriter.lig", "boutcriter.mvpa", "boutdur.in",
                                "boutdur.lig", "boutdur.mvpa", "excludefirstlast.part5", "save_ms5rawlevels",
                                "threshold.lig", "threshold.mod", "threshold.vig", "timewindow",
                                "boutcriter", "closedbout", "epochvalues2csv", "hrs.del.end", "hrs.del.start", "iglevels",  
                                "ilevels", "includedaycrit", "IVIS_epochsize_seconds", "IVIS_windowsize_minutes",
                                "IVIS.activity.metric", "M5L5res", "maxdur", "mvpadur",
                                "mvpathreshold", "ndayswindow", "qlevels", "qM5L5", "qwindow", "strategy",
                                "TimeSegments2ZeroFile", "window.summary.size", "winhr",
                                "dofirstpage", "viewingwindow", "visualreport")
        }
        # str(info)
        
        isolate(
          if (i %in% match(modifiable_params, rownames(v$params))) {
            # print(match(modifiable_params, rownames(v$params)))
            print("trying to coerceValues")
            v$params[i, j] <<- DT::coerceValue(k, v$params[i, j])
          } else {
            stop("You are not supposed to change this row.") # check to stop the user from editing only few columns
          }
        )
        DT::replaceData(proxy, v$params, resetPaging = FALSE)  # replaces data displayed by the updated table
      })
      
      ### Reset Table
      observeEvent(reset(), {
        v$params <- params # your default data
      })
      
      # ### Save table to file
      observeEvent(save(), {
        save_params(new_params = v$params, file = input$configfile$datapath)
      })
      # print(isolate(rownames(v$params)))
      output$mod_table <- DT::renderDataTable({
        DT::datatable(v$params, editable = TRUE)
      })
    })
    reactive(input$configfile$datapath) # return filepath
  })
}
