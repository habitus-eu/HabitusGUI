#' modConfigServer
#'
#' @param id ...
#' @param reset ...
#' @param save ...
#' @param configfile ...
#' @param tool ...
#' @return No object returned, this is a shiny module
#' @export

modConfigServer = function(id, reset, save, configfile, tool) {
  stopifnot(is.reactive(reset))
  stopifnot(is.reactive(save))
  stopifnot(is.reactive(configfile))
  
  moduleServer(id, function(input, output, session) {
    observeEvent(configfile(), {
      # print(isolate(configfile()))
      if (tool() == "PALMSpy") {
        params = load_params(file = configfile()$datapath, format="json_palsmpy")
      } else if (tool() == "GGIR") {
        params = load_params(file = configfile()$datapath, format="csv_GGIR")
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
        modifiable_params = c("gps.path", "acc.path", "interval",
                              "insert.missing_flag", "insert.until_flag", "insert.max.seconds",
                              "filter.invalid.values_flag", "max.speed", "max.ele.change",
                              "include.acc_flag", "minutes.zeros.row", "detect.activity.bouts_flag",
                              "activity.bout.duration", "activity.bout.up", "activity.bout.low",
                              "activity.bout.tol", "hard.cut", "moderate.cut", "light.cut",
                              "merge.acc.to.gps_flag")
        # str(info)
        
        isolate(
          if (i %in% match(modifiable_params, rownames(v$params))) {
            # print(match(modifiable_params, rownames(v$params)))
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
        save_params(new_params = v$params, file = configfile()$datapath)
      })
      # print(isolate(rownames(v$params)))
      output$mod_table <- DT::renderDataTable({
        DT::datatable(v$params, editable = TRUE)
      })
    })
    reactive(configfile()$datapath) # return filepath
  })
}
