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
        modifiable_column = "value" # modifiable columns
        # str(info)
        
        isolate(
          if (j %in% match(modifiable_column, colnames(v$params))) {
            # print(match(modifiable_params, rownames(v$params)))
            print("trying to coerceValues")
            v$params[i, j] <<- DT::coerceValue(k, v$params[i, j])
          } else {
            stop("You are not supposed to change this column.") # check to stop the user from editing only few columns
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
        update_params(new_params = v$params, file = input$configfile$datapath)
      })
      # print(isolate(rownames(v$params)))
      output$mod_table <- DT::renderDataTable({
        DT::datatable(v$params,
                      editable = list(target = "column", disable = list(columns = c(2,3,4))),
                      options = list(lengthMenu = list(c(5, 10, -1), c('5', '10', 'All')),
                                     pageLength = 5
                      ))
      })
    })
    reactive(input$configfile$datapath) # return filepath
  })
}
