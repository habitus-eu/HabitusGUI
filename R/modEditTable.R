#' smodEditTable
#'
#' @param input ...
#' @param output ...
#' @param session ...
#' @param data ...
#' @param reset ...
#' @param save ...
#' @param configfile ...
#' @return No object returned, this is a shiny module
#' @export

modEditTable <- function(input, output, session, data,reset, save, configfile) {
  
  v <- reactiveValues(data = data)
  
  proxy = DT::dataTableProxy("mod_table")
  
  observeEvent(input$mod_table_cell_edit, {
    print(rownames(v$data))
    info = input$mod_table_cell_edit
    # str(info)
    i = info$row
    j = info$col
    k = info$value
    # str(info)
    
    isolate(
      if (i %in% match(c("ratio","cost","updated_price"), rownames(v$data))) {
        print(match(c("ratio","cost", "updated_price"), rownames(v$data)))
        v$data[i, j] <<- DT::coerceValue(k, v$data[i, j])
        print(v$data)
        
        if (i %in% match("cost", names(v$data))) {
          v$data$updated_price <<- v$data$cost * v$data$ratio
        }
        if (i %in% match("ratio", names(v$data))) {
          v$data$updated_price <<- v$data$cost * v$data$ratio
        }
      } else {
        stop("You are not supposed to change this row.") # check to stop the user from editing only few columns
      }
    )
    DT::replaceData(proxy, v$data, resetPaging = FALSE)  # replaces data displayed by the updated table
  })
  
  ### Reset Table
  observeEvent(reset(), {
    v$data <- data # your default data
  })
  
  # ### Save table to file
  observeEvent(save(), {
    save_palmspy_params(new_params = v$data, file = configfile) 
  })
  
  print(isolate(colnames(v$data)))
  output$mod_table <- DT::renderDataTable({
    DT::datatable(v$data, editable = TRUE)
  })
}