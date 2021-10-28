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
    # eventReactive(create_template_config(), {
    if (tool() == "PALMSpy") {
      output$download = downloadHandler(
        filename = "palmspy-params.json",
        content <- function(file) {
          config_default = system.file("testfiles_palmspy/palmspy-params.json", package = "HabitusGUI")[1]
          file.copy(config_default, file)
        },
        contentType = "application/json")
    } else if (tool() == "GGIR") {
      output$download = downloadHandler(
        filename = "config.csv",
        content <- function(file) {
          config_default = system.file("testfiles_ggir/config.csv", package = "HabitusGUI")[1]
          file.copy(config_default, file)
        },
        contentType = "text/csv")
    }
   
    observeEvent(input$configfile, {
      # inspired on https://community.rstudio.com/t/saving-editable-dt-table-values-to-reactivevalue-in-shiny/48825
      if (tool() == "PALMSpy") {
        params = load_params(file = input$configfile$datapath, format = "json_palmspy")
      } else if (tool() == "GGIR") {
        params = load_params(file = input$configfile$datapath, format = "csv_ggir")
      }
      v <- reactiveValues(params=params)
      proxy = DT::dataTableProxy("mod_table")
      observeEvent(input$mod_table_cell_edit, {
        info = input$mod_table_cell_edit
        i = info$row
        j = info$col
        k = info$value
        modifiable_column = "value" # modifiable columns
        isolate(
          if (j %in% match(modifiable_column, colnames(v$params))) {
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
        if (tool() == "PALMSpy") {
          update_params(new_params = v$params, file = input$configfile$datapath, format="json_palmspy")
        } else if (tool() == "GGIR") {
          update_params(new_params = v$params, file = input$configfile$datapath, format="csv_ggir")
        }
      })
      # Render table for use in UI
      output$mod_table <- DT::renderDataTable({
        DT::datatable(v$params, editable=TRUE,
                      options = list(lengthMenu = list(c(5, 10, -1), c('5', '10', 'All')),
                                                    pageLength = 5))
                      # editable = list(target = "column", disable = list(columns = c(2,3,4))), #< would be nice, but seems to disable reset option
      })
    })
    
    output$config_explanation <- renderText({
      if(tool() == "GGIR"){
        explanation = paste0("GGIR takes as input accelerometer data expressed in universal units ",
                             "of gravitational acceleration and offers a broad analysis spanning: ",
                             "data quality, sleep, physical activity, behaviour fragmentation, and ",
                             "circadian rhythms. For additional information see: https://cran.r-project.org/package=GGIR/vignettes/GGIR.html")
      } else if (tool() == "PALMSpy"){
        explanation = paste0("PALMSpy takes as input summarised accelerometer data (ActiGraph counts) ",
                             "and GPS data and uses them to estimate movement behaviours from the ",
                             "perspective location in a country or city and travel distance and speed")
      }
      explanation
    })
    
    output$config_instruction <- renderText({
      if(tool() == "GGIR"){
        config_instruction = "Select your GGIR configuration file (.csv) or if you do not have one Download a template:"
      } else if (tool() == "PALMSpy"){
        config_instruction = "Select your PALMSpy config file file (.json) or if you do not have one Download a template:"
      }
      config_instruction
    })
    
    # output$config_explanation = renderText({  explanation() })
    reactive(input$configfile$datapath) # return filepath, such that this can be used outside this module
  })
}
