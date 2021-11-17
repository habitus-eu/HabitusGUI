#' modConfigServer
#'
#' @param id ...
#' @param tool ...
#' @return No object returned, this is a shiny module
#' @export

modConfigServer = function(id, tool) {

  moduleServer(id, function(input, output, session) {
    observeEvent(tool(), {
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
    })
   
    observeEvent(input$configfile, {
      # inspired on https://community.rstudio.com/t/saving-editable-dt-table-values-to-reactivevalue-in-shiny/48825
      if (tool() == "PALMSpy") {
        params = load_params(file = input$configfile$datapath, format = "json_palmspy")
      } else if (tool() == "GGIR") {
        params = load_params(file = input$configfile$datapath, format = "csv_ggir")
      }
      params_errors = check_params(params)
      output$config_issues <- renderUI({
        HTML(params_errors$error_message)
      })
      output$config_green <- renderUI({
        HTML(params_errors$green_message)
      })
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
        
        params_errors = check_params(v$params)
        output$config_issues <- renderUI({
          HTML(params_errors$error_message)
        })
        output$config_green <- renderUI({
          HTML(params_errors$green_message)
        })
        if (nrow(params_errors$blocked_params) == 0) {
          # Only show Saving sign when no errors were found
          showNotification("Saving changes", type = "message")
        }
        # Auto-save after every change
        if (tool() == "PALMSpy") {
          update_params(new_params = v$params, file = input$configfile$datapath, format = "json_palmspy")
        } else if (tool() == "GGIR") {
          update_params(new_params = v$params, file = input$configfile$datapath, format = "csv_ggir")
        }
      })
      
      ### Reset Table
      observeEvent(input$reset, {
        showNotification("Resetting values", type = "message")
        v$params <- params # your default data
        # also saving to file
        if (tool() == "PALMSpy") { 
          update_params(new_params = v$params, file = input$configfile$datapath, format = "json_palmspy")
        } else if (tool() == "GGIR") {
          update_params(new_params = v$params, file = input$configfile$datapath, format = "csv_ggir")
        }
        # update list with errors
        params_errors = check_params(params)
        output$config_issues <- renderUI({
          HTML(params_errors$error_message)
        })
        output$config_green <- renderUI({
          HTML(params_errors$green_message)
        })
        
      })

      # Render table for use in UI
      output$mod_table <- DT::renderDataTable({
        cols2show = which(colnames(v$params) %in% c("class", "minimum", "maximum",	"set") == FALSE)
        DT::datatable(v$params[cols2show], editable = TRUE,
                      options = list(lengthMenu = list(c(5, 10, -1), c('5', '10', 'All')),
                                                    pageLength = 5))
                      # editable = list(target = "column", disable = list(columns = c(2,3,4))), #< would be nice, but seems to disable reset option
      })
      output$config_instruction <- renderText({
        "Review the parameter values and edit where needed by double clicking:"
      })
    })
    output$config_instruction <- renderText({ # the default output before the configuration file is selected
      "Select a configuration file on the left. Download the template if you do not have a configuration file."
    })
    output$config_explanation1 <- renderText({
      if (tool() == "GGIR") {
        explanation = paste0("GGIR takes as input accelerometer data expressed in universal units ",
                             "of gravitational acceleration and offers a broad analysis spanning: ",
                             "data quality, sleep, physical activity, behaviour fragmentation, and ",
                             "circadian rhythms. For additional information see: https://cran.r-project.org/package=GGIR/vignettes/GGIR.html")
      } else if (tool() == "PALMSpy") {
        explanation = paste0("PALMSpy takes as input summarised accelerometer data (ActiGraph counts) ",
                             "and GPS data and uses them to estimate movement behaviours from the ",
                             "perspective location in a country or city and travel distance and speed")
      }
      explanation
    })
    
    output$config_explanation2 <- renderText({
      if (tool() == "GGIR") {
        config_explanation2 = "GGIR configuration files are in .csv format. If you do not have one Download a template below."
      } else if (tool() == "PALMSpy") {
        config_explanation2 = "PALMSpy configuration files are in .json. If you do not have one Download a template below."
      }
      config_explanation2
    })
    
    
  
    
    # # Inform UI that file has been upload such that save and reset button can be displayed
    # getData <- reactive({
    #   if(is.null(input$configfile)) return(NULL)
    # })
    # output$configfileUploaded <- reactive({
    #   return(!is.null(getData()))
    # })
    # outputOptions(output, 'configfileUploaded', suspendWhenHidden=FALSE)
    
    # return filepath, such that this can be used outside this module
    reactive(input$configfile$datapath)
  })
}
