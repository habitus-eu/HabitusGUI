#' modConfigServer
#'
#' @param id ...
#' @param tool ...
#' @param homedir character to specify home directory
#' @return No object returned, this is a shiny module
#' @import shinyFiles
#' @importFrom magrittr %>%
#' @export

modConfigServer = function(id, tool, homedir = getwd()) {
  
  moduleServer(id, function(input, output, session) {
    observeEvent(tool(), {
      if (tool() == "PALMSpy") {
        output$download = downloadHandler(
          filename = "palmspy-params.json",
          content <- function(file) {
            config_default = system.file("testfiles_palmspy/palmspy-params.json", package = "HabitusGUI")[1]
            if (config_default != file) file.copy(config_default, file)
          },
          contentType = "application/json")
      } else if (tool() == "GGIR") {
        output$download = downloadHandler(
          filename = "config.csv",
          content <- function(file) {
            config_default = system.file("testfiles_ggir/config.csv", package = "HabitusGUI")[1]
            if (config_default != file) file.copy(config_default, file)
          },
          contentType = "text/csv")
      }
    })
    
    shinyFileChoose(input, "configfile",  roots = c(home = homedir))
    configfile <- reactive(input$configfile)
    
    observeEvent(input$configfile, {
      # inspired on https://community.rstudio.com/t/saving-editable-dt-table-values-to-reactivevalue-in-shiny/48825
      current_config = as.character(parseFilePaths(c(home = homedir), configfile())$datapath)
      if (length(current_config) > 0) {
        if (tool() == "PALMSpy") {
          params = load_params(file = current_config, format = "json_palmspy") #$datapath
        } else if (tool() == "GGIR") {
          params = load_params(file = current_config, format = "csv_ggir") #$datapath
        }
        params_errors = check_params(params, tool = tool())
        output$config_issues <- renderUI({
          HTML(params_errors$error_message)
        })
        output$config_green <- renderUI({
          HTML(params_errors$green_message)
        })
        v <- reactiveValues(params = params)
        proxy = DT::dataTableProxy("mod_table", session)
        observeEvent(input$mod_table_cell_edit, {
          info = input$mod_table_cell_edit
          i = info$row
          j = info$col
          k = info$value
          modifiable_column = "value" # modifiable columns
          isolate(
            if (j %in% match(modifiable_column, colnames(v$params))) {
              do.replace = TRUE
              v$params[which(v$params$display == TRUE)[i], j] <<- DT::coerceValue(k, v$params[i, j])
            } else {
              do.replace = FALSE
              warning("You are not supposed to change this column.") # check to stop the user from editing only few columns
            }
          )
          if (do.replace == TRUE) {
            DT::replaceData(proxy, v$params, resetPaging = FALSE)  # replaces data displayed by the updated table
            params_errors = check_params(v$params, tool = tool())
            output$config_issues <- renderUI({
              HTML(params_errors$error_message)
            })
            output$config_green <- renderUI({
              HTML(params_errors$green_message)
            })
            if (nrow(params_errors$blocked_params) != 0) {
              v$params$display[which(rownames(v$params) %in% params_errors$blocked_params$name == TRUE)] = TRUE
            }
            # Auto-save after every change
            if (tool() == "PALMSpy") {
              update_params(new_params = v$params, file = current_config, format = "json_palmspy") #$datapath
            } else if (tool() == "GGIR") {
              update_params(new_params = v$params, file = current_config, format = "csv_ggir") #$datapath
            }
          }
        })
        ### Reset Table
        observeEvent(input$reset, {
          showNotification("Resetting values", type = "message")
          v$params <- params # your default data
          current_config = as.character(parseFilePaths(c(home = homedir), configfile())$datapath)
          # also saving to file
          if (tool() == "PALMSpy") { 
            update_params(new_params = v$params, file = current_config, format = "json_palmspy") #$datapath
          } else if (tool() == "GGIR") {
            update_params(new_params = v$params, file = current_config, format = "csv_ggir") #$datapath
          }
          # update list with errors
          params_errors = check_params(params, tool = tool())
          output$config_issues <- renderUI({
            HTML(params_errors$error_message)
          })
          output$config_green <- renderUI({
            HTML(params_errors$green_message)
          })
        })
        # Prepare data to be visualised:
        rows2show = which(v$params$display == TRUE)
        v$params = v$params[order(v$params$priority, decreasing = TRUE),]
        cols2show = which(colnames(v$params) %in% c("class", "minimum", "maximum",	"set", "display") == FALSE)
        data2vis = reactive(v$params[rows2show, cols2show])
        # Render table for use in UI
        output$mod_table <- DT::renderDT({
            DT::datatable(data2vis(), editable = TRUE,
                        options = list(lengthMenu = list(c(5, 10, -1), c('5', '10', 'All')),
                                       pageLength = 5
                                       # , columnDefs = list(list(targets = 'priority', visible = FALSE))
                        )) %>% DT::formatStyle(
                          'value', 'priority',
                          backgroundColor = DT::styleEqual(c("0", "1"), c('gray91', 'lightyellow'))
                        )
          # editable = list(target = "column", disable = list(columns = c(2,3,4))), #< would be nice, but seems to disable reset option
        })
        output$config_instruction <- renderText({
          "Review the parameter values, especially the ones in yellow, and edit where needed by double clicking:"
        })
        
      }
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
    
    # return filepath, such that this can be used outside this module
    reactive(as.character(parseFilePaths(c(home = homedir),configfile())$datapath))
  })
}
