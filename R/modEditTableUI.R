modEditTableUI <- function(id) {
  ns <- NS(id)
  DT::dataTableOutput(ns("mod_table"))
}