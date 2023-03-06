library(shiny)

ui = function(req) {
  fluidPage(
    radioButtons(inputId = "b1", label = "Button 1", choices = c("Yes", "No")),
    radioButtons(inputId = "b2", label = "Button 2", choices = c("Yes", "No"))
    # verbatimTextOutput("value1"),
    # verbatimTextOutput("value2"),
  )
}

server = function(input, output, session) {
  observe({
    # Trigger this observer every time an input changes
    reactiveValuesToList(input)
    session$doBookmark()
  })
  onBookmarked(function(url) {
    updateQueryString(url)
  })
  output$value1 <- renderText({ input$b1 })
  output$value2 <- renderText({ input$b2 })
}

shinyApp(ui, server, enableBookmarking = "url")



