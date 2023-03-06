library(shiny)


# BOOKMARK OPTION

# PROS -------
# App status is stored in url
# Users can easily share the url with preselected app input
# Users can see the changes they make in the url in real time

# CONS -------
# urls get very long

# TO INVESTIGATE -------
# Status not stored for the research goals (conditional check group panel)
# BUT, the selection of these buttons can be seen in the url
# Potential reason: lines 412:418 in myApp.R
# Investigate how to make updateCheckboxGroupInput work with bookmark

ui = function(req) {
  fluidPage(
    radioButtons(inputId = "b1", label = "Button 1", choices = c("Yes", "No")),
    radioButtons(inputId = "b2", label = "Button 2", choices = c("Yes", "No"))
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






