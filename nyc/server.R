library(leaflet)
library(shiny)

map <- readRDS("nyc.rds")

shinyServer(function(input, output) {
  output$leaflet <- renderLeaflet(map)
  observeEvent(input$hide, {hide("description"); show("show")})
  observeEvent(input$show, {show("description"); hide("show")})
})

# EOF
