library(leaflet)
library(shiny)

shinyUI(fluidPage(
  verticalLayout(
    leafletOutput("plot", height="550px"),
    wellPanel(fluidRow(
      column(4, checkboxGroupInput("activities", label=NULL,
        choices=list("walking", "cycling", "transport"),
        selected=c("walking", "cycling", "transport"))),
      column(4, checkboxInput("places", label="places", value=TRUE),
        checkboxInput("heatmap", label="heatmap", value=FALSE))
    ))
  )
))

# EOF
