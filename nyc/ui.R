library(leaflet)
library(shiny)

shinyUI(fluidPage(
  verticalLayout(
    titlePanel(title=NULL, windowTitle="Moves in New York"),
    leafletOutput("plot", height="550px"),
    wellPanel(fluidRow(
      column(4, checkboxGroupInput("activities", label=NULL,
        choices=list("walking", "cycling", "transport"),
        selected=c("walking", "cycling", "transport"))),
      column(4, checkboxInput("places", label="places", value=TRUE),
        checkboxInput("heatmap", label="heatmap", value=FALSE)),
      column(4, p("This map shows my personal",
        a(href="https://www.moves-app.com", "Moves app"),
        "data for the time I spent at the",
        a(href="https://www.recurse.com", "Recurse Center"),
        "in New York. Source code is available on",
        a(href="https://github.com/ilarischeinin/moves", "GitHub.")))
    ))
  )
))

# EOF
