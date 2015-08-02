library(leaflet)
library(shiny)

shinyUI(
  fluidPage(verticalLayout(
    titlePanel(title=NULL, windowTitle="Boating Moves"),
    tags$style(type="text/css", "html, body {width:100%; height:100%}"),
    leafletOutput("plot", height="600px"),
    wellPanel(p("This map shows my personal",
      a(href="https://www.moves-app.com", "Moves app"),
      "data for boating. Source code is available on",
      a(href="https://github.com/ilarischeinin/moves", "GitHub.")))
  ))
)

# EOF
