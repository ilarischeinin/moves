library(leaflet)
library(shiny)

shinyUI(fluidPage(
  verticalLayout(
    titlePanel(title=NULL, windowTitle="Moves in New York"),
    leafletOutput("plot", height="600px"),
    wellPanel(
      p("This map shows my personal",
      a(href="https://www.moves-app.com", "Moves app"),
      "data for the time I spent at the",
      a(href="https://www.recurse.com", "Recurse Center"),
      "in New York. It takes a while to load, so please be patient.",
      "Source code is available on",
      a(href="https://github.com/ilarischeinin/moves", "GitHub."))
    )
  )
))

# EOF
