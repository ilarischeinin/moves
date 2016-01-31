library(leaflet)
library(shiny)
library(shinyjs)

shinyUI(
  fillPage(title="Moves in New York",
    useShinyjs(),
    leafletOutput("leaflet", height="100%"),
    absolutePanel(id="description", bottom=10L, left=10L,
      wellPanel(
        actionButton("hide", label="X",
          style="margin-right: 1em; font-size: x-small;"),
        "This map shows my personal",
        a(href="https://www.moves-app.com", "Moves app"),
        "data for the time I spent at the",
        a(href="https://www.recurse.com", "Recurse Center"),
        "in New York. It takes a while to load and is slow to operate,",
        "so please be patient. Source code is available on",
        a(href="https://github.com/ilarischeinin/moves", "GitHub.")
      )
    ),
    absolutePanel(bottom=10L, left=10L,
      actionButton("show", label="?",
        style="display: none; font-size: x-small;")
    )
  )
)

# EOF
