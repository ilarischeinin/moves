library(leaflet)
library(shiny)
library(shinyjs)

shinyUI(
  fillPage(title="Boating Moves",
    useShinyjs(),
    inlineCSS(
      ".leaflet-bottom .legend { padding: 0; background: rgba(0, 0, 0, 0); }"
    ),
    leafletOutput("leaflet", height="100%"),
    absolutePanel(id="description", bottom=50L, left=10L, right=10L,
      style="display: none; font-size: small;",
      wellPanel(
        "This map shows my personal", a(href="https://www.moves-app.com",
        "Moves app"), "data for boating. Clicking a track brings up more",
        "details, including weather and wave observations. They are obtained",
        "from", a(href="http://en.ilmatieteenlaitos.fi", "FMI's"), "nearest",
        "weather station or wave buoy, when not further away than 30 NM.",
        "Tracking accuracy is lower than with proper chart plotters, and",
        "there are clear errors when close to shore as Moves has a",
        "tendency to place the location on known roads. Tracks and all values",
        "should therefore be taken with a grain of salt.",
        "Source code is available on",
        a(href="https://github.com/ilarischeinin/moves", "GitHub")
      )
    )
  )
)

# EOF
