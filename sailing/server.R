library(data.table)
library(leaflet)
library(leafletplugins)
library(shiny)

load("sailing.rda")

map <- leaflet() %>%
  fitBounds(18.2, 59.3, 25.5, 60.5) %>%
  addControlFullScreen() %>%
  # addControlGPS() %>% # does not work anymore without https
  addControl(
    actionButton("info", label="?", style="font-size: x-small;"),
    position="bottomright"
  ) %>%
  addCircleMarkers(data=stations, lng=~longitude, lat=~latitude, radius=2L,
    group="FMI Stations", color="black", popup=~popup)

# Navionics map requires a key from:
# http://webapiv2.navionics.com
# and a corresponding token to access map tiles.
if (file.exists("navionics.R")) {
  source("navionics.R")
  map <- map %>%
    addControl(
      a(href="http://www.navionics.com", img(src=navlogo)),
      position="bottomleft"
    ) %>%
    addTiles(
      urlTemplate=paste0("http://backend.navionics.io/tile/{z}/{x}/{y}?",
        "LAYERS=config_1_1_0&TRANSPARENT=FALSE&navtoken=", navtoken),
      attribution=paste0("<a href=",
        '"http://www.navionics.com/en/acknowledgements">Navionics</a> ',
        "&mdash; Not to be used for Navigation"),
      group="Navionics"
    ) %>%
    addLayersControl(
      overlayGroups=c("FMI Stations", unique(years))
    )
  } else {
  map <- map %>%
  addProviderTiles("Esri.WorldImagery", group="Esri World Imagery") %>%
  addProviderTiles("OpenTopoMap", group="OpenTopoMap") %>%
  addTiles(
    urlTemplate="http://tiles.openseamap.org/seamark/{z}/{x}/{y}.png",
    attribution=paste('Map data: &copy;',
    '<a href="http://www.openseamap.org">OpenSeaMap</a> contributors'),
    group="OpenSeaMap") %>%
  addLayersControl(
    baseGroups=c("OpenTopoMap", "Esri World Imagery"),
    overlayGroups=c("OpenSeaMap", "FMI Stations", unique(years))
  )
}

for (segment in unique(tracks$segment)) {
  map <- map %>% addPolylines(data=tracks[segment, ],
    lng=~longitude, lat=~latitude, col="blue", weight=4L, opacity=0.75,
    popup=popups[[segment]], group=years[[segment]])
}

shinyServer(function(input, output) {
  output$leaflet <- renderLeaflet(map)
  onclick("info", toggle("description"))
})

# EOF
