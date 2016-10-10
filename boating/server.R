library(data.table)
library(leaflet)
library(leafletplugins)
library(shiny)

load("boating.rda")

map <- leaflet() %>%
  addProviderTiles("Esri.WorldImagery", group="Esri World Imagery") %>%
  addProviderTiles("Hydda.Full", group="Hydda") %>%
  addProviderTiles("OpenTopoMap", group="OpenTopoMap") %>%
  addTiles(
    urlTemplate="http://tiles.openseamap.org/seamark/{z}/{x}/{y}.png",
    attribution=paste('Map data: &copy;',
    '<a href="http://www.openseamap.org">OpenSeaMap</a> contributors'),
    group="OpenSeaMap") %>%
  fitBounds(18.2, 59.3, 25.5, 60.5) %>%
  # addRectangles(18.2, 59.3, 25.5, 60.5) %>%
  addLayersControl(
      baseGroups=c("OpenTopoMap", "Hydda", "Esri World Imagery"),
      overlayGroups=c("OpenSeaMap", "Weather stations and buoys", 
        unique(years))
    ) %>%
    addControlFullScreen() %>%
    # addControlGPS() %>%
    addCircleMarkers(data=stations, lng=~longitude, lat=~latitude, radius=2L,
      group="Weather stations and buoys", color="black",
      popup=~popup)
for (segment in unique(tracks$segment)) {
  map <- map %>% addPolylines(data=tracks[segment, ],
    lng=~longitude, lat=~latitude, col="blue", weight=4L,
    popup=popups[[segment]],
    group=years[[segment]])
}

shinyServer(function(input, output) {
  output$leaflet <- renderLeaflet(map)
  observeEvent(input$hide, {hide("description"); show("show")})
  observeEvent(input$show, {show("description"); hide("show")})
})

# EOF
