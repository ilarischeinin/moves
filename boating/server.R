library(data.table)
library(leaflet)
library(shiny)

boating <- readRDS("boating.rds")

map <- leaflet() %>%
  addProviderTiles("Hydda.Full", group="Hydda") %>%
  addProviderTiles("OpenTopoMap", group="OpenTopoMap") %>%
  addProviderTiles("Esri.WorldImagery", group="Esri World Imagery") %>%
  addTiles(
    urlTemplate="http://tiles.openseamap.org/seamark/{z}/{x}/{y}.png",
    attribution=paste('Map data: &copy;',
    '<a href="http://www.openseamap.org">OpenSeaMap</a> contributors'),
    group="OpenSeaMap") %>%
  fitBounds(20.9, 59.7, 25.4, 60.4) %>%
  addLayersControl(
    baseGroups=c("Hydda", "OpenTopoMap", "Esri World Imagery"),
    overlayGroups=c("OpenSeaMap", as.character(unique(year(boating$date)))),
    options=layersControlOptions(collapsed=FALSE))
for (segment in unique(boating$segment)) {
  map <- map %>% addPolylines(data=boating[segment, ],
    lng=~longitude, lat=~latitude, col="blue", weight=4, popup=~date[1],
    group=~as.character(year(date[1])))
}

shinyServer(function(input, output) {
  output$leaflet <- renderLeaflet(map)
  observeEvent(input$hide, {hide("description"); show("show")})
  observeEvent(input$show, {show("description"); hide("show")})
})

# EOF
