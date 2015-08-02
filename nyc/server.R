library(data.table)
library(leaflet)
library(shiny)

load("nyc.rda")

cols <- c(walking="green",
  running="green",
  cycling="cyan",
  tram="black",
  underground="black",
  train="black",
  bus="black",
  car="black",
  helicopter="black",
  ferry="blue",
  boat="blue")

groups <- c(walking="walking",
  running="walking",
  cycling="cycling",
  tram="transport",
  underground="transport",
  train="transport",
  bus="transport",
  car="transport",
  helicopter="transport",
  ferry="transport",
  boat="transport")

shinyServer(function(input, output) {
  output$plot <- renderLeaflet({
    m <- leaflet() %>%
      addTiles() %>%
      addLayersControl(overlayGroups=c("walking", "cycling", "transport",
        "places", "heatmap"), options=layersControlOptions(collapsed=FALSE))
    for (i in seq_along(nyccontours)) {
      m <- m %>%
        addPolygons(nyccontours[[i]]$x, nyccontours[[i]]$y,
          color="black", weight=1, fillColor="red", group="heatmap")
    }
    for (segment in unique(nycactivities$segment)) {
      color <- as.character(cols[nycactivities[segment, activity][1]])
      group <- as.character(groups[nycactivities[segment, activity][1]])
      m <- m %>%
        addPolylines(data=nycactivities[segment], ~longitude, ~latitude,
          col=color, weight=4, popup=~activity, group=group)
    }
    m <- m %>% addCircles(data=nycplaces, ~longitude, ~latitude,
      col="yellow", popup=~name, group="places")
    m %>%
      hideGroup(c("places", "heatmap"))
  })
})

# EOF
