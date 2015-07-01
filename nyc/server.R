library(leaflet)
library(shiny)
library(data.table)

load("nyc.rda")

cols <- c(walking="green",
  cycling="cyan",
  tram="black",
  underground="black",
  train="black",
  bus="black",
  car="black",
  helicopter="black",
  ferry="blue",
  boat="blue")

m <- leaflet() %>%
  addTiles() %>%
  # setView(-73.5, 40.75, zoom=12) %>%
  # fitBounds(-74.03,  40.70, -73.94, 40.79) %>%
  mapOptions(zoomToLimits="first")

shinyServer(function(input, output) {
  output$plot <- renderLeaflet({
    activities <- input$activities
    if ("transport" %in% activities)
      activities <- c(activities, "underground", "car", "bus", "train",
        "tram", "helicopter", "boat", "ferry")
    for (segment in unique(nycactivities$segment)) {
      if (!nycactivities[segment, activity][1] %in% activities)
       next
      color <- as.character(cols[nycactivities[segment, activity][1]])
      m <- m %>%
        # addPolylines(nycactivities[segment, longitude],
        #   nycactivities[segment, latitude],
        #   col=as.character(cols[nycactivities[segment, activity][1]]), weight=3)
        addPolylines(data=nycactivities[segment], ~longitude, ~latitude,
          col=color, weight=4, popup=~activity)
    }
    if (input$heatmap) {
      for (i in seq_along(nyccontours)) {
        m <- m %>%
          addPolygons(nyccontours[[i]]$x, nyccontours[[i]]$y,
            color="black", weight=1, fillColor="red")
      }
    }
    if (input$places)
      m <- m %>% addCircles(data=nycplaces, ~longitude, ~latitude,
        col="yellow", popup=~name)
    m
  })
})

# EOF
