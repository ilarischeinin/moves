library(data.table)
library(leaflet)
library(shiny)

boating <- readRDS("boating.rds")

shinyServer(function(input, output) {
  output$plot <- renderLeaflet({
    map <- leaflet() %>%
      addTiles() %>%
      fitBounds(20.9, 59.7, 25.4, 60.4) %>%
      addLayersControl(overlayGroups=as.character(unique(year(boating$date))),
        options=layersControlOptions(collapsed=FALSE))
      for (segment in unique(boating$segment)) {
          map <- map %>% addPolylines(data=boating[segment, ],
            ~longitude, ~latitude, col="blue", weight=4, popup=~date[1],
            group=as.character(year(boating[segment, date][1])))
      }
      map
  })
})

# EOF
