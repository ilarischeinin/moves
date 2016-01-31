#!/usr/bin/Rscript

suppressMessages({
  library(data.table)
  library(dplyr)
  library(KernSmooth)
  library(leaflet)
  library(pbapply)
  library(XML)
})

d <- commandArgs(TRUE)[1]
if (is.na(d))
  d <- "."

source("library.R")

# tracks

tracks <- get_tracks(d)

tracks[tracks$date == "2015-05-24" & tracks$activity == "transport",
  "activity"] <- "helicopter"

# The app seems to have a tendency to locate me on roads, which seems to be a
# problem especially along the shores of the East River, where it frequently
# placed me on the wrong side of the river, hopping back and forth. In the
# beginning I was fixing these manually, but then got tired of it.
# fix errors in data
# tracks <- tracks %>%
#   filter(!(date == as.IDate("2015-02-28") & latitude < 40.705)) %>%
#   filter(!(date == as.IDate("2015-04-24") & longitude > -73.97)) %>%
#   filter(!(date == as.IDate("2015-04-24") & latitude < 40.7046)) %>%
#   filter(!(segment == "4/19/15-9" & latitude < 40.653))

# restrict to New York
nyctracks <- tracks %>%
  filter((date == as.IDate("2015-02-10") & longitude < -70) |
    (date > as.IDate("2015-02-10") & date < as.IDate("2015-06-17")) |
    (date == as.IDate("2015-06-17") & longitude < -70 & activity != "airplane"))

# places

places <- get_places(d)

nycplaces <- places %>%
  filter((startdate == as.IDate("2015-02-10") & longitude < -70) |
    (startdate > as.IDate("2015-02-10") & enddate < as.IDate("2015-06-17")) |
    (enddate == as.IDate("2015-06-17") & longitude < -70))

# contour

nyccontours <- as.matrix(nyctracks[, .(longitude, latitude)])
nyccontours <- bkde2D(nyccontours,
  bandwidth=c(bw.ucv(nyccontours[,1]), bw.ucv(nyccontours[,2])))
nyccontours <- contourLines(nyccontours$x1, nyccontours$x2, nyccontours$fhat)

# build map

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

map <- leaflet() %>%
  addTiles() %>%
  addLayersControl(overlayGroups=c("walking", "cycling", "transport",
    "places", "heatmap"), options=layersControlOptions(collapsed=FALSE))
for (i in seq_along(nyccontours)) {
  map <- map %>%
    addPolygons(nyccontours[[i]]$x, nyccontours[[i]]$y,
      group="heatmap", color="black", weight=1, fillColor="red")
}
for (segment in unique(nyctracks$segment)) {
  map <- map %>%
    addPolylines(data=nyctracks[segment], ~longitude, ~latitude,
      group=~as.character(groups[activity[1]]),
      col=~as.character(cols[activity[1]]), weight=4, popup=~activity)
}
map <- map %>% addCircles(data=nycplaces, ~longitude, ~latitude,
  group="places", col="yellow", popup=~name)
map <- map %>%
  hideGroup(c("places", "heatmap"))

# save
if (!file.exists("nyc"))
  dir.create("nyc", mode="755")
saveRDS(map, file.path("nyc", "nyc.rds"))

# shiny::runApp("nyc")

# shinyapps::deployApp("nyc", appName="moves-nyc")

# EOF
