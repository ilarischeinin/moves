#!/usr/bin/Rscript

suppressMessages({
  library(data.table)
  library(dplyr)
  library(XML)
  library(pbapply)
  library(KernSmooth)
  library(leaflet)
})

d <- commandArgs(TRUE)[1]
if (is.na(d))
  d <- "."

# activities

activitiesfile <- file.path(d, "gpx", "full", "activities.gpx")
if (!file.exists(activitiesfile)) {
  zipfile <- file.path(d, "gpx.zip")
  if (file.exists(zipfile))
    unzip(zipfile, exdir=d)
  rm(list="zipfile")
}

if (!file.exists(activitiesfile))
  stop("File not found: ", activitiesfile)

activitylist <- xmlToList(xmlTreeParse(activitiesfile))
# remove first and last element as they contain only metadata
activitylist[[1]] <- NULL
activitylist[[length(activitylist)]] <- NULL

# we now have a list of lists
# each element in activitylist is a list of segments for one day
# define function extractdays() to each day
extractdays <- function(daylist) {
  # define function extractsegments() to process segments in each ady
  extractsegments <- function(segmentlist) {
    # segmentlist is a list with 3 elements for each segment
    # 1: time stamp
    # 2: activity type
    # 3: coordinates
    dayactivities <- as.data.table(t(as.data.frame(
      segmentlist[seq(from=3, to=length(segmentlist), by=3)])))
    setnames(dayactivities, c("longitude", "latitude"))
    dayactivities$longitude <- as.numeric(dayactivities$longitude)
    dayactivities$latitude <- as.numeric(dayactivities$latitude)
    dayactivities$time <-
      unlist(segmentlist[seq(from=1, to=length(segmentlist), by=3)])
    dayactivities$activity <-
      unlist(segmentlist[seq(from=2, to=length(segmentlist), by=3)])
    dayactivities
  }
  name <- daylist[[1]]
  daylist[[1]] <- NULL
  activities <- rbindlist(lapply(daylist, extractsegments))
  activities$segment <- paste0(name, "-",
    rep(1:length(daylist), sapply(daylist, length)/3))
  setcolorder(activities, c("segment", "time", "activity", "latitude",
    "longitude"))
  activities
}

message("Processing activities data...")
activities <- tbl_dt(rbindlist(pblapply(activitylist, extractdays)))
activities$date <- as.IDate(activities$time, format="%Y-%m-%dT%H:%M:%S")
setkey(activities, segment)

# count running as walking
activities[activities$activity == "running", "activity"] <- "walking"

activities[activities$date == "2015-05-24" & activities$activity == "transport",
  "activity"] <- "helicopter"

# The app seems to have a tendency to locate me on roads, which seems to be a
# problem especially along the shores of the East River, where it frequently
# placed me on the wrong side of the river, hopping back and forth. In the
# beginning I was fixing these manually, but then got tired of it.
# fix errors in data
# activities <- activities %>%
#   filter(!(date == as.IDate("2015-02-28") & latitude < 40.705)) %>%
#   filter(!(date == as.IDate("2015-04-24") & longitude > -73.97)) %>%
#   filter(!(date == as.IDate("2015-04-24") & latitude < 40.7046)) %>%
#   filter(!(segment == "4/19/15-9" & latitude < 40.653))

# restrict to New York
nycactivities <- activities %>%
  filter((date == as.IDate("2015-02-10") & longitude < -70) |
    (date > as.IDate("2015-02-10") & date < as.IDate("2015-06-17")) |
    (date == as.IDate("2015-06-17") & longitude < -70 & activity != "airplane"))

# places

placesfile <- file.path(d, "csv", "full", "places.csv")
if (!file.exists(placesfile)) {
  zipfile <- file.path(d, "csv.zip")
  if (file.exists(zipfile))
    unzip(zipfile, exdir=d)
  rm(list="zipfile")
}

if (!file.exists(placesfile))
  stop("File not found: ", placesfile)

places <- tbl_dt(fread(placesfile))
setnames(places, tolower(colnames(places)))
places$startdate <- as.IDate(places$start, format="%Y-%m-%dT%H:%M:%S")
places$starttime <- as.ITime(places$start, format="%Y-%m-%dT%H:%M:%S")
places$enddate <- as.IDate(places$end, format="%Y-%m-%dT%H:%M:%S")
places$endtime <- as.ITime(places$end, format="%Y-%m-%dT%H:%M:%S")

nycplaces <- places %>%
  filter((startdate == as.IDate("2015-02-10") & longitude < -70) |
    (startdate > as.IDate("2015-02-10") & enddate < as.IDate("2015-06-17")) |
    (enddate == as.IDate("2015-06-17") & longitude < -70))

# contour

nyccontours <- as.matrix(nycactivities[, .(longitude, latitude)])
nyccontours <- bkde2D(nyccontours,
  bandwidth=c(bw.ucv(nyccontours[,1]), bw.ucv(nyccontours[,2])))
nyccontours <- contourLines(nyccontours$x1, nyccontours$x2, nyccontours$fhat)

# save

if (!file.exists("nyc"))
  dir.create("nyc", mode="755")
save(nycactivities, nycplaces, nyccontours, file=file.path("nyc", "nyc.rda"))
  
# library(shiny)
# runApp("nyc")

# suppressMessages(library(shinyapps))
# deployApp("nyc", "moves-nyc")

# EOF
