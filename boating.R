#!/usr/bin/Rscript

suppressMessages({
  library(data.table)
  library(dplyr)
  library(pbapply)
  library(XML)
})

d <- commandArgs(TRUE)[1]
if (is.na(d))
  d <- "."

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
  name <- as.Date(daylist[[1]], format="%m/%d/%y")
  daylist[[1]] <- NULL
  activities <- rbindlist(lapply(daylist, extractsegments))
  activities$segment <- paste0(name, "-",
    sprintf("%03i", rep(1:length(daylist), sapply(daylist, length)/3)))
  setcolorder(activities, c("segment", "time", "activity", "latitude",
    "longitude"))
  activities
}

message("Processing activities data...")
activities <- tbl_dt(rbindlist(pblapply(activitylist, extractdays)))
activities$date <- as.IDate(activities$time, format="%Y-%m-%dT%H:%M:%S")
setkey(activities, segment)

boating <- activities %>% filter(activity == "boat")

if (!file.exists("boating"))
  dir.create("boating", mode="755")
saveRDS(boating, file=file.path("boating", "boating.rds"))
  
# library(shiny)
# runApp("boating")

# suppressMessages(library(shinyapps))
# deployApp("boating", appName="moves-boating")

# EOF
