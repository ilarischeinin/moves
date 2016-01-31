suppressMessages({
  library(data.table)
  library(dplyr)
  library(pbapply)
  library(XML)
})

get_tracks <- function(d) {
  tracksfile <- file.path(d, "gpx", "full", "activities.gpx")
  if (!file.exists(tracksfile)) {
    zipfile <- file.path(d, "gpx.zip")
    if (file.exists(zipfile))
      unzip(zipfile, exdir=d)
    rm(list="zipfile")
  }

  if (!file.exists(tracksfile))
    stop("File not found: ", tracksfile)

  activitylist <- xmlToList(xmlTreeParse(tracksfile))
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
      daytracks <- as.data.table(t(as.data.frame(
        segmentlist[seq(from=3, to=length(segmentlist), by=3)])))
      setnames(daytracks, c("longitude", "latitude"))
      daytracks$longitude <- as.numeric(daytracks$longitude)
      daytracks$latitude <- as.numeric(daytracks$latitude)
      daytracks$time <-
        unlist(segmentlist[seq(from=1, to=length(segmentlist), by=3)])
      daytracks$activity <-
        unlist(segmentlist[seq(from=2, to=length(segmentlist), by=3)])
      daytracks
    }
    name <- as.Date(daylist[[1]], format="%m/%d/%y")
    daylist[[1]] <- NULL
    tracks <- rbindlist(lapply(daylist, extractsegments))
    tracks$segment <- paste0(name, "-",
      sprintf("%03i", rep(1:length(daylist), sapply(daylist, length)/3)))
    setcolorder(tracks, c("segment", "time", "activity", "latitude",
      "longitude"))
    tracks
  }

  message("Processing tracks data...")
  tracks <- tbl_dt(rbindlist(pblapply(activitylist, extractdays)))
  tracks$date <- as.IDate(tracks$time, format="%Y-%m-%dT%H:%M:%S")
  setkey(tracks, segment)
  tracks
}

get_activities <- function(d) {
  activitiesfile <- file.path(d, "csv", "full", "activities.csv")
  if (!file.exists(activitiesfile)) {
    zipfile <- file.path(d, "csv.zip")
    if (file.exists(zipfile))
      unzip(zipfile, exdir=d)
    rm(list="zipfile")
  }

  if (!file.exists(activitiesfile))
    stop("File not found: ", activitiesfile)

  activities <- tbl_df(fread(activitiesfile))
  setnames(activities, tolower(colnames(activities)))
  activities$startdate <- as.IDate(activities$start, format="%Y-%m-%dT%H:%M:%S")
  activities$enddate <- as.IDate(activities$end, format="%Y-%m-%dT%H:%M:%S")
  activities
}

get_places <- function(d) {
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
  places
}

# EOF
