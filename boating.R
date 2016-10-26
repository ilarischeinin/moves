#!/usr/bin/Rscript

suppressMessages({
  library(data.table)
  library(dplyr)
  library(dtplyr)
  library(fmi)
  library(geosphere)
  library(lubridate)
  library(pbapply)
  library(R.cache)
  library(stringr)
  library(tidyr)
  library(XML)
})

d <- commandArgs(TRUE)[1]
if (is.na(d))
  d <- "."

apikey <- readLines(file.path("boating", "fmi-apikey.txt"))

source("library.R")
all_tracks <- get_tracks(d)

boating <- all_tracks %>%
  filter(activity == "boat") %>%
  # mutate(time=ymd_hms(time)) %>%
  mutate(time=as.POSIXct(time, format="%FT%T")) %>%
  select(segment, time, latitude, longitude) %>%
  filter(date(time) != as.Date("2016-08-14") |
    time <= as.POSIXct("2016-08-14T15:51:15.000+03:00", format="%FT%T"))

# fetch lists of weather stations, wave buoys, and mareographs
weather_stations <- fmi_stations(groups="Weather stations")
wave_stations <- fmi_stations(groups="Buoys")
mareo_stations <- fmi_stations(groups="Mareographs")

# calculate distances to weather stations, wave buyoys and mareographs,
# and pick the closest one within 30 NM
weather_distances <- distm(boating[, .(longitude, latitude)],
  weather_stations[, c("Lon", "Lat")]) / 1852
boating$weather_station <-
  weather_stations$FMISID[apply(weather_distances, 1L, which.min)]
boating$weather_distance <- apply(weather_distances, 1L, min)
boating$weather_station[boating$weather_distance > 30] <- NA

wave_distances <- distm(boating[, .(longitude, latitude)],
  wave_stations[, c("Lon", "Lat")]) / 1852
boating$wave_station <-
  wave_stations$FMISID[apply(wave_distances, 1L, which.min)]
boating$wave_distance <- apply(wave_distances, 1L, min)
boating$wave_station[boating$wave_distance > 30] <- NA

mareo_distances <- distm(boating[, .(longitude, latitude)],
  mareo_stations[, c("Lon", "Lat")]) / 1852
boating$mareo_station <-
  mareo_stations$FMISID[apply(mareo_distances, 1L, which.min)]
boating$mareo_distance <- apply(mareo_distances, 1L, min)
boating$mareo_station[boating$mareo_distance > 30] <- NA

# define function to access and cache the fmi api
cached_fmi <- function(query, fmisid, date) {
  fmisid <- as.integer(fmisid)
  date <- format(date, "%F")
  key <- list(query=query, fmisid=fmisid, date=date)
  suffix <- paste0("::", query, "::", fmisid, "::", date)

  cached <- loadCache(key=key, suffix=suffix, dirs="fmi")
  if (!is.null(cached)) {
    message("Loaded cached ", suffix, ".")
    return(cached)
  }
  
  request <- FMIWFSRequest$new(apiKey=apikey)
  request$setParameters(request="getFeature",
    storedquery_id=query,
    fmisid=fmisid,
    starttime=paste0(date, "T00:00:00"),
    endtime=paste0(date, "T23:59:59"))
  client <- FMIWFSClient$new(request=request)
  suppressMessages({
    layers <- client$listLayers()
  })
  if (length(layers) > 0L) {
    suppressMessages({
      response <- client$getLayer(layer=layers[1L],
        crs="+proj=longlat +datum=WGS84", swapAxisOrder=TRUE,
        parameters=list(splitListFields=TRUE))
    })
    data <- response@data %>%
      tbl_df() %>%
      transmute(
        fmisid=fmisid,
        time=ymd_hms(Time),
        variable=ParameterName,
        value=as.numeric(ParameterValue)
      )
  } else {
    data <- data_frame(
      fmisid=integer(),
      time=as.POSIXct(character()),
      variable=character(),
      value=numeric())
  }
  saveCache(data, key=key, suffix=suffix, dirs="fmi", compress=TRUE)
  message("Downloaded    ", suffix, ".")
  return(data)
}

# define dates for which to retrieve weather and wave data
weather_dates <- boating %>%
  filter(!is.na(weather_station)) %>%
  distinct(weather_station, date=date(time))

wave_dates <- boating %>%
  filter(!is.na(wave_station)) %>%
  distinct(wave_station, date=date(time))

mareo_dates <- boating %>%
  filter(!is.na(mareo_station)) %>%
  distinct(mareo_station, date=date(time))

# retrieve data
weather_data <- weather_dates %>%
  nrow() %>%
  seq_len() %>%
  lapply(function(row) {
    cached_fmi(
      query="fmi::observations::weather::simple",
      fmisid=weather_dates$weather_station[row],
      date=weather_dates$date[row])
  }) %>%
  bind_rows() %>%
  spread(variable, value) %>%
  data.table(key=c("fmisid", "time")) %>%
  tbl_dt()

wave_data <- wave_dates %>%
  nrow() %>%
  seq_len() %>%
  lapply(function(row) {
    cached_fmi(
      query="fmi::observations::wave::simple",
      fmisid=wave_dates$wave_station[row],
      date=wave_dates$date[row])
  }) %>%
  bind_rows() %>%
  spread(variable, value) %>%
  data.table(key=c("fmisid", "time")) %>%
  tbl_dt()

mareo_data <- mareo_dates %>%
  nrow() %>%
  seq_len() %>%
  lapply(function(row) {
    cached_fmi(
      query="fmi::observations::mareograph::simple",
      fmisid=mareo_dates$mareo_station[row],
      date=mareo_dates$date[row])
  }) %>%
  bind_rows() %>%
  spread(variable, value) %>%
  data.table(key=c("fmisid", "time")) %>%
  tbl_dt()

# join with rolling joins
setkey(boating, weather_station, time)
boating <- weather_data[boating, roll=TRUE] %>%
  rename(weather_station=fmisid)
setkey(boating, wave_station, time)
boating <- wave_data[boating, roll=TRUE] %>%
  rename(wave_station=fmisid)
setkey(boating, mareo_station, time)
boating <- mareo_data[boating, roll=TRUE] %>%
  rename(mareo_station=fmisid)
setkey(boating, time)
setkey(boating, segment)

# function to calculate segment length
segment_length <- function(latitude, longitude) {
  # create a matrix from the coordinates
  matrix(c(longitude, latitude), ncol=2L) %>%
    # calculate a distance matrix between points
    distm() %>%
    # remove first row (first column would do just as well)
    `[`(-1L, ) %>%
    # take diagonal, which now contains distances between points
    # 1..2, 2..3, 3..4, etc
    diag() %>%
    sum() %>%
    # divide by 1852 to get nautical miles instead of meters
    `/`(1852)
}

# function to format a range for degrees
# when a range is around north, we want to show it as e.g. 350-010 °,
# not the other way around
format_direction <- function(x) {
  if (length(x) > 1L) {
    if (any(x < 90) && any(x > 270)) {
      # get average direction
      # https://en.m.wikipedia.org/wiki/Mean_of_circular_quantities
      average_direction <- abs(atan2(
        sum(sin(x * pi / 180)),
        sum(cos(x * pi / 180))
      ) * 180 / pi)
      if (average_direction < 90 || average_direction > 270) {
        x <- range(x) %>% rev()
      } else {
        x <- range(x)
      }
    } else {
      x <- range(x)
    }
  }
  sprintf("%03.0f", x)
}

# when formatting sea levels, include the + sign for positive values
format_level <- function(x) {
  if (length(x) > 1L) {
    x <- range(x)
  }
  sprintf("%+.1f", x)
}

format_round <- function(x) {
  if (length(x) > 1L) {
    x <- range(x)
  }
  round(x, digits=1L)
}

format_range <- function(x, pre="", post="", fun=format_round) {
  x <- x[!is.na(x)]
  if (length(x) == 0L) {
    return("")
  }
  x <- unique(x)
  y <- do.call(fun, list(x)) %>%
    paste(collapse=" – ")
    # paste(collapse=" … ")
  return(paste0(pre, y, post))
}

segments <- boating %>%
  group_by(segment) %>%
  summarise(
    year=year(first(time)),
    length=segment_length(latitude, longitude),
    hours=difftime(max(time), min(time), units="hours") %>% as.numeric,
    date=date(time) %>%
      format_range("Date: "),
    wind_speed=(ws_10min * 3600 / 1852) %>%
      format_range("<br />Wind speed: ", " kn"),
    wind_gusts=(wg_10min * 3600 / 1852) %>%
      format_range("<br />Wind gusts: ", " kn"),
    wind_direction=wd_10min %>%
      format_range("<br />Wind from: ", " °", fun=format_direction),
    wave_height=WaveHs %>%
      format_range("<br />Significant wave height: ", " m"),
    wave_direction=ModalWDi %>%
      format_range("<br />Waves from: ", " °", fun=format_direction),
    water_level=(WATLEV / 10) %>%
      format_range("<br />Water level: ", " cm", fun=format_level),
    water_temp=TWATER %>%
      format_range("<br />Water temperature: ", " °C"),
    air_temp=t2m %>%
      format_range("<br />Air temperature: ", " °C"),
    pressure=p_sea %>%
      format_range("<br />Air pressure: ", " kPa")
  ) %>%
  mutate(
    distance=paste0("<br />Distance: ", round(length, digits=1L), " NM"),
    speed=paste0("<br />Average speed: ",
      round(length / hours, digits=1L), " kn")
  )

# create year labels and popup messages for tracks
years <- as.character(segments$year)
names(years) <- segments$segment
popups <- paste0(
  segments$date,
  segments$distance,
  segments$speed,
  segments$wind_speed,
  segments$wind_gusts,
  segments$wind_direction,
  segments$wave_height,
  segments$wave_direction,
  segments$water_level,
  segments$water_temp,
  segments$air_temp,
  segments$pressure)
names(popups) <- segments$segment
# for some reason leaflet cannot handle named character vectors,
# so convert to a list
# https://github.com/rstudio/leaflet/issues/299
years <- as.list(years)
popups <- as.list(popups)

tracks <- boating %>%
  select(segment, latitude, longitude)

stations <- fmi_stations() %>%
  # filter(
  #     FMISID %in% unique(weather_dates$weather_station) |
  #     FMISID %in% unique(wave_dates$wave_station) |
  #     FMISID %in% unique(mareo_dates$mareo_station)
  #   ) %>%
    transmute(
      latitude=Lat,
      longitude=Lon,
      popup=paste0(Name, "<br />", Groups)
    )

if (!file.exists("boating"))
  dir.create("boating", mode="755")
save(tracks, years, popups, stations,
  file=file.path("boating", "boating.rda"), compress=FALSE)

# shiny::runApp("boating")

# EOF
