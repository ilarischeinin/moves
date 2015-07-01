#!/usr/bin/Rscript

suppressMessages({
  library(data.table)
  library(dplyr)
})

d <- commandArgs(TRUE)[1]
if (is.na(d))
  d <- "."

# read data
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

# remove activies with a distance of 0 (includes manually added + others)
activities <- activities %>% filter(distance > 0)

activities[activities$date == "5/24/15" & activities$activity == "transport",
  "activity"] <- "helicopter"

# check missing modes of transport?
if (sum(activities$activity == "transport") > 0) {
  print(activities %>% filter(activity == "transport"))
  stop("Missing modes of transport.")
}
  
# restict to full months
activities <- activities %>%
  filter((year(startdate) > year(min(startdate)) |
    month(startdate) > month(min(startdate))),
    (year(startdate) < year(max(startdate)) |
    month(startdate) < month(max(startdate))))

# exclude flying
activities <- activities %>%
  filter(activity != "airplane", activity != "helicopter")

# count running as walking
activities[activities$activity == "running", "activity"] <- "walking"

# calculate shares
shares <- activities %>%
  group_by(year=year(startdate), month=month(startdate), activity) %>%
  summarise(time=sum(duration) / 60 / 60, distance=sum(distance))

shares$month <- as.IDate(paste0(shares$year, '-', shares$month, '-01'))
shares$year <- NULL

# fill in missing values
shares <- merge(shares, expand.grid(month=unique(shares$month),
  activity=unique(shares$activity), stringsAsFactors=FALSE), all.y=TRUE)
shares[is.na(shares)] <- 0

shares$activity <- factor(shares$activity, levels=c("boat", "ferry", "car",
  "bus", "train", "underground", "tram", "cycling", "walking"), ordered=TRUE)

if (!file.exists("shares"))
  dir.create("shares", mode="755")
saveRDS(shares, file.path("shares", "shares.rds"))

# run shiny app
# library(shiny)
# runApp("shares")

# deploy shiny app
# suppressMessages(library(shinyapps))
# deployApp("shares", "moves-share")

# EOF
