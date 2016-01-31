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

source("library.R")
tracks <- get_tracks(d)

boating <- tracks %>%
  filter(activity == "boat") %>%
  select(segment, date, latitude, longitude)

if (!file.exists("boating"))
  dir.create("boating", mode="755")
saveRDS(boating, file=file.path("boating", "boating.rds"), compress=FALSE)
  
# shiny::runApp("boating")

# EOF
