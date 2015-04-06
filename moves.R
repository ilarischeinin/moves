library(maptools)
gpclibPermit()

if (!file.exists("gpx/full/activities.gpx")) {
  if (file.exists("gpx.zip")) {
    unzip("gpx.zip")
  }
}

moves <- readGPS(i="gpx", f="gpx/full/activities.gpx", type="w")

# fix errors
errors <- moves$V23 == 20150228 & moves$V3 %in% c(40.698363, 40.702833)
moves <- moves[!errors,]
rm(list=c("errors"))
invisible(gc())

nyc <- moves[moves$V23 >= 20150210 & moves$V4 < -70,]



cycling <- readGPS(i="gpx", f="gpx/full/cycling.gpx")







library(ggmap)


# nyc <- get_map(location=1.001 * c(min(stations$longitude),
#   min(stations$latitude), max(stations$longitude), max(stations$latitude)))

# map <- get_map(c(-74.05, 40.55, -73.93, 40.90))
# map <- get_map(c(-75, 40, -73, 41))
map <- get_map("New York", zoom=12)
# map <- get_map(c(min(nyc$V4), min(nyc$V3), max(nyc$V4), max(nyc$V3)))

p <-
ggmap(map, extent="device") +
  geom_path(data=nyc, aes(V4, V3)) +
  scale_x_continuous(limits=range(nyc$V4)) +
  scale_y_continuous(limits=range(nyc$V3)) +
  coord_map(xlim=c(-74.03, -73.94), ylim=c(40.70, 40.79))
  # coord_map(xlim=ggplot_build(p)$panel$ranges[[1]]$x.range,
  #   ylim= ggplot_build(p)$panel$ranges[[1]]$y.range)
  # + geom_point(data=nyc, aes(V4, V3), color="red")









library(maptools)
gpclibPermit()

moves <- readGPS(i="gpx", f="gpx/monthly/activities/activities_2015-02.gpx", type="w")
errors <- moves$V23 == 20150228 & moves$V3 %in% c(40.698363, 40.702833)
moves <- moves[!errors,]
rm(list=c("errors"))
invisible(gc())

walking <- readGPS(i="gpx", f="gpx/monthly/walking/walking_2015-02.gpx", type="w")
errors <- walking$V23 == 20150228 & walking$V3 %in% c(40.698363, 40.702833)
walking <- walking[!errors,]
rm(list=c("errors"))
invisible(gc())

cycling <- readGPS(i="gpx", f="gpx/monthly/cycling/cycling_2015-02.gpx", type="w")

mov <- paste(moves$V3, moves$V4, moves$V15, moves$V23, sep='-')
wal <- paste(walking$V3, walking$V4, walking$V15, walking$V23, sep='-')
cyc <- paste(cycling$V3, cycling$V4, cycling$V15, cycling$V23, sep='-')

moves$type[mov %in% wal] <- moves$type[mov %in% wal] + 1
moves$type[mov %in% cyc] <- moves$type[mov %in% cyc] + 2

# EOF
