#!/usr/bin/env Rscript

library(ggmap)
library(plyr)
library(stringr)
library(rjson)
library(assertthat)
source("R/mapper_funcs.R")

# Load data ####
input_file <- "data/ride_data.txt"
data <- read.table(input_file, header = TRUE, sep = "\t", stringsAsFactors=FALSE)
data$Duration <- duration_to_minutes(data$Duration)
data$Start.Station <- cleanse_station_names(data$Start.Station)
data$End.Station <- cleanse_station_names(data$End.Station)

# Load bike share stations
url <- "http://www.bikesharetoronto.com/stations/json"
stations <- fromJSON(file=url, method='C')
stations <- data.frame(do.call("rbind", stations$stationBeanList), 
                       stringsAsFactors=FALSE)
for (j in 1:ncol(stations)) {
  if (is.list(stations[,j])) {
    z <- unlist(stations[,j])
    if (is.null(z)) {
      stations[,j] <- rep("NULL", nrow(stations))
    } else {
      stations[,j] <- z
    }
  }
} 
stations$stationName <- cleanse_station_names(stations$stationName)

# Get longitude and latitude coordinates
starts <- join(data.frame(stationName=data$Start.Station, stringsAsFactors=FALSE), 
               stations[, c("stationName", "latitude", "longitude")])
ends <- join(data.frame(stationName=data$End.Station, stringsAsFactors=FALSE),
             stations[, c("stationName", "latitude", "longitude")])
assert_that(nrow(starts) == nrow(ends))

# Get frequencies ####
freq <- rbind(data.frame(ddply(starts, .(stationName, longitude, latitude), 
                               summarize, N = length(stationName)),
                         type = "Start"),
              data.frame(ddply(ends, .(stationName, longitude, latitude), 
                               summarize, N = length(stationName)),
                         type = "End"))
  
# Load map ####
city <- get_map("toronto", zoom=14, maptype = "roadmap")

# Plot map ####
gp1 <- ggmap(city, extent = 'device') + 
  geom_point(aes(x = longitude, y = latitude, colour = N, size = N), data = freq) + 
  scale_size_continuous("", range = c(4,12)) +
  scale_colour_gradient("Number of Visits") + 
  facet_wrap(~ type, ncol = 2) + 
  ggtitle(input_file) 

print(gp1)
