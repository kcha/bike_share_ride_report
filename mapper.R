#!/usr/bin/env Rscript

library(ggmap)
library(plyr)
source("R/mapper_funcs.R")

# Load bike share stations ####
stations <- get_bike_share_data()

# Load data ####
input_file <- "data/ride_data.txt"
data <- get_ride_data(input_file)

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
  scale_colour_gradient("Number of Visits", low="orange", high="red") + 
  facet_wrap(~ type, ncol = 2) + 
  ggtitle(input_file) 

print(gp1)
