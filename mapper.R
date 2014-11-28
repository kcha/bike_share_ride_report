#!/usr/bin/env Rscript

library(grid)
library(ggmap)
library(plyr)
library(stringr)
source("R/mapper_funcs.R")

# Load data
input_file <- "data/ride_data.txt"
data <- read.table(input_file, header = TRUE, sep = "\t")
data$Start.Station <- cleanse_station_names(data$Start.Station)
data$End.Station <- cleanse_station_names(data$End.Station)

# Plot frequency of stations on map ####

# Organize data ####
stations <- data.frame(stations = c(as.character(data$Start.Station), 
                                    as.character(data$End.Station)))
stations$stations <- paste0(stations$stations, ", Toronto, ON")
geo <- apply(unique(stations), 1, geocode)
geo <- data.frame(stations = unique(stations$stations), do.call("rbind", geo))
geo$stations <- as.character(geo$stations)
stations <- join(stations, geo)
# stations.loc <- data.frame(stations, do.call("rbind", geo))

freq <- ddply(stations, .(stations, lon, lat), summarize, N = length(stations))
  
# Load map ####
city <- get_map("toronto", zoom=14, maptype = "roadmap")

# Plot map ####
ggmap(city, extent = 'device') + 
  geom_point(aes(x = lon, y = lat, colour = N, size = N), data = freq) +
  scale_size_continuous("", range = c(4,12)) +
  scale_colour_gradient("Number of Visits") + 
  ggtitle(input_file)
  guides(size = FALSE)