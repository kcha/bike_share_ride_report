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
data$Duration <- duration_to_minutes(data$Duration)

# Organize data ####
attach(data)
stations <- data.frame(stations = c(as.character(Start.Station), 
                                    as.character(End.Station)))
geo <- apply(unique(stations), 1, geocode)
geo <- data.frame(stations = unique(stations$stations), do.call("rbind", geo))
geo$stations <- as.character(geo$stations)
starts <- join(data.frame(stations=Start.Station), geo)
ends <- join(data.frame(stations=End.Station), geo)
detach(data)

# Get frequencies ####
freq <- rbind(data.frame(ddply(starts, .(stations, lon, lat), summarize, N = length(stations)),
                         type = "Start"),
              data.frame(ddply(ends, .(stations, lon, lat), summarize, N = length(stations)),
                         type = "End"))
  
# Load map ####
city <- get_map("toronto", zoom=14, maptype = "roadmap")

# Plot map ####
gp1 <- ggmap(city, extent = 'device') + 
  geom_point(aes(x = lon, y = lat, colour = N, size = N), data = freq) + 
  scale_size_continuous("", range = c(4,12)) +
  scale_colour_gradient("Number of Visits") + 
  facet_wrap(~ type, ncol = 2) + 
  ggtitle(input_file) 

print(gp1)
