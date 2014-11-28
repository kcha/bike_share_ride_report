#!/usr/bin/env Rscript

library(grid)
library(ggmap)
library(plyr)
library(stringr)

duration_to_minutes <- function(dur) {
  # Convert 0:00:00 time format to minutes
  t <- str_split(dur, ":")
  t <- lapply(t, as.numeric)
  t <- do.call("rbind", t)
  t <- as.matrix(t)
  minutes <- 60*t[,1] + t[,2] + t[,3]/60
  return(minutes)
}

# Load data
input_file <- "data/ride_data.txt"
data <- read.table(input_file, header = TRUE, sep = "\t")

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