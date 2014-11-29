#!/usr/bin/env Rscript

source("R/mapper_funcs.R")

# Load bike share stations ####
stations <- get_bike_share_data()

# Load data ####
input_file <- "data/ride_data.txt"
data <- get_ride_data(input_file)

# Get frequencies ####
freq <- calculate_station_frequencies(data, stations)
  
# Plot map ####
gp1 <- map_stations(freq) + 
  facet_wrap(~ type, ncol = 2) +
  ggtitle(input_file)
print(gp1)
