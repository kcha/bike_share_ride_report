#!/usr/bin/env Rscript
#
# Generate random trip data

source("R/mapper_funcs.R")

set.seed(123)

N <- 300

stations <- get_bike_share_data()

write("Generating simulated ride data", stderr())

# randomly select a few stations to be more frequently visited
freq_start_stations <- sample(1:nrow(stations), 3)
freq_end_stations <- sample(setdiff(1:nrow(stations), freq_start_stations), 3)

start_prob <- runif(nrow(stations), 0, 0.1)
start_prob[freq_start_stations] <- runif(length(freq_start_stations), 0.8, 1)
end_prob <- runif(nrow(stations), 0, 0.1)
end_prob[freq_end_stations] <- runif(length(freq_end_stations), 0.8, 1)

start_id <- sample(1:nrow(stations), N, replace = TRUE, prob = start_prob)
end_id <- sample(1:nrow(stations), N, replace = TRUE, prob = end_prob)

start_dates <- generate_random_dates(N)
end_dates <- start_dates + rnorm(N, 900, 300) # avg 15 min +/- 5 min
  
rdata <- data.frame(
  Trip = 1:N,
  Start.Station = stations[start_id, "stationName"],
  Start.Date = format(start_dates, "%m/%d/%Y %I:%M %p"),
  End.Station = stations[end_id, "stationName"],
  End.Date = format(end_dates, "%m/%d/%Y %I:%M %p"),
  Duration = format(.POSIXct(difftime(end_dates, start_dates, unit = "secs"), 
                             tz="GMT"), "%H:%M:%S")
)
rdata <- rdata[order(rdata$Trip, decreasing = TRUE),]

write.table(rdata, file="", quote=F, row.names=F, sep="\t")
