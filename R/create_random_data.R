#!/usr/bin/env Rscript
#
# Generate random trip data

source("R/mapper_funcs.R")

set.seed(123)

N <- 300

stations <- get_bike_share_data()

start.id <- sample(1:nrow(stations), N, replace = TRUE)
end.id <- sample(1:nrow(stations), N, replace = TRUE)

start.dates <- generate_random_dates(N)
end.dates <- start.dates + rnorm(N, 900, 300) # avg 15 min +/- 5 min
  
rdata <- data.frame(
  Trip = 1:N,
  Start.Station = stations[start.id, "stationName"],
  Start.Date = format(start.dates, "%m/%d/%Y %I:%M %p"),
  End.Station = stations[end.id, "stationName"],
  End.Date = format(end.dates, "%m/%d/%Y %I:%M %p"),
  Duration = format(.POSIXct(difftime(end.dates, start.dates, unit = "secs"), 
                             tz="GMT"), "%I:%M:%S")
)

write.table(rdata, file="", quote=F, row.names=F, sep="\t")
