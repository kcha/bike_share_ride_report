#!/usr/bin/env Rscript
#
# Generate random trip data
library(tidyverse)
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

# randomly choose a common route
ix <- sample(length(start_id), 80)
start_id[ix] <- sample(freq_start_stations, length(ix), replace=TRUE)
end_id[ix] <- sample(freq_end_stations, length(ix), replace=TRUE)

start_dates <- generate_random_dates(N)
end_dates <- start_dates + rnorm(N, 900, 300) # avg 15 min +/- 5 min
  
rdata <- tibble(
  `Trip id` = 1:N,
  `Start Station` = stations[start_id, "stationName"],
  `Start Time` = format(start_dates, "%Y-%m-%d %H:%M"),
  `End Station` = stations[end_id, "stationName"],
  `End Time` = format(end_dates,  "%Y-%m-%d %H:%M"),
  Duration = format(.POSIXct(difftime(end_dates, start_dates, unit = "secs"), 
                             tz="GMT"), "%Mm %Ss")
)
rdata <- rdata[order(rdata$`Trip id`, decreasing = TRUE),]

write.table(rdata, file="", quote=F, row.names=F, sep="\t")
