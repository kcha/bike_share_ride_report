library(stringr)
library(rjson)
library(assertthat)

cleanse_station_names <- function(stations) {
  # Correct station names that have been moved
  stations <- trim(stations)
  stations <- remove_periods(stations)
  stations <- str_replace_all(stations, "Wellesley St W / Queens Park Cres W",
                              "Queens Park Cres E")
  stations <- str_replace_all(stations, "Dundas / Yonge St",
                              "Dundas St / Yonge St")
  return(stations)
}

remove_periods <- function(x) {
  # Remove periods at the end of street names
  gsub("St\\.", "St", x)
}

trim <- function(x) {
  # Remove leading and trailing whitespace
  gsub("^\\s+|\\s+$", "", x)
}

duration_to_minutes <- function(dur) {
  # Convert 0:00:00 time format to minutes
  t <- str_split(dur, ":")
  t <- lapply(t, as.numeric)
  t <- do.call("rbind", t)
  t <- as.matrix(t)
  minutes <- 60*t[,1] + t[,2] + t[,3]/60
  return(round(minutes, digits=2))
}

get_ride_data <- function(ride_data) {
  write(paste("Reading ride data from", ride_data), stderr())
  data <- read.table(input_file, header = TRUE, sep = "\t", stringsAsFactors=FALSE)
  data$Duration <- duration_to_minutes(data$Duration)
  data$Start.Station <- cleanse_station_names(data$Start.Station)
  data$End.Station <- cleanse_station_names(data$End.Station)
  return(data)
}

get_bike_share_data <- function(
  json_url = "http://www.bikesharetoronto.com/stations/json") {
  # Download Bike Share Toronto station data
  write(paste("Downloading bike share stations from", json_url), stderr())
  stations <- fromJSON(file=json_url, method='C')
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
  return(stations)
}

generate_random_dates <- function(ndates, start = "2013/01/01 00:00 AM", end = "2014/12/31 12:00 PM") {
  first <- as.POSIXct(strptime(start, "%Y/%m/%d %H:%M %p"))
  last <- as.POSIXct(strptime(end, "%Y/%m/%d %H:%M %p"))
  td <- last - first
  
  # first day + random day + random time of day
  rdates <- first + sample(as.numeric(dt), ndates) * 86400 + 
    runif(ndates, 1, 86400) 
  rdates <- sort(rdates)
  return(rdates)  
}
