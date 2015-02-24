library(stringr)
library(rjson)
library(assertthat)
library(ggmap)
library(dplyr)

# Data pre-processing functions ####
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

# Data I/O functions ####
get_ride_data <- function(ride_data) {
  write(paste("Reading ride data from", ride_data), stderr())
  data <- read.table(ride_data, header = TRUE, sep = "\t", 
                     stringsAsFactors=FALSE)
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

# Data processing ####
calculate_station_frequencies <- function(data, stations, start = NULL, 
                                          end = NULL) {
  # Calculate station frequencies given ride data.
  # Can optionally filter by start and end date (format: YYYY-mm-dd)
  #
  # Return data frame of start and end stations, their frequencies, and 
  # coordinates
  df <- data
  if (!is.null(start)) {
    dates <- convert_date_time(df$Start.Date)
    df <- df[which(dates >= start),]
  }
  if (!is.null(end)) {
    dates <- convert_date_time(df$End.Date)
    df <- df[which(dates <= end),]
  }
  rbind(data.frame(stationName=df$Start.Station, type = "Start", 
                   stringsAsFactors=FALSE), 
        data.frame(stationName=df$End.Station, type = "End", 
                   stringsAsFactors=FALSE)) %>%
    group_by(stationName, type) %>%
    summarize(N = length(stationName)) %>%
    inner_join(., stations[, c("stationName", "latitude", "longitude")]) %>%
    as.data.frame
}

format_by_datetime <- function(data, start = NULL, end = NULL) {
  # Create a new data frame with different time formats
  dates <- convert_date_time(data$Start.Date)
  ddf <- data.frame(
    mo = strftime(dates, "%m"),
    dy = strftime(dates, "%d"),
    yr = strftime(dates, "%Y"),
    mo.abbr = strftime(dates, "%b"),
    duration = data$Duration,
    hr = strftime(dates, "%H"),
    min = strftime(dates, "%M"),  
    route = paste(data$Start.Station, "to", data$End.Station)
  )
  assert_that(nrow(ddf) == nrow(data))
  if (!is.null(start)) {
    ix <- which(dates >= start)
  }
  if (!is.null(end)) {
    ix <- intersect(ix, which(dates <= end))
  }
  if (exists("ix")) {
    ddf <- ddf[ix,]
  }
  ddf$mo <- factor(ddf$mo, levels=sort(unique(ddf$mo)), ordered=T)
  ddf$dy <- factor(ddf$dy, levels=sort(unique(ddf$dy)), ordered=T)
  ddf$yr <- factor(ddf$yr, levels=sort(unique(ddf$yr)), ordered=T)
  ddf$mo.abbr <- with(ddf, factor(mo.abbr, levels = unique(mo.abbr[order(mo)]),
                                  ordered=T))
  ddf$hr <- factor(ddf$hr, levels=sort(unique(ddf$hr)), ordered=T)
  ddf$min<- factor(ddf$min, levels=sort(unique(ddf$min)), ordered=T)
  return(ddf)
}

convert_date_time <- function(dates) {
  # Helper function to convert date time to POSIX class
  as.POSIXct(strptime(dates, "%m/%d/%Y %I:%M %p"))
}

# Misc functions ####
generate_random_dates <- function(ndates, start = "2013/01/01 12:00 AM", 
                                  end = "2014/12/25 12:00 PM") {
  # Generate random dates and times given a start and end date
  first <- as.POSIXct(strptime(start, "%Y/%m/%d %I:%M %p"))
  last <- as.POSIXct(strptime(end, "%Y/%m/%d %I:%M %p"))
  td <- last - first
  
  # Weight times in morning and afternoon to simulate a more realistic times
  i <- round(ndates / 2)
  random_time <- c(round(rnorm(i, 32400, 3600)),
                   round(rnorm(ndates - i, 61200, 3600)))
  assert_that(length(random_time) == ndates)
  
  # first day + random day + random time of day
  rdates <- first + sample(as.numeric(td), ndates, replace=TRUE) * 86400 +
    random_time
  return(sort(rdates))
}

# Plotting functions ####
calculate_bbox <- function(lon, lat, offset = 0.1) {
  # Calcualte bounding box for get_map() given a set of longitudes and 
  # corresponding latitudes
  #
  # From ggmap documentation:
  # bbox - a bounding box in the format c(lowerleftlon, lowerleftlat, 
  #           upperrightlon, upperrightlat)
  c(min(lon) - offset, min(lat) - offset, max(lon) + offset, max(lat) + offset)  
}

map_stations <- function(df) {
  city <- get_map(calculate_bbox(df$longitude, df$latitude), zoom=14, 
                  maptype = "roadmap", scale = 2)
  
  gp1 <- ggmap(city, extent = 'device') + 
    geom_point(aes(x = longitude, y = latitude, colour = N, size = N), 
               data = df) + 
    scale_size_continuous("", range = c(3,9)) +
    scale_colour_gradient("Number of Visits", low="orange", high="red")
  return(gp1)
}