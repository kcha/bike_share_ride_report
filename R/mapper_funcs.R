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