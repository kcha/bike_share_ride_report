cleanse_station_names <- function(stations) {
  # Correct station names that have been known to give inaccurate locations when
  # searching for their geographical locations
  stations <- str_replace_all(stations, " / ", " and ")
  stations <- str_replace_all(stations, "Ferry Ramp and Queens Quay", 
                              "Harbour Square Park")  
  stations <- str_replace_all(stations, "Princess Ave and Adelaide St( E)*", 
                              "Princess Ave and Adelaide St E")
  return(stations)
}

duration_to_minutes <- function(dur) {
  # Convert 0:00:00 time format to minutes
  t <- str_split(dur, ":")
  t <- lapply(t, as.numeric)
  t <- do.call("rbind", t)
  t <- as.matrix(t)
  minutes <- 60*t[,1] + t[,2] + t[,3]/60
  return(minutes)
}