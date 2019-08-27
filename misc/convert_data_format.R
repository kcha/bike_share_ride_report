# Convert old Bike Share format to new format

library(tidyverse)

old_data <- read_tsv("data/ride_data.old_system.txt")

# Change column names
new_data <- old_data %>% 
  rename(`Trip id` = "Trip",
         `Start Time` = "Start Date",
         `End Time` = "End Date")


# Format dates:
#    Change MM/DD/YYYY to YYYY-MM-DD
#    Change HH:MM AM/PM to 24 hour format
to_24h <- function(date) {
  new_date <- strptime(date, "%m/%d/%Y %I:%M %p")
  new_date <- format(new_date, "%Y-%m-%d %H:%M")
  return(new_date)
}

# Convert duration to Xm YYs
convert_duration <- function(duration) {
  items <- str_split(duration, ":")
  new_duration <- sapply(items, function(x) {
    sprintf("%sm %ss", x[2], x[3])
  })
  return(new_duration)
}


new_data <- new_data %>% 
  mutate(
         `Start Time` = to_24h(`Start Time`),
         `End Time` = to_24h(`End Time`),
         Duration = convert_duration(Duration)
  )



# Concat existing data from new system
second_data <- read_tsv("data/ride_data.new_system.txt")
new_data <- rbind(new_data, second_data)

write.table(new_data, file="", quote=F, row.names=F, sep="\t")