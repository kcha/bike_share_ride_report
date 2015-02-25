library(weatherData)

get_weather_data <- function(years) {
  W <- lapply(years, function(y) getWeatherForYear("YYZ", y)) %>% 
    do.call(what = "rbind")
  dates <- as.POSIXct(strptime(W$Date, "%Y-%m-%d"))
  W$mo <- strftime(dates, "%m")
  W$dy <- strftime(dates, "%d")
  W$yr <- strftime(dates, "%Y")
  W$mo.abbr <- strftime(dates, "%b")
  W$mo.abbr <- with(W, factor(mo.abbr, levels = unique(mo.abbr[order(mo)]),
                              ordered=T))
  return(W)
}

plot_weather_by_month <- function(W) {

  

  select(W, -Date) %>%
    group_by(mo.abbr, yr) %>%
    summarize(temp = mean(Mean_TemperatureC)) %>%
    ggplot(aes(x = mo.abbr, y = temp, group = yr, color = yr)) +
    geom_point() + geom_line() +
    ylab("Average Temperature (C)") +
    xlab("Month") +
    ggtitle("Historical Weather")
}