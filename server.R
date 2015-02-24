library(shiny)
source("R/mapper_funcs.R")
source("R/weather_funcs.R")

shinyServer(function(input, output, session) {
  
  stations <- get_bike_share_data()
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Reactive expression for ride data
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Data <- reactive({
    if (is.null(input$file)) {
      data <- get_ride_data("sample_data/ride_data.txt")
    } else {
      data <- get_ride_data(input$file$datapath)  
    }  
    
    withProgress(message = "Processing ride data", value = 0.1, {
      freq <- calculate_station_frequencies(data, stations)
      
      incProgress(0.1)
      
      most_start <- which(freq$N == max(freq[freq$type=="Start", "N"]) & 
                            freq$type == "Start")
      most_end <- which(freq$N == max(freq[freq$type=="End", "N"]) & 
                          freq$type == "End")
      
      avg_ride <- round(mean(data$Duration), digits=2)
      shortest_trip<- which.min(data$Duration)
      longest_trip <- which.max(data$Duration)
      most_freq_trips <- data %>% 
        group_by(Start.Station, End.Station) %>% 
        summarize(N = length(Start.Station), 
                  Avg.Duration = round(mean(Duration), digits=2)) %>% 
        mutate(route = paste(Start.Station, "to", End.Station)) %>%
        as.data.frame %>%
        arrange(desc(N))
      
      incProgress(0.1)
      
      ddf <- format_by_datetime(data)
      
      trips_in_day <- ddf %>% group_by(dy, mo, yr) %>% 
        summarize(N = length(route))
      
      most_trips_in_day <- trips_in_day %>%
        subset(N == max(N)) %>%
        mutate(date = paste(mo, dy, yr, sep="/")) %>% 
        as.data.frame
      
      incProgress(0.1, detail = "Downloading weather data")
      
      # weather
      W <- get_weather_data(seq(
        min(as.numeric(as.character(ddf$yr))), 
        max(as.numeric(as.character(ddf$yr)))
      ))
      
      incProgress(0.4, detail = "Analyzing weather")
      
      ddfw <- plyr::join(ddf, W)
      ddfw$Date <- as.character(ddfw$Date)
      coldest <- filter(ddfw, Max_TemperatureC == min(Max_TemperatureC))
      coldest <- paste(
        unique(coldest$Max_TemperatureC), "C",
        paste0("(",
               paste(coldest$Date, collapse=", "),
               ")")
      )
      warmest <- filter(ddfw, Max_TemperatureC == max(Max_TemperatureC))
      warmest <- paste(
        unique(warmest$Max_TemperatureC), "C",
        paste0("(",
               paste(warmest$Date, collapse=", "),
               ")")
      )
      
      incProgress(0.1, "Putting it all together")
      
      info <- list(
        data=data, 
        stations=stations,
        freq=freq,
        most_start=freq[most_start, c("stationName", "N")],
        most_end=freq[most_end, c("stationName", "N")],
        avg_ride=avg_ride,
        shortest_trip=paste(
          data[shortest_trip, "Start.Station"], "to", 
          data[shortest_trip, "End.Station"], ",", 
          data[shortest_trip, "Duration"], "minutes"),
        longest_trip=paste(
          data[longest_trip, "Start.Station"], "to", 
          data[longest_trip, "End.Station"], ",", 
          data[longest_trip, "Duration"], "minutes"),
        most_freq_trips=most_freq_trips,
        trips_in_day=trips_in_day,
        most_trips_in_day=paste(
          unique(most_trips_in_day$N), 
          paste0("(", 
                 paste(most_trips_in_day$date, collapse=", "), 
                 ")")
        ),
        coldest=coldest,
        warmest=warmest,
        ddfw=ddfw,
        ddf=ddf)
      setProgress(1)
    })
    return(info)
  })  
  
  Freq <- reactive({
    calculate_station_frequencies(Data()$data, stations, 
                                  format(input$date_range_map[1]),
                                  format(input$date_range_map[2]))
  })
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Update maximum ride frequency for maps
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  observe({
    updateNumericInput(session, "max_map_ride_freq", value = max(Data()$freq$N))  
    
    # Get earliest and latest dates
    latest <- with(Data()$ddf, max(paste(yr, mo, dy, sep = "-")))
    earliest <- with(Data()$ddf, min(paste(yr, mo, dy, sep = "-")))

    updateDateRangeInput(session, "date_range_map", start = earliest, end = latest)
    updateDateRangeInput(session, "date_range_chart", start = earliest, end = latest)  
  })
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Render summary statistics
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$summary <- renderUI({
    tot <- paste("Total rides:", nrow(Data()$data))
    start <- paste("Most frequent start station, # of visits:", 
                   paste(Data()$most_start, collapse=", "))
    end <- paste("Most frequent end station, # of visits:", 
                 paste(Data()$most_end, collapse=", "))
    avg <- paste("Average ride time:", Data()$avg_ride, "minutes")
    shortest <- paste("Shortest trip:", Data()$shortest_trip)
    longest <- paste("Longest trip:", Data()$longest_trip)
    avg_rides <- paste("Average number of rides per day:", 
                       round(mean(Data()$trips_in_day$N), 2))
    most_trips_in_day <- paste("Most trips in one day:", Data()$most_trips_in_day)
    coldest <- paste("Coldest trip(s):", Data()$coldest)
    warmest <- paste("Warmest trip(s):", Data()$warmest)
    HTML(paste(tot, start, end, avg, shortest, longest, avg_rides, 
               most_trips_in_day, coldest, warmest, 
               sep = "<br/>")
         )  
  })

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Render data table for most frequency routes
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$popular <- renderDataTable({
    Data()$most_freq_trips[,-5]
  }, options = list(lengthMenu = c(25, 50, 100)))
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Render map of visited stations
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$maps <- renderPlot({
    input$go_map_button

    isolate({
      # Check minimum and maximum settings
      validate(
        need(input$min_map_ride_freq <= input$max_map_ride_freq, "Invalid range")
      )
      
      freq <- Freq()
      
      # Check map setting
      withProgress(message = 'Generating map', value = 0.1, {
        if (input$facet_map == "all") {
          agg <- aggregate(N ~ stationName + latitude + longitude,
                           data = freq, sum) %>%
            filter(N >= input$min_map_ride_freq, N <= input$max_map_ride_freq)
          validate(need(nrow(agg) > 0, "No matching results"))
          incProgress(0.5)
          gp <- map_stations(df = agg)
          incProgress(0.3) 
          gp <- gp + ggtitle("Map of all visited stations")
        } else {
          df <- filter(freq, 
                       N >= input$min_map_ride_freq,
                       N <= input$max_map_ride_freq)
          validate(need(nrow(df) > 0, "No matching results"))
          incProgress(0.2)
          gp <- map_stations(df = df)
          incProgress(0.5)
          gp <- gp + facet_wrap(~ type, ncol = 2) + ggtitle("Map of visited stations")
          incProgress(0.2)
        }
        setProgress(1)
      })
      
      print(gp)
    })
  }, height = 800)
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Render plots
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$charts <- renderPlot({
    format_by_datetime(Data()$data, 
                       format(input$date_range_chart[1]), 
                       format(input$date_range_chart[2])) %>%
    group_by(yr, mo, mo.abbr) %>% 
      summarize(N = length(mo)) %>%
      arrange(yr, mo) %>%
      ggplot(aes(x = mo.abbr, 
                 y = N, 
                 fill = yr)) + 
      geom_bar(stat = "identity", position="dodge") +
      xlab("Month") + ylab("Number of trips") +
      scale_fill_discrete("Year") +
      ggtitle("Number of trips by month and year")
  })
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Render data table
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$ridedata <- renderDataTable({
    Data()$data
  }, options = list(lengthMenu = c(25, 50, 100)))
  
})