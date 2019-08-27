library(shiny)
library(gridExtra)
source("R/mapper_funcs.R")

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
    
    withProgress(message = "Loading ride data", value = 0.1, {
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
      
      incProgress(0.6, message = "Analyzing ride data")
      
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
  
  RouteDurations <- reactive({
    format_by_datetime(Data()$data, 
                       format(input$date_range_chart[1]), 
                       format(input$date_range_chart[2]))
  })
  
  RouteFreqByMonth <- reactive({
    RouteDurations() %>% group_by(yr, mo, mo.abbr, route) %>%
      summarize(N = length(route),
                avg.duration = round(mean(duration), digits=2),
                sd = round(sd(duration), digits=2))
  })
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Update maximum ride frequency for maps
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  observe({
    group_by(Data()$freq, stationName) %>%
      summarize(N = sum(N)) %>%
      select(N) %>%
      max %>%
    updateNumericInput(session, "max_map_ride_freq", value = .)  
    
    # Get earliest and latest dates
    latest <- with(Data()$ddf, max(paste(yr, mo, dy, sep = "-")))
    earliest <- with(Data()$ddf, min(paste(yr, mo, dy, sep = "-")))

    updateDateRangeInput(session, "date_range_map", start = earliest, end = latest)
    updateDateRangeInput(session, "date_range_chart", start = earliest, end = latest)  
    
    # Update route selection
    updateSelectInput(session, "routes", 
                      choices = Data()$most_freq_trips$route,
                      selected = Data()$most_freq_trips[1:6, "route"])
                      
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
    HTML(paste(tot, start, end, avg, shortest, longest, avg_rides, 
               most_trips_in_day,
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
    withProgress(message = "Loading", value = 0.1, {
      df <- RouteDurations()
      validate(need(nrow(df) > 0, "No matching results"))  
      
      incProgress(0.6)
      if (input$chart_type == "plot_by_month") {
        gp1 <- group_by(df, yr, mo, mo.abbr) %>% 
          summarize(N = length(mo)) %>%
          arrange(yr, mo) %>%
          ggplot(aes(x = mo.abbr, 
                     y = N, 
                     fill = yr)) + 
          geom_bar(stat = "identity", position="dodge") +
          xlab("Month") + ylab("Number of trips") +
          scale_fill_discrete("Year") +
          ggtitle("Number of trips by month and year")
      } else if (input$chart_type == "plot_trip_by_day") {
        gp1 <- group_by(df, yr, mo, mo.abbr, dy) %>%
          summarize(avg.duration = round(mean(duration), digits=2),
                    sd = round(sd(duration), digits=2)) %>%
          ggplot(aes(x = dy,
                     y = avg.duration, color=yr, group=yr)) +
          geom_point() +
          geom_line() +
          geom_errorbar(aes(ymin = avg.duration - sd, ymax = avg.duration + sd, 
                            color = yr), 
                        width = 0.2, position=position_dodge(width = 0.1)) +
          facet_wrap(~ mo.abbr, ncol = 2, scale="free_x") +
          theme(axis.text.x = element_text(angle= 45, hjust = 1, size=8)) +
          xlab("Day") + ylab("Duration") +
          ggtitle("Trip duration by day")
      } else if (input$chart_type == "plot_trip_by_month") {
        gp1 <- df %>%
          ggplot(aes(x = mo.abbr,
                     y = duration, fill=yr)) +
          geom_boxplot(position="dodge") +
          xlab("Month") + ylab("Duration") +
          ggtitle("Trip duration by month")
      } else if (input$chart_type == "plot_trip_by_station") {        
        freq_routes <- RouteFreqByMonth() %>%
          subset(route %in% input$routes)
        
        gp1 <- freq_routes %>%
          ggplot(aes(x = mo.abbr,
                     y = avg.duration, group=yr)) +
          geom_point(aes(color=yr)) +
          geom_line(aes(color=yr)) +
          geom_errorbar(aes(ymin = avg.duration - sd, ymax = avg.duration + sd, 
                            color = yr), 
                        width = 0.2, position=position_dodge(width = 0.1)) +
          facet_wrap(~ route, ncol =2, scale="free_x") +
          xlab("Month") + ylab("Average Duration") +
          ggtitle("Trip duration by most frequent routes")
      } else if (input$chart_type == "plot_time_of_day") {
        gp1 <- df %>% 
          mutate(HR=as.numeric(as.character(hr)) + 
                   as.numeric(as.character(min))/60) %>%
          ggplot(aes(x = HR, group = yr, fill = yr)) +
          geom_histogram(binwidth = 0.25) +
          xlab("Time (24 hr)") + ylab("Count") +
          facet_wrap(~ yr, ncol=1) +
          ggtitle("Trips by time of day") +
          scale_x_continuous(breaks = seq(1,24,1))
      } else if (input$chart_type == "plot_num_trips_by_day") {
        gp1 <- df %>%
          group_by(dy, mo, yr) %>% 
          summarize(N = length(route)) %>%
          ggplot(aes(x = N, fill = yr, group = yr)) +
          geom_histogram(position="dodge", binwidth = 1) +
          xlab("Trips per day") + ylab("Count") +
          ggtitle("Number of trips per day")
      }
      
      setProgress(0.9)
      
      print(gp1)
      
      setProgress(1)
    })
  })
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Render data table
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$ridedata <- renderDataTable({
    Data()$data
  }, options = list(lengthMenu = c(25, 50, 100)))
  
})