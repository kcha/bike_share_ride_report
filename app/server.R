library(shiny)
source("../R/mapper_funcs.R")

shinyServer(function(input, output) {
  
  Data <- reactive({
    if (is.null(input$file)) {
      data <- get_ride_data("../sample_data/ride_data.txt")
    } else {
      data <- get_ride_data(input$file$datapath)  
    }
    
    ddf <- format_by_datetime(data)
    
    info <- list(data=data, ddf=ddf)
    return(info)
  })  
  
  # a large table, reative to input$show_vars
  output$mytable1 <- renderDataTable({
    library(ggplot2)
    cars[, input$show_vars, drop = FALSE]
  })
  
  # sorted columns are colored now because CSS are attached to them
  output$mytable2 <- renderDataTable({
    mtcars
  }, options = list(orderClasses = TRUE))
  
  # customize the length drop-down menu; display 5 rows per page by default
  output$ridedata <- renderDataTable({
    subset(Data()$data
  }, options = list(lengthMenu = c(10, 30, 50, 100), pageLength = 5))
  
})