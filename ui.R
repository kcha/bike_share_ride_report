library(shiny)

shinyUI(fluidPage(
  headerPanel("Bike Share Toronto Ride Visualization and Report"),
  
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        helpText(HTML("<b>LOAD RIDE DATA</b>")),
        fileInput(
          "file", "Choose tab-delimited file:", 
          multiple=FALSE,
          accept = c('text/plain', 'text/tab-separated-values')
        )
      ),
      
      conditionalPanel(
        'input.dataset != "Introduction"',
        wellPanel(
          helpText(HTML("<b>SETTINGS</b>")),
          conditionalPanel(
            'input.dataset == "Summary Statistics"',
            helpText('Some summary statistics from ride data')
          ),
          conditionalPanel(
            'input.dataset == "Most Popular Routes"',
            helpText('Number of rides based on route')
          ),
          conditionalPanel(
            'input.dataset == "Maps"',
            selectInput("facet_map", "Choose facet type:",
                         c("None" = "all",
                           "Facet by start and end station" = "facet")
                         ),
            numericInput("min_map_ride_freq", "Minimum:", 1, min = 1),
            numericInput("max_map_ride_freq", "Maximum:", 100, min = 1),
            dateRangeInput("date_range_map", "Date range:", 
                           start = "2010-01-01", end = "2015-12-31",
                           max = format(Sys.time(), "%Y-%m-%d")),
            br(),
            actionButton("go_map_button", "Update"),
            p("Click 'Update' to refresh the map")
          ),
          conditionalPanel(
            'input.dataset == "Charts"',
            dateRangeInput("date_range_chart", "Date range:", 
                           start = "2010-01-01", end = "2015-12-31",
                           max = format(Sys.time(), "%Y-%m-%d")),
            radioButtons(
              "chart_type", "Select plot type:",
              c("Number of trips by month" = "plot_by_month",
                "Trip duration by day" = "plot_trip_by_day",
                "Trip duration by month" = "plot_trip_by_month",
                "Trip duration by most frequent routes" = "plot_trip_by_station")
            ),
            checkboxInput("show_weather", "Show weather data")
          ),
          conditionalPanel(
            'input.dataset == "Ride Data"',
            helpText("Data table showing the input data")
                        
          )
        )
      ),
      
      wellPanel(
        helpText(HTML("<b>SOURCE CODE</b>")),
        HTML('<a href="https://github.com/kcha/bike_share_ride_report" target="_blank">GitHub</a>')
      ),
      
      wellPanel(
        helpText(HTML("<b>ABOUT ME</b>")),
        HTML('Kevin Ha'),
        HTML('<br/>'),
        HTML('<a href="http://individual.utoronto.ca/hakevin" target="_blank">http://individual.utoronto.ca/hakevin</a>')
      )
    ),
    mainPanel(
      tabsetPanel(
        id = 'dataset',
        tabPanel('Summary Statistics', htmlOutput('summary')),
        tabPanel('Most Popular Routes', dataTableOutput('popular')),
        tabPanel('Maps', plotOutput('maps')),
        tabPanel('Charts', 
                 conditionalPanel(
                   'input.chart_type == "plot_trip_by_station"',
                   selectInput('routes', "Choose route(s):",
                               width = "100%",
                               multiple = TRUE,
                               choices = "")
                 ),
                 plotOutput('charts')),
        tabPanel('Ride Data', dataTableOutput('ridedata')),
        tabPanel('Usage', includeMarkdown('docs/introduction.md'))
      )
    )
  )
))