library(shiny)

shinyUI(fluidPage(
#   title = 'Bike Share Toronto Report',
  
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
            radioButtons("facet_map", "Choose map type:",
                         c("Show all stations in one map" = "all",
                           "Facet by start and end station" ="facet")
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
                           max = format(Sys.time(), "%Y-%m-%d"))
            
          ),
          conditionalPanel(
            'input.dataset == "Ride Data"',
            
            sliderInput("years", "Select year range:",
                        min = 2010, max = 2015,
                        value = c(2010, 2015),
                        round = TRUE)
            
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
        tabPanel('Charts', plotOutput('charts')),
        tabPanel('Ride Data', dataTableOutput('ridedata')),
        tabPanel('Usage', includeMarkdown('docs/introduction.md'))
      )
    )
  )
))