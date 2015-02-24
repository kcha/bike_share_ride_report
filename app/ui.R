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
            'input.dataset === "Summary Statistics"',
            checkboxGroupInput('show_vars', 'Columns in diamonds to show:',
                               names(cars), selected = names(cars))
          ),
          conditionalPanel(
            'input.dataset === "Maps"',
            helpText('Click the column header to sort a column.')
          ),
          conditionalPanel(
            'input.dataset === "Ride Data"',
            
            sliderInput("years", "Select year range",
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
        HTML('<br>'),
        HTML('<a href="http://individual.utoronto.ca/hakevin" target="_blank">http://individual.utoronto.ca/hakevin</a>')
      )
    ),
    mainPanel(
      tabsetPanel(
        id = 'dataset',
        tabPanel('Introduction', includeMarkdown('docs/introduction.md')),
        tabPanel('Summary Statistics', dataTableOutput('mytable1')),
        tabPanel('Maps', dataTableOutput('mytable2')),
        tabPanel('Ride Data', dataTableOutput('ridedata'))
      )
    )
  )
))