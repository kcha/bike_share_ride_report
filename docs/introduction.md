# Usage

This Shiny app takes your personal ride data from [Bike Share Toronto](https://www.bikesharetoronto.com/) 
and creates a collection of summary reports of your ride usage.

## Usage

### Getting ride data

Currently, the only way to get ride data is to log into your Bike Share Toronto online account and
manually copy and paste all of your trip data to a tab-delimited text file. The format of the
trip data should be exactly as it appears when viewing your data online. See
[`sample_data/ride_data.txt`](https://github.com/kcha/bike_share_ride_report/blob/master/sample_data/ride_data.txt) for an example.

### Usage steps

1. Upload your saved ride data file using the submission form on the left (titled `LOAD RIDE DATA`)
1. The Shiny App will begin processing your file
1. Use the tabs in the main panel to view the different charts and statistics

## Privacy

The data you upload will be analyzed solely to generate the summary reports for 
this Shiny app. 

If you are running this app on [shinyapps.io](http://www.shinyapps.io/), your data
will be uploaded to their servers for analysis (see their [terms of use](http://www.rstudio.com/about/shiny-apps-terms-use/)).

If you don't want to upload your data to a third-party server, then simply run 
this Shiny App locally by downloading the source code from [GitHub](https://github.com/kcha/bike_share_ride_report). 
Alternatively, you can execute the following command in R:

```r
shiny::runGitHub('bike_share_ride_report', 'kcha')
```

## Disclaimer
This is a personal project and not affiliated with Bike Share Toronto.

