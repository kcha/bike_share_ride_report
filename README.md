# Bike Share Toronto Ride Report

**For [Bike Share Toronto](https://www.bikesharetoronto.com/) users.**

This program takes your personal ride data and creates a map of your station usage.
The maps are created in R using the
[ggmap](http://cran.r-project.org/web/packages/ggmap/index.html) package.

A summary report of usage statistics is generated using R Markdown (see
this [sample report](http://individual.utoronto.ca/hakevin/projects/ride_report/sample.html) 
for an example). 

## Requirements
 * RStudio (this project was developed using 0.98.1091)
 * R 3.1.1+ plus the following packages:
  * rjson
  * ggmap
  * dplyr
  * stringr
  * assertthat
  * shiny

## Usage

### Station data

Bike station geolocation data is obtained from http://www.bikesharetoronto.com/stations/json

### Getting ride data

Currently, the only way to get ride data is to log into your online account and
manually copy and paste all of your trip data to a text file. The format of the
trip data should be exactly as it appears when viewing your data online. See
[`sample_data/ride_data.txt`](https://github.com/kcha/bike_share_ride_report/blob/master/sample_data/ride_data.txt) for an example.

### Starting Shiny App

1. To start the app on your local machine, run the following command in RStudio
```r
# install.packages("shiny")
shiny::runGitHub('bike_share_ride_report', 'kcha')
```
2. Upload your ride data to generate your personal ride report

## Disclaimer
This is a personal project and not affiliated with Bike Share Toronto.
