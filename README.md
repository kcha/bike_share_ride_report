# Bike Share Toronto Ride Mapper

**For [Bike Share Toronto](https://www.bikesharetoronto.com/) users.**

This program takes your personal ride data and creates a map of your dock usage.
The maps are created in R using the
[ggmap](http://cran.r-project.org/web/packages/ggmap/index.html) package.

A summary report of usage statistics is generated using R Markdown. 

## Requirements
 * R 3.1.1+ plus the following packages:
  * rjson
  * ggmap
  * dplyr
  * stringr

## Usage

### Getting ride data

There's two ways to download your ride data: 

1. Logging into your online account and copy and paste all of your trip data to
a text file; or 
2. Automatically downloading your data using your credentials (not available yet)

The format of the trip data should be exactly as it appears when viewing your
data online.

## Disclaimer
This is a personal project and not affiliated with Bike Share Toronto.
