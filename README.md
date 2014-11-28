# Bike Share Toronto Ride Mapper

**For [Bike Share Toronto](https://www.bikesharetoronto.com/) users.**

I was curious to see what were my usage patterns and which docks I frequently visited.
This program takes your personal ride data and creates a map of your dock usage.
The maps are created in R using the
[ggmap](http://cran.r-project.org/web/packages/ggmap/index.html) package.

A report of usage statistics are also generated using R Markdown (in development).

## Requirements
 * R plus the following packages:
  * ggmap
  * plyr
  * stringr

## Usage

### Getting ride data

There's two ways to download your ride data: 
1. Logging into your online account and copy and paste all of your trip data to
a text file; or 
2. Automatically downloading your data using the provided script (requires your
login credentials) (not available yet)

The format of the data should be exactly as it appears when viewing your trip data.

## Disclaimer
This is a personal project to learn some new R packages (e.g. `ggmap`). It is
entirely my own and not affiliated with Bike Share Toronto.
