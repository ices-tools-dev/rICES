---
title: 'R for ICES: Linking R with DATRAS Web Services'
author: "Scott Large"
date: "Wednesday, June 24, 2015"
output: html_document
---

## Introduction
Most of the advice that ICES provides relies heavily upon DATRAS and other ICES Data Centre products. The user interfaces of these products are well developed and very user friendly for point-and-click data exploration and accessing a few years or surveys. Users interested in accessing the data in a more transparent, reproducible, and streamlined way can use the following [R](http://cran.r-project.org/) code to download batches of surveys, years, and quarters.

The following function interacts directly with the [ICES DATRAS WebServices](https://datras.ices.dk/WebServices/DATRASWebService.asmx) and is able to extract station data (HH), age data (CA), and length data (HL) and returns each record as an R object. Keep in mind that the speed of this function is limited by bandwidth availability and parsing XML can be a labor intensive task, especially for long time-series. The code is written to parse the XML using parallel processing, which can significantly reduce run-time. The full time series of North Sea IBTS length based data has ~ 3 million rows and can take 30+ minutes to download and parse in parallel. Note: users of these products are subject to the [ICES Data Policy](http://www.ices.dk/marine-data/Documents/ICES_Data_Policy_2012.pdf) and the following code is provided without warranty and may be under further development. Please contribute to the development on [GitHub](https://github.com/slarge/ices) or contact the author directly <scott.large@ices.dk>.

## Basic Ussage

Install the ICES R package and load it.

```r
require(devtools)
install_github("slarge/ices")

library(ices)
```

### Haul Data
To download the haul meta-data from the first and third quarters in 2010 and 2011 of the North Sea International Bottom Trawl Survey, use the following code:
```r 
haulData <- getDATRAS(record = "HH",
                      survey="NS-IBTS",
                      startyear = 2010,
                      endyear = 2011,
                      quarters = c(1,3),
                      parallel = TRUE,
                      cores = 4)
dim(haulData)
```

### Length Data
To download the length-based information from the first quarter in 2000-2001 of the Baltic International Trawl Survey, use the following code:
```r 
lengthData <- getDATRAS(record = "HL",
                      survey="BITS",
                      startyear = 2000,
                      endyear = 2001,
                      quarters = 1,
                      parallel = TRUE,
                      cores = 4)
dim(lengthData)
```

### Age Data
To download the age-based information from all quarters in 1999 of the French Southern Atlantic Bottom Trawl Survey, use the following code:
```r
ageData <- getDATRAS(record = "CA",
                      survey="EVHOE",
                      startyear = 1999,
                      endyear = 1999,
                      quarters = c(1:4),
                      parallel = TRUE,
                      cores = 4)
dim(ageData)
```

The function above returns *data.table* objects, which are able to efficiently handle very large amounts of data. 
