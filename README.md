# R for ICES: Linking R with DATRAS Web Services
Scott Large

__Introduction__:  Most of the advice that ICES provides relies heavily upon DATRAS and other ICES Data Centre products. The user interfaces of these products are well developed and very user friendly for point-and-click data exploration and accessing single years or surveys. However, many of the advisory processes could be more transparent and reproducible by using a consistent set of functions within an 'ICES' R package. Primarily, these functions interact directly wiht the [ICES DATRAS webservices](https://datras.ices.dk/WebServices/DATRASWebService.asmx). Users of these products are subject to the [ICES Data Policy](http://www.ices.dk/marine-data/Documents/ICES_Data_Policy_2012.pdf).

### Basic features

This package interacts directly with the [ICES DATRAS webservices](https://datras.ices.dk/WebServices/DATRASWebService.asmx). Currently, it has three functions which extract station data, age data, and length data from the DATRAS web services returning _data.table_ objects.


### R-library installation
Need only only once or when updating the ICES package:

```r
require(devtools)
install_github("slarge/ices")
```

### Basic usage

__Attaching the library:__

```r
library(ices)
```

__Station data__: 

```r
st <- getHHfun(survey="NS-IBTS",year=2013,quarter=3)
dim(st)
```
