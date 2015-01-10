#' Haul function
#'
#' More descriptive name
#' @param survey, startyear, endyear, startquarter, endquarter, parallel
#' @return none
#' @export



###########################################################################
getHHfun <- function(survey, startyear, endyear, startquarter, endquarter, parallel = FALSE){
  # Downloads and parses XML haul data from ICES DATRAS
  #   library(XML)
  #   library(doParallel)
  #   library(parallel)
  #   library(foreach)
  #
  seqYear <- startyear:endyear
  seqQuarter <- startquarter:endquarter
  #
  getHHurl <- paste0("http://datras.ices.dk/WebServices/DATRASWebService.asmx/getHHdata",
                     "?survey=", survey,
                     "&year=", seqYear,
                     "&quarter=", seqQuarter)
  strt <- Sys.time()
  if(parallel == TRUE) {
    cl <- makeCluster(2)
    registerDoParallel(cores = cl)
    getHH <- foreach(temp = getHHurl, .combine=rbind, .packages = "XML" ) %dopar% { #%dopar% parallel %do% sequential
      xmlHH <- data.frame(t(xmlSApply(xmlRoot(xmlTreeParse(temp, isURL = T, options = HUGE, useInternalNodes =  T)),
                                      function(x) xmlSApply(x, xmlValue))),
                          row.names = NULL,
                          stringsAsFactors = F)
      if(length(xmlHH) > 1){ # Not all quarters are sampled
        # Data wrangling to make sure -9 = NA and numbers are numeric
        xmlHH <- data.frame(lapply(xmlHH, function(x) gsub("[[:space:]]","", x)), stringsAsFactors = FALSE)[,1:29]
        #
        HHnumCols <- c("Quarter", "SweepLngt", "StNo", "HaulNo", "Year", "month", "Day", "TimeShot", "Stratum",
                       "HaulDur", "ShootLat", "ShootLong", "HaulLat", "HaulLong", "Depth", "HydroStNo",
                       "StdSpecRecCode", "Netopening", "Rigging", "Tickler")
        xmlHH[HHnumCols] <- suppressWarnings(sapply(xmlHH[HHnumCols], as.numeric)) # Warning here is OK but silenced
        xmlHH[xmlHH == "-9" |
                xmlHH < -9] <- NA
      }
      return(xmlHH)
      stopCluster(cl)
    } # close parallel
  } else {
    getHH <- foreach(temp = getHHurl, .combine=rbind, .packages = "XML" ) %do% { #%dopar% parallel %do% sequential
      xmlHH <- data.frame(t(xmlSApply(xmlRoot(xmlTreeParse(temp, isURL = T, options = HUGE, useInternalNodes =  T)),
                                      function(x) xmlSApply(x, xmlValue))),
                          row.names = NULL,
                          stringsAsFactors = F)
      if(length(xmlHH) > 1){ # Not all quarters are sampled
        # Data wrangling to make sure -9 = NA and numbers are numeric
        xmlHH <- data.frame(lapply(xmlHH, function(x) gsub("[[:space:]]","", x)), stringsAsFactors = FALSE)[,1:29]
        #
        HHnumCols <- c("Quarter", "SweepLngt", "StNo", "HaulNo", "Year", "month", "Day", "TimeShot", "Stratum",
                       "HaulDur", "ShootLat", "ShootLong", "HaulLat", "HaulLong", "Depth", "HydroStNo",
                       "StdSpecRecCode", "Netopening", "Rigging", "Tickler")
        xmlHH[HHnumCols] <- suppressWarnings(sapply(xmlHH[HHnumCols], as.numeric)) # Warning here is OK but silenced
        xmlHH[xmlHH == "-9" |
                xmlHH < -9] <- NA
      }
      return(xmlHH)
    } # close sequential
  }
  print(Sys.time()-strt)
  return(getHH)
}
#
