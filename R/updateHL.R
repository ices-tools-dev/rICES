

###########################################################################
getHLfun <- function(survey, startyear, endyear, startquarter, endquarter, parallel = FALSE) {
  library(XML)
  library(doParallel)
  library(parallel)
  library(foreach)
  library(data.table)
  strt <- Sys.time()

  #
  seqYear <- startyear:endyear
  seqQuarter <- startquarter:endquarter
  #
  getHLurl <- apply(expand.grid(survey, seqYear, seqQuarter),
                    1,
                    function(x) paste0("http://datras.ices.dk/WebServices/DATRASWebService.asmx/getHLdata",
                                       "?survey=", x[1],
                                       "&year=", x[2],
                                       "&quarter=", x[3]))
  #
  if(parallel == TRUE) {
    unregister <- function() {
      env <- foreach:::.foreachGlobals
      rm(list=ls(name=env), pos=env)
    } # close unregister

    cl <- makeCluster(detectCores())
    registerDoParallel(cores = cl)
    #

    getHL <- foreach(temp = getHLurl,
                     .combine = function(...) rbindlist(list(...), fill = TRUE),
                     .packages = c("XML", "data.table")) %dopar% {
                      data.table(t(xmlSApply(xmlRoot(xmlTreeParse(temp, isURL = T, options = HUGE, useInternalNodes =  T)),
                                                       function(x) xmlSApply(x, xmlValue))))
                      } # close foreach
    stopCluster(cl)
    unregister()
    } # close parallel
  #
  if(parallel == FALSE) {
    getHL <- foreach(temp = getHLurl,
                     .combine = function(...) rbindlist(list(...), fill = TRUE),
                     .packages = c("XML", "data.table")) %do% {
                       data.table(t(xmlSApply(xmlRoot(xmlTreeParse(temp, isURL = T, options = HUGE, useInternalNodes =  T)),
                                              function(x) xmlSApply(x, xmlValue))))
                     } # close foreach
  }
  print(Sys.time()-strt)
  return(getHL)

} # close function

# tt <- getHLfun(survey = "NS-IBTS",
#          startyear = 2014, endyear = 2014,
#          startquarter = 1, endquarter = 4, parallel = T)
#
#
#
# HLnumCols <- c("Quarter", "SweepLngt", "StNo", "HaulNo", "Year",
#                "SpecCode", "SpecVal", "TotalNo", "CatIdentifier", "NoMeas",
#                "SubFactor", "SubWgt", "CatCatchWgt", "LngtClass","HLNoAtLngt",
#                "DateofCalculation", "Valid_Aphia")
#
#
#
#
#
#
#
# data.frame(lapply(xmlHH, function(x) gsub("[[:space:]]","", x)), stringsAsFactors = FALSE)[,1:29]
#
# str(dtnew)
#
#
#
# #
# if(length(xmlHH) > 1){ # Not all quarters are sampled
#   # Data wrangling to make sure -9 = NA and numbers are numeric
#   xmlHH <- data.frame(lapply(xmlHH, function(x) gsub("[[:space:]]","", x)), stringsAsFactors = FALSE)[,1:29]
#   #
#   HHnumCols <- c("Quarter", "SweepLngt", "StNo", "HaulNo", "Year", "month", "Day", "TimeShot", "Stratum",
#                  "HaulDur", "ShootLat", "ShootLong", "HaulLat", "HaulLong", "Depth", "HydroStNo",
#                  "StdSpecRecCode", "Netopening", "Rigging", "Tickler")
#   xmlHH[HHnumCols] <- suppressWarnings(sapply(xmlHH[HHnumCols], as.numeric)) # Warning here is OK but silenced
#   xmlHH[xmlHH == "-9" |
#           xmlHH < -9] <- NA
# }
