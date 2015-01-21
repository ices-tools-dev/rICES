#' getHLfun.R

#' Extracts length data files from DATRAS
#' @param survey, startyear, endyear, startquarter, endquarter, parallel = FALSE
#' @return none
#' @seealso \code{\link{cacheDATRAS}}, a\code{\link{loadDATRAS}}, \code{\link{getCAfun}}, and \code{\link{getHHfun}}
#' @details the update is slow, avoiding straining the server or client.
#'   please allow this call to run overnight for a complete upgrade.
#' @keywords download, DATRAS, survey, age
#' @examples \dontrun{
#'  getHLfun()
#' }
#' @export

###########################################################################
#
getHLfun <- function(survey, startyear, endyear, startquarter, endquarter, parallel = FALSE) {
  # Downloads and parses XML species length data from ICES DATRAS
#   library(XML)
#   library(doParallel)
#   library(parallel)
#   library(foreach)
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
  strt <- Sys.time()
  if(parallel == TRUE) {
    cl <- makeCluster(detectCores())
    registerDoParallel(cores = cl)
    on.exit(stopCluster(cl))

    getHL <- foreach(temp = getHLurl, .combine=rbind, .packages = "XML" ) %dopar% { #%dopar% parallel %do% sequential
      xmlHL <- data.frame(t(xmlSApply(xmlRoot(xmlTreeParse(temp, isURL = T, options = HUGE, useInternalNodes =  T)),
                                      function(x) xmlSApply(x, xmlValue))),
                          row.names = NULL,
                          stringsAsFactors = F)
      if(length(xmlHL) > 1){ # Not all quarters are sampled
        # Data wrangling to make sure -9 = NA and numbers are numeric
        xmlHL <- data.frame(lapply(xmlHL, function(x) gsub("[[:space:]]","", x)), stringsAsFactors = FALSE)
        #
        HLnumCols <- c("Quarter", "SweepLngt", "StNo", "HaulNo", "Year",
                       "SpecCode", "SpecVal", "TotalNo", "CatIdentifier", "NoMeas",
                       "SubFactor", "SubWgt", "CatCatchWgt", "LngtClass","HLNoAtLngt",
                       "DateofCalculation", "Valid_Aphia")
        #
        xmlHL[HLnumCols] <- suppressWarnings(sapply(xmlHL[HLnumCols], as.numeric)) # Warning here is OK but silenced
        xmlHL[xmlHL == "-9" |
                xmlHL < -9] <- NA
      }
      return(xmlHL)
      stopCluster(cl)
    } # close parallel
  }
  if(parallel == FALSE) {
    getHL <- foreach(temp = getHLurl, .combine=rbind, .packages = "XML" ) %do% { #%dopar% parallel %do% sequential
      xmlHL <- data.frame(t(xmlSApply(xmlRoot(xmlTreeParse(temp, isURL = T, options = HUGE, useInternalNodes =  T)),
                                      function(x) xmlSApply(x, xmlValue))),
                          row.names = NULL,
                          stringsAsFactors = F)
      if(length(xmlHL) > 1){ # Not all quarters are sampled
        # Data wrangling to make sure -9 = NA and numbers are numeric
        xmlHL <- data.frame(lapply(xmlHL, function(x) gsub("[[:space:]]","", x)), stringsAsFactors = FALSE)
        #
        HLnumCols <- c("Quarter", "SweepLngt", "StNo", "HaulNo", "Year",
                       "SpecCode", "SpecVal", "TotalNo", "CatIdentifier", "NoMeas",
                       "SubFactor", "SubWgt", "CatCatchWgt", "LngtClass","HLNoAtLngt",
                       "DateofCalculation", "Valid_Aphia")
        #
        xmlHL[HLnumCols] <- suppressWarnings(sapply(xmlHL[HLnumCols], as.numeric)) # Warning here is OK but silenced
        xmlHL[xmlHL == "-9" |
                xmlHL < -9] <- NA
      }
      return(xmlHL)
    } # close sequential
  }
  print(Sys.time()-strt)
  return(getHL)
}
