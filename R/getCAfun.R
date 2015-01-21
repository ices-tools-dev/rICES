#' getCAfun.R

#' Extracts age data files from DATRAS
#' @param survey, startyear, endyear, startquarter, endquarter, parallel = FALSE
#' @return none
#' @seealso \code{\link{cacheDATRAS}}, a\code{\link{loadDATRAS}}, \code{\link{getHLfun}}, and \code{\link{getHHfun}}
#' @details the update is slow, avoiding straining the server or client.
#'   please allow this call to run overnight for a complete upgrade.
#' @keywords download, DATRAS, survey, age
#' @examples \dontrun{
#'  getCAfun()
#' }
#' @export



###########################################################################
#
getCAfun <- function(survey, startyear, endyear, startquarter, endquarter, parallel = FALSE) {
  # Downloads and parses XML species length data from ICES DATRAS
#   library(XML)
#   library(doParallel)
#   library(parallel)
#   library(foreach)
  #
  seqYear <- startyear:endyear
  seqQuarter <- startquarter:endquarter
  #
  getCAurl <- apply(expand.grid(survey, seqYear, seqQuarter),
                    1,
                    function(x) paste0("http://datras.ices.dk/WebServices/DATRASWebService.asmx/getCAdata",
                                       "?survey=", x[1],
                                       "&year=", x[2],
                                       "&quarter=", x[3]))
  strt <- Sys.time()
  if(parallel == TRUE) {
    #
    cl <- makeCluster(detectCores())
    registerDoParallel(cores = cl)
    on.exit(stopCluster(cl))
    #
    getCA <- foreach(temp = getCAurl, .combine=rbind, .packages = "XML" ) %dopar% { #%dopar% parallel %do% sequential
      xmlCA <- data.frame(t(xmlSApply(xmlRoot(xmlTreeParse(temp, isURL = T, options = HUGE, useInternalNodes =  T)),
                                      function(x) xmlSApply(x, xmlValue))),
                          row.names = NULL,
                          stringsAsFactors = F)
      if(length(xmlCA) > 1){ # Not all quarters are sampled
        # Data wrangling to make sure -9 = NA and numbers are numeric
        xmlCA <- data.frame(lapply(xmlCA, function(x) gsub("[[:space:]]","", x)), stringsAsFactors = FALSE)
        #
        CAnumCols <- c("Quarter", "SweepLngt", "StNo", "HaulNo", "Year",
                       "SpecCode", "AreaType", "LngtClass", "Maturity", "Age", "NoAtALK",
                       "IndWgt", "DateofCalculation", "Valid_Aphia")
        #
        xmlCA[CAnumCols] <- suppressWarnings(sapply(xmlCA[CAnumCols], as.numeric)) # Warning here is OK but silenced
        xmlCA[xmlCA == "-9" |
                xmlCA < -9] <- NA
        #
      }
      return(xmlCA)
#       stopCluster(cl)
    }
  }

  if(parallel == FALSE) {
    getCA <- foreach(temp = getCAurl, .combine=rbind, .packages = "XML" ) %do% { #%dopar% parallel %do% sequential
      xmlCA <- data.frame(t(xmlSApply(xmlRoot(xmlTreeParse(temp, isURL = T, options = HUGE, useInternalNodes =  T)),
                                      function(x) xmlSApply(x, xmlValue))),
                          row.names = NULL,
                          stringsAsFactors = F)
      if(length(xmlCA) > 1){ # Not all quarters are sampled
        # Data wrangling to make sure -9 = NA and numbers are numeric
        xmlCA <- data.frame(lapply(xmlCA, function(x) gsub("[[:space:]]","", x)), stringsAsFactors = FALSE)
        #
        CAnumCols <- c("Quarter", "SweepLngt", "StNo", "HaulNo", "Year",
                       "SpecCode", "AreaType", "LngtClass", "Maturity", "Age", "NoAtALK",
                       "IndWgt", "DateofCalculation", "Valid_Aphia")
        #
        xmlCA[CAnumCols] <- suppressWarnings(sapply(xmlCA[CAnumCols], as.numeric)) # Warning here is OK but silenced
        xmlCA[xmlCA == "-9" |
                xmlCA < -9] <- NA
        #
      }
      return(xmlCA)
    } # close sequential
  }
  print(Sys.time()-strt)
  return(getCA)
}
