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
#
###########################################################################
getHLfun <- function(survey, startyear, endyear, startquarter, endquarter, parallel = FALSE) {
  #   library(XML)
  #   library(doParallel)
  #   library(parallel)
  #   library(foreach)
  #   library(data.table)
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
