#' getDATRASfun.R

#' Extracts age data files from DATRAS
#' @param record, survey, startyear, endyear, startquarter, endquarter, parallel = FALSE, cores = NULL
#' @return none
#' @seealso none
#' @details the update is slow, avoiding straining the server or client.
#'   please allow this call to run overnight for a complete upgrade.
#' @keywords download, DATRAS, survey, age, length
#' @examples \dontrun{
#'  getDATRAS()
#' }
#' @export
#
getDATRAS <- function(record, survey, startyear, endyear, quarters, parallel = FALSE, cores = NULL) {
  #   library(XML)
  #   library(doParallel)
  #   library(parallel)
  #   library(foreach)
  #   library(data.table)
  strt <- Sys.time()
  #
  seqYear <- startyear:endyear
  #
  if(!record %in% c("HL", "HH", "CA") ) stop("Please specify record type: HH (haul meta-data),
                                             HL (Species length-based data),
                                             CA (species age-based data)")
  getURL <- apply(expand.grid(record, survey, seqYear, quarters),
                    1,
                    function(x) paste0("http://datras.ices.dk/WebServices/DATRASWebService.asmx/get",
                                       x[1],
                                       "data?survey=", x[2],
                                       "&year=", x[3],
                                       "&quarter=", x[4]))
  #
  if(parallel == TRUE) {
    if(missing("cores")) stop("Please specify how many cores you wish to devote to this task.")
    #
    unregister <- function() {
      env <- foreach:::.foreachGlobals
      rm(list=ls(name=env), pos=env)
    } # close unregister
    #
    cl <- makeCluster(cores)
    registerDoParallel(cores = cl)
    #
    getDATA <- foreach(temp = getURL,
                     .combine = function(...) rbindlist(list(...), fill = TRUE),
                     .multicombine = T,
                     .inorder = F,
                     .maxcombine = 1000,
                     .packages = c("XML", "data.table")) %dopar% {
                       data.table(t(xmlSApply(xmlRoot(xmlTreeParse(temp, isURL = T, options = HUGE, useInternalNodes =  T)),
                                              function(x) xmlSApply(x, xmlValue))))
                     } # close foreach %dopar%
    stopCluster(cl)
    unregister()
  } # close parallel == TRUE
  #
  if(parallel == FALSE) {
    getDATA <- foreach(temp = getURL,
                     .combine = function(...) rbindlist(list(...), fill = TRUE),
                     .multicombine=T,
                     .inorder=F,
                     .maxcombine=1000,
                     .packages = c("XML", "data.table")) %do% {
                       data.table(t(xmlSApply(xmlRoot(xmlTreeParse(temp, isURL = T, options = HUGE, useInternalNodes =  T)),
                                              function(x) xmlSApply(x, xmlValue))))
                     } # close foreach %do%
  } # close parallel == FALSE
  print(Sys.time()-strt)
  return(getDATA)
} # close function
