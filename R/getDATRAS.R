#' getDATRASfun.R

#' Extracts age data files from DATRAS
#' @param record, survey, startyear, endyear, startquarter, endquarter, parallel = FALSE, cores = NULL
#' @return none
#' @seealso none
#' @details the update is slow, avoiding straining the server or client.
#'   please allow this call to run overnight for a complete upgrade.
#' @keywords download, DATRAS, survey, age, length
#' @examples \dontrun{
#'  getDATRAS(record = "HH",
#'            survey="NS-IBTS",
#'            startyear = 2010,
#'            endyear = 2011,
#'            quarters = 1,
#'            parallel = TRUE,
#'            cores = 4)
#' }
#' @export
#
getDATRAS <- function(record, survey, startyear, endyear, quarters, parallel = FALSE, cores = NULL) {
  #   library(XML)
  #   library(doParallel)
  #   library(parallel)
  #   library(foreach)
  #   library(data.table)
  dataPolicy <- 
"  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~## DISCLAIMER AND COPYRIGHT ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  ### This dataset is for the sole use of the organisation or individual downloading this dataset. ###
  ### The geodata may not be redistributed without the permission of the International Council for ###
  ### the Exploration of the Sea (ICES).                                                           ###
  ###                                                                                              ###
  ### Redistribution rights are granted for hard-copy renditions or static,                        ###
  ### electronic map images (e.g. jpeg, gif, etc.) that are plotted,                               ###
  ### printed or publicly displayed with proper metadata and source/copyright attribution.         ###
  ### The geodata may be used in a Value-Added Software Application (like webservices)             ###
  ### with proper metadata and source/copyright attribution.                                       ###  
  ###                                                                                              ###
  ### Correct and appropriate data interpretation is solely the responsibility of data users.      ###   
  ### Data Users must not expressly or otherwise imply ICES substantiation of their work,          ###
  ### results, conclusions and/or recommendations.                                                 ###
  ###                                                                                              ###
  ### Users of the geodata must respect any and all restrictions on the use or reproduction        ###
  ### of data such as restrictions on use for commercial purposes.                                 ###
  ###                                                                                              ###
  ### The use of data must comply with the ICES data policy. The full ICES Data Policy             ###
  ### can be accessed on this link:                                                                ###  
  ### http://www.ices.dk/marine-data/guidelines-and-policy/Pages/ICES-data-policy.aspx             ###
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#"
  #
  cat(dataPolicy)
  answer <- toupper(readline(prompt = "I accept the ICES Data Policy: "))
  accepted <- ifelse(answer %in% c("YES", "NO"), "true", "false")
  #
  if(answer == "YES") {
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
      temp = getURL
      getDATA <- foreach::foreach(temp = getURL,
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
      getDATA <- foreach::foreach(temp = getURL,
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
    } #  Close answer == YES if statement 
  if(answer != "YES") {
    cat("To download data, you must read the ICES Data Policy and type 'yes' into the console window.")
    } # Close answer != YES if statement
} # close function
