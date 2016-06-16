#' getDATRAS.R

#' Downloads and parses data from ICES DATRAS database using web services
#' @param record, survey, startyear, endyear, startquarter, endquarter, parallel = FALSE, cores = NULL, time = NULL
#' @return none
#' @seealso none
#' @details none
#' @keywords download, DATRAS, survey, age, length
#' @examples \dontrun{
#'  getDATRAS(record = "HL", 
#'            survey = "NS-IBTS", 
#'            startyear = 2010, 
#'            endyear = 2010, 
#'            quarters = 1, 
#'            parallel = FALSE,
#'            cores = NULL,
#'            time = NULL)
#' }
#' @export
#
getDATRAS <- function(record = "HL",
                      survey = "NS-IBTS", 
                      startyear = 2015, 
                      endyear = 2015, 
                      quarters = 1, 
                      parallel = TRUE, 
                      cores = 4, 
                      time = TRUE) {
  # 
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
  ###                                                                                              ###
  ### By continuing, the user implicitly accepts and agrees to the terms of the ICES Data Policy   ###
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#"
  #
  cat(dataPolicy)
  #
  seqYear <- startyear:endyear
  #
  if(!record %in% c("HL", "HH", "CA")) {
    stop("Please specify record type: HH (haul meta-data),
                                            HL (Species length-based data),
                                             CA (species age-based data)")
  } # close record catch
  # 
  # Create list of web service URLs
  getURL <- apply(expand.grid(record, survey, seqYear, quarters),
                  1,
                  function(x) paste0("http://datras.ices.dk/WebServices/DATRASWebService.asmx/get",
                                     x[1],
                                     "data?survey=", x[2],
                                     "&year=", x[3],
                                     "&quarter=", x[4]))
  # 
  # Download web service responses into a list of "iter" objects for parallel parsing 
  downloadStart <- proc.time()
  listURL <-  iterators::iter(lapply(getURL, httr::GET))
  downloadFinish <- proc.time()
  #
  if(parallel == TRUE) {
    # 
    if(missing("cores")) {
      stop("Please specify how many cores you wish to devote to this task.")
    } # Close cores catch
    #
    unregister <- function() {
      env <- foreach:::.foreachGlobals
      rm(list=ls(name=env), pos=env)
    } # close unregister function
    #
    cl <- parallel::makeCluster(cores)
    doParallel::registerDoParallel(cores = cl)
    #
  } # Close parallel == TRUE
  # 
  # Register a sequential backend
  if(parallel == FALSE) {
    foreach::registerDoSEQ()
  } # Close parallel == FALSE
  # 
  getDATA <- foreach::foreach(temp = listURL,
                              .combine = dplyr::bind_rows,
                              .multicombine = TRUE,
                              .inorder = TRUE,
                              .maxcombine = 1000,
                              .packages = c("httr", "xml2", "dplyr")) %dopar% {
                                #
                                x <- temp %>%
                                  httr::content(as = "parsed",
                                                type = "text/xml",
                                                isURL = T, # Check if these options matter
                                                options = HUGE,
                                                useInternalNodes =  T) %>%
                                  xml2::xml_children()
                                var.names <- tolower(xml2::xml_name(xml2::xml_children(x[[1]])))
                                n.col <- length(var.names)
                                # 
                                x <-
                                  x %>%
                                  xml2::xml_text() %>%
                                  stringr::str_replace_all("\\n", "\t") %>%
                                  stringr::str_replace_all(" +", "") %>%
                                  read.table(text = .,
                                             sep = "\t",
                                             na.strings = c("-9.0000",
                                                            "-9.000",
                                                            "-9.00",
                                                            "-9.0",
                                                            "-9"),
                                             stringsAsFactors = FALSE)
                                # 
                                x <- data.frame(x[, c(2:(ncol(x) - 1))])
                                colnames(x) <- var.names
                                # 
                                if(toupper(record) == "HH") {
                                  x$timeshot <- as.character(x$timeshot)
                                  i <- nchar(x$timeshot) == 3
                                  if(any(i)) x$timeshot[i] <- paste0("0", x$timeshot[i])
                                } ## HH timeshot
                                return(x)
                              } # close %dopar%
  if(parallel == TRUE){
    parallel::stopCluster(cl = cl)
    unregister()
  } # close parallel == TRUE
  # 
  parseFinish <- proc.time()
  # 
  if(time) {
    cat("\nDownload time: \n")
    print(downloadFinish - downloadStart)
    cat("\n Parse time: \n")
    print(parseFinish - downloadFinish)
    cat("\n Total time: \n")
    print(parseFinish - downloadStart)
  }
  #
  return(getDATA)
} # close getDATRAS function
