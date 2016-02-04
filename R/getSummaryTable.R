#' getStockSummary.R

#' ICES standard graph data extraction
#' @param year
#' @return none
#' @seealso none
#' @details none
#' @keywords none
#' @examples \dontrun{
#' tt <- getSummaryTable()
#' }
#' @export
#
#
getSummaryTable <- function(year = 2014) {
  # If you want all stocks for all years, then make year == 0
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
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#"
  #
  cat(dataPolicy)
  answer <- toupper(readline(prompt = "I accept the ICES Data Policy: "))
  accepted <- ifelse(answer %in% c("YES", "NO"), "true", "false")
  #
  if(answer == "YES") {
    keys <- data.table(t(xmlSApply(xmlRoot(xmlTreeParse(paste0("http://standardgraphs.ices.dk/StandardGraphsWebServices.asmx/getListStocks?year=",
                                                               year),
                                                        isURL = T,
                                                        options = HUGE,
                                                        useInternalNodes =  T)),
                                   function(x) xmlSApply(x, xmlValue))))
    #
    refList <- paste0("http://standardgraphs.ices.dk/StandardGraphsWebServices.asmx/getFishStockReferencePoints?key=",
                      unique(keys$key))
    #
    allRefs <- data.frame()
    # i = 63
    for(i in 1:length(refList)) { # Loop over all reference points tables and extract data
      refNames.i <-  xmlRoot(xmlTreeParse(refList[i], isURL = T))
      refDat <- xmlSApply(refNames.i[["FishSettingsList"]], xmlValue)
      refDat[sapply(refDat, function(x) length(x) == 0)] <- NA
      refDat <- data.frame(t(refDat))
      refDat$FMSY_type <- colnames(refDat[8])
      refDat$MSYBtrigger_type <- colnames(refDat[9])
      colnames(refDat)[c(8,9)] <- c("FMSY", "MSYBtrigger")
      allRefs <- rbind(allRefs, refDat)
    } # Close i loop
    #
    # Clean up data
    numColsRefs <- colnames(allRefs)[!colnames(allRefs) %in% c("FishStockName")]
    allRefs[, numColsRefs] <- sapply(allRefs[, numColsRefs], function(x) as.numeric(x))
    allRefs[, c("FishStockName")] <- sapply(allRefs[, c("FishStockName")], function(x) as.character(x))
    #
    summaryList <- paste0("http://standardgraphs.ices.dk/StandardGraphsWebServices.asmx/getSummaryTable?key=",
                          unique(keys$key))
    #
    summaryDat <- data.frame()
    for(j in 1:length(summaryList)) { # Loop over all published summary tables and extract data
      summaryNames <-  xmlRoot(xmlTreeParse(summaryList[j], isURL = T))
      # Parse XML data and convert into a data frame
      xmlDat <- xmlSApply(summaryNames[["lines"]], function(x) xmlSApply(x, xmlValue))
      xmlDat[sapply(xmlDat, function(x) length(x) == 0)] <- NA
      dimnames(xmlDat)[2] <- NULL
      summaryInfo <- data.frame(t(xmlDat))
      #
      stockList <- names(summaryNames[names(summaryNames) != "lines"])
      stockValue <-  rbind(lapply(stockList, function(x) xmlSApply(summaryNames[[x]], xmlValue)))
      stockValue[sapply(stockValue, function(x) length(x) == 0)] <- NA
      colnames(stockValue) <- stockList
      #
      summaryInfo <- cbind(summaryInfo, stockValue)
      #
      if(any(colnames(summaryDat) %in% colnames(summaryInfo) |
               any(colnames(summaryInfo) %in% colnames(summaryDat)))) {
        newDat <- colnames(summaryInfo)[!colnames(summaryInfo) %in% colnames(summaryDat)]
        summaryDat[,newDat] <- NA
        newInfo <- colnames(summaryDat)[!colnames(summaryDat) %in% colnames(summaryInfo)]
        summaryInfo[,newInfo] <- NA
      }
      summaryDat <- rbind(summaryDat, summaryInfo)
    } # close j loop
    #
    # Clean up data
    numCols <- colnames(summaryDat)[!colnames(summaryDat) %in% c("fishstock", "units", "Fage")]
    summaryDat[, numCols] <- lapply(summaryDat[, numCols], function(x) as.numeric(x))
    summaryDat[, c("fishstock", "units", "Fage")] <- lapply(summaryDat[, c("fishstock", "units", "Fage")], function(x) as.character(x))
    #
    # Create new list with all summary tables and reference points
    newList <- list("summaryTable" = summaryDat, "referencePoints" = allRefs, "keys" = keys)
    #
    return(newList)
  } # Close answer == YES if statement
  if(answer != "YES") {
    cat("To download data, you must read the ICES Data Policy and type 'yes' into the console window.")
  } # Close answer != YES if statement
} # Close function
