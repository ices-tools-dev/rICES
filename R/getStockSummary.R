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
  for(i in 1:length(refList)) { # Loop over all reference points tables and extract data
    refNames.i <-  xmlRoot(xmlTreeParse(refList[i], isURL = T))
    refDat <- xmlSApply(refNames.i[["FishSettingsList"]], xmlValue)
    refDat[sapply(refDat, function(x) length(x) == 0)] <- NA
    allRefs <- rbind(allRefs, data.frame(t(refDat)))
  } # Close i loop
  #
  # Clean up data
  numColsRefs <- colnames(allRefs)[!colnames(allRefs) %in% c("FishStockName")]
  allRefs[, numColsRefs] <- sapply(allRefs[, numColsRefs], function(x) as.numeric(x))
  allRefs[, c("FishStockName")] <- sapply(allRefs[, c("FishStockName")], function(x) as.character(x))
  #
  summaryList <- paste0("http://standardgraphs.ices.dk/StandardGraphsWebServices.asmx/getSummaryTable?key=",
                        keys$key[keys$Status == " Published " &
                                   !keys$key %in% c(5411)])
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
} # Close function
