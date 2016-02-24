#' getSummaryTable.R

#' ICES standard graph data extraction
#' @param year The year of summary data to be extracted
#' @return none
#' @seealso none
#' @details none
#' @keywords none
#' @examples \dontrun{
#' d <- getSummaryTable()
#' }
#' @export
#
#
getSummaryTable <- function(year = 2015) {
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
    keys <- data.frame(t(XML::xmlSApply(XML::xmlRoot(XML::xmlTreeParse(paste0("http://standardgraphs.ices.dk/StandardGraphsWebServices.asmx/getListStocks?year=",
                                                               year),
                                                        isURL = T,
                                                        options = HUGE,
                                                        useInternalNodes =  T)),
                                   function(x) XML::xmlSApply(x, XML::xmlValue))), row.names = NULL)
    #
    keys$Status <- gsub("[[:space:]]", "",  keys$Status)
    #
    colnames(keys)[colnames(keys) == "FishStockName"] <- "STOCKID"
    refList <- paste0("http://standardgraphs.ices.dk/StandardGraphsWebServices.asmx/getFishStockReferencePoints?key=",
                      unique(keys$key[keys$Status == "Published"]))
    #
    allRefs <- data.frame()
    for(i in 1:length(refList)) { # Loop over all reference points tables and extract data
      refNames.i <-  XML::xmlRoot(XML::xmlTreeParse(refList[i], isURL = T))
      refDat <- XML::xmlSApply(refNames.i[["FishSettingsList"]], XML::xmlValue)
      refDat[sapply(refDat, function(x) length(x) == 0)] <- NA
      allRefs <- plyr::rbind.fill(allRefs, data.frame(t(refDat)))
    } # Close i loop
    #
    # Clean up data
    allRefs <- data.frame(sapply(allRefs, function(x) ifelse(x == "NULL", NA, x)))
    numColsRefs <- colnames(allRefs)[!colnames(allRefs) %in% c("FishStockName")]
    allRefs[, numColsRefs] <- sapply(allRefs[, numColsRefs], function(x) as.numeric(x))
    allRefs[, c("FishStockName")] <- sapply(allRefs[, c("FishStockName")], function(x) as.character(x))
    colnames(allRefs)[colnames(allRefs) == "FishStockName"] <- "STOCKID"
    #
    summaryList <- data.frame(key = unique(keys$key[keys$Status == "Published"]),
                              URL = paste0("http://standardgraphs.ices.dk/StandardGraphsWebServices.asmx/getSummaryTable?key=",
                                           unique(keys$key[keys$Status == "Published"])))
    #
    summaryDat <- data.frame()
    for(j in 1:nrow(summaryList)) { # Loop over all published summary tables and extract data
      summaryNames <-  XML::xmlRoot(XML::xmlTreeParse(summaryList$URL[j], isURL = T))
      # Parse XML data and convert into a data frame
      xmlDat <- XML::xmlSApply(summaryNames[["lines"]], function(x) XML::xmlSApply(x, XML::xmlValue))
      xmlDat[sapply(xmlDat, function(x) length(x) == 0)] <- NA
      dimnames(xmlDat)[2] <- NULL
      summaryInfo <- data.frame(lapply(data.frame(t(xmlDat)), function(x) as.numeric(x)))
      #
      stockList <- names(summaryNames[names(summaryNames) != "lines"])
      stockValue <-  rbind(lapply(stockList, function(x) XML::xmlSApply(summaryNames[[x]], XML::xmlValue)))
      stockValue[sapply(stockValue, function(x) length(x) == 0)] <- NA
      dimnames(stockValue)[2] <- NULL
      stockValue <- data.frame(lapply(stockValue, function(x) as.character(x)), stringsAsFactors = F)
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
      summaryInfo$key <- as.numeric(as.character(summaryList$key[j]))
      summaryDat <- rbind(summaryDat, summaryInfo)
    } # close j loop
    #
    # Clean up data
    charCols <- c("fishstock", "units", "Fage", "stockSizeDescription", 
                  "stockSizeUnits", "fishingPressureDescription", "fishingPressureUnits", "StockPublishNote")
    numCols <- colnames(summaryDat)[!colnames(summaryDat) %in% charCols]
    summaryDat[, numCols] <- lapply(summaryDat[, numCols], function(x) as.numeric(x))
    summaryDat[, charCols] <- lapply(summaryDat[, charCols], function(x) as.character(x))
    colnames(summaryDat)[colnames(summaryDat) == "fishstock"] <- "STOCKID"
    #
    # Create new list with all summary tables and reference points
    allRefs$STOCKID <- tolower(as.character(allRefs$STOCKID))
    summaryDat$STOCKID <- tolower(as.character(summaryDat$STOCKID))
    keys$STOCKID <- tolower(as.character(keys$STOCKID))  
    #
    sTable <- merge(allRefs, keys, c("STOCKID", "AssessmentYear", "key"))
    sTable <- merge(summaryDat, sTable, by = c("STOCKID", "AssessmentYear","key"))
    #
    # To facilitate adding guild information
    sTable$speciesID <- tolower(gsub( "-.*$", "", as.character(sTable$STOCKID)))
    #   
    return(sTable)
  } # Close answer == YES if statement
  if(answer != "YES") {
    cat("To download data, you must read the ICES Data Policy and type 'yes' into the console window.")
  } # Close answer != YES if statement
} # Close function
