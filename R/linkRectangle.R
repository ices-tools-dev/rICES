#' linkRect.R

#' Link ICES rectangles with ICES areas
#' @param surveyName, startYear, endYear, startQuarter, endQuarter, path = "~/", inParallel
#' @return none
#' @seealso \code{\link{cacheDATRAS}}, a\code{\link{loadDATRAS}}, \code{\link{getCAfun}},
#' \code{\link{getHLfun}}, and \code{\link{getHHfun}}
#' @details the update is slow, avoiding straining the server or client.
#'   please allow this call to run overnight for a complete upgrade.
#' @keywords download, DATRAS, survey\
#' @examples \dontrun{
#'  getDATRAS(surveyName = "NS-IBTS", startYear = 2005, endYear = 2006, startQuarter = 1,
#'  endQuarter = 4, path = ".", inParallel = TRUE)
#' }
#' @export

#########################################################


# Function to extract all areaNames for all ICES areas
getAreaName <- function(areaList) {
  areaName <- data.frame()
  for(i in 1:length(areaList)){
    getArea <- aData[aData$ICES_area == areaList[i],]
    getRect <- rData[getArea,]

    areaName.i <- data.frame("ICES_AREA" = areaList[i],
                             "ICES_NAME" = getRect$ICESNAME)
    areaName <- rbind(areaName, areaName.i)
  }
  return(areaName)
}
allAreaName <- getAreaName(unique(aData$ICES_area))
save(allAreaName, file = "ICESareaName_v001.Rdata")
