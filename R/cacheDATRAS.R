# cacheDATRAS.R

#' Update the cached copy of DATRAS data
#' @param path where cache should be stored. (default to working directory)
#' @return a date-database.Rdat file.
#' @seealso \code{\link{loadDATRAS}}, and \code{\link{getDATRAS}}
#' @details the update is slow, avoiding straining the server or client.
#'   please allow this call to run overnight for a complete upgrade.
#' @keywords cache
#' @examples \dontrun{
#'  updateCache(path = ".", surveyName = "NS-IBTS",
#'  startYear = 2005, endYear = 2010, startQuarter = 1, endQuarter = 4, inParallel = FALSE)
#'  loadCache()
#' }
#' @export

######################################################
updateCache <- function(path = ".",
                        surveyName,
                        startYear,
                        endYear,
                        startQuarter,
                        endQuarter,
                        inParallel = FALSE){
  date <- Sys.Date()
  #ifelse other parameters that get sent to getDATRAS
  endYear <- format(date, "%Y")
#
  survey.data <- getDATRAS(surveyName,
                         startYear,
                         endYear,
                         startQuarter,
                         endQuarter,
              #           path = path,
                         inParallel = TRUE)
  file <-  paste0(path, "/",
                  date, "-",
                  surveyName, ".Rdat")
  save(list = "survey.data",
       file = file)
#   save(list="fish.data", file=file)
}

# loadDATRAS.R

#' Load an updated cache
#' @param path location where cache is located
#' @return loads the DATRAS survey object into the working space.
#' @seealso \code{\link{cacheDATRAS}} and \code{\link{getDATRAS}}
#' @keywords cache
#' @examples \dontrun{
#'  catchDATRAS()
#'  loadDATRAS()
#' }
#' @export

###################################################
loadCache <- function(path=NULL, surveyName){
  if(is.null(path)) {
    surveyData <- paste0(surveyName, ".rda")
    file <- system.file("data", surveyData, package = "ices")
  } else {
    # load the most recent file from the cache
    files <- list.files(path)
    copies <- grep(paste0(surveyName, ".Rdat"), files)
    most_recent <- files[copies[length(copies)]]
    file <- paste(path, "/", most_recent, sep="")
  }
#   load(file, envir = icesCache)
#   survey.data <- get("survey.data", envir = icesCache)
  load(file)
  survey.data <- get("survey.data")
  #
  HLrecs <- survey.data$HLrecs
  HHrecs <- survey.data$HHrecs
  CArecs <- survey.data$CArecs
  #
  HLrecs
  HHrecs
  CArecs
}


