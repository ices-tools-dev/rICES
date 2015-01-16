#' getDATRAS
#'
#' Download files from DATRAS
#' @param surveyName, startYear, endYear, startQuarter, endQuarter, dataDir = "~/", inParallel
#' @return none
#' @export

getDATRAS <- function(surveyName, startYear, endYear, startQuarter, endQuarter, dataDir = "~/", inParallel = FALSE) {
#   if(paste0("allDATRAS_", startYear, "-", endYear, ".Rdata") %in% list.files(dataDir)) {
#     cat("Hold on!!! \n \n",
#         paste0("allDATRAS_", startYear, "-", endYear, ".Rdata"),
#         " is located within ",
#         dataDir,
#         "\n Try load(",
#         paste0("allDATRAS_", startYear, "-", endYear, ".Rdata"),
#         ") to continue.")
#
#   } else {
    # Reading the data can take some time, so the process is split and saved into 5 yr blocks in case there is a problem
    # with a connection and the process must be restarted. In that case, change the startYear accordingly.
    hhFile <- list()
    hlFile <- list()
    caFile <- list()
    #
    if(startYear < 1965) stop("startYear must be greater than 1965.")
    START <- startYear
    #
    while(START <= endYear){
      END <- ifelse(START + 5 <= endYear, START + 5, (endYear - START) + START)
      cat("Extracting from ", START, "-", END, "\n")
      #
      tHH <- getHHfun(survey = surveyName,
                      startyear = START,
                      endyear = END,
                      startquarter = startQuarter,
                      endquarter = endQuarter,
                      parallel = inParallel)
#       hhPath <- paste0("HH_", START, "-", END)
      hhPath <- paste0(dataDir, "/HH_", START, "-", END, ".Rdata")
      save(tHH, file = hhPath)
      rm(tHH)
      gc(verbose = TRUE)
#       assign(hhPath, tHH)
      hhFile <- rbind(hhFile, hhPath)
      #
      tHL <- getHLfun(survey = surveyName,
                      startyear = START,
                      endyear = END,
                      startquarter = startQuarter,
                      endquarter = endQuarter,
                      parallel = inParallel)
#       hlPath <-  paste0("HL_", START, "-", END)
      hlPath <-  paste0(dataDir, "/HL_", START, "-", END, ".Rdata")
      save(tHL, file = hlPath)
      rm(tHL)
      gc(verbose = TRUE)
#       assign(hlPath, tHL)
      hlFile <- rbind(hlFile, hlPath)
      #
      tCA <- getCAfun(survey = surveyName,
                      startyear = START,
                      endyear = END,
                      startquarter = startQuarter,
                      endquarter = endQuarter,
                      parallel = inParallel)
#       caPath <- paste0("CA_", START, "-", END)
      caPath <- paste0(dataDir, "/CA_", START, "-", END, ".Rdata")
#       assign(caPath, tCA)
      save(tCA, file = caPath)
      rm(tCA)
      gc(verbose = TRUE)
      caFile <- rbind(caFile, caPath)
      #
      START <- END + 1
    }
    # Combine 5 year blocks into full length data.frames and return objects
    HHrecs <- do.call(rbind, lapply(hhFile, function(x) get(load(x))))
    HLrecs <- do.call(rbind, lapply(hlFile, function(x) get(load(x))))
    CArecs <- do.call(rbind, lapply(caFile, function(x) get(load(x))))

    save(HHrecs, HLrecs, CArecs, file = paste0(dataDir, "/allDATRAS_", startYear, "-", endYear, ".Rdata"))
#     return(list(HHrecs = HHrecs, HLrecs = HLrecs, CArecs = CArecs))
#   }
}

