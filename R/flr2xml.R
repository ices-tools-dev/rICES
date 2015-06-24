#' flr2xml.R

#' Translate FLCore "FLRobject" into xml template ready for ICES Stock Assessment Graphs
#' @param AssessmentYear
#'   icesID
#'   Fage
#'   RecruitmentAge
#'   outputDirectory
#'   FLim = NULL
#'   Fpa = NULL
#'   Bpa = NULL
#'   Blim = NULL
#'   FMSY = NULL
#'   MSYBtrigger = NULL
#'   Fmanagement = NULL
#'   Bmanagement  = NULL
#'   RecruitmentLength = NULL
#'   Custom = FALSE
#'   CustomName = FALSE
#' @return none
#' @seealso none
#' @details none
#' @keywords none
#' @examples \dontrun{
#'  needList <- c("XML", "RCurl", "data.table", "FLCore")
#'  new.packages <- needList[!(needList %in% installed.packages()[,"Package"])]
#'  if("FLCore" %in% new.packages) {
#'   install.packages("FLCore", repos="http://flr-project.org/R")
#'   new.packages <- new.packages[new.packages != "FLCore"]
#'  }
#'  if(length(new.packages)) install.packages(new.packages)
#'  lapply(needList, require, c = T)
#'
#'  urls <- ("https://community.ices.dk/ExpertGroups/WGMIXFISH-NS/2015%20Meeting%20docs/06.%20Data/FLStock%20objects/North%20Sea/standardised%20RData/PLE-NS.RData")
#'  tmpFile <- tempfile()
#'  download.file(urls, destfile = tmpFile, mode = "wb")
#'  load(tmpFile)
#'  tt <- load(tmpFile)
#'  assign(x = "FLRobject", value = get(tt))
#'
#'  flr2xml(FLRobject = FLRobject, AssessmentYear = 2015,
#'    icesID = "ple-nsea", Fage = c(2:6),
#'    Fmanagement = 0.3 , Bmanagement = 230000,
#'    RecruitmentAge = 1, catchType = "ICES Estimates",
#'    outputDirectory = "~/Data Products/FLR2XML/")
#'
#' }
#' @export
#

###############
# Script Info #
###############
# PURPOSE: Translate FLCore "FLRobject" into xml template ready for ICES Stock Assessment Graphs
# AUTHOR: Scott Large 2015
# REVIEWED BY:
# VERSION: 0.1
#
######################
# CHANGES/ ADDITIONS #
######################
# Need to add:
#~#  make a check for the ICES stock codes
## edit:
#~#  FLRxml$addNode("UnitsWeigths", value = "tonnes") # Contacted Carlos about misspelling
#~#  FLRxml$addNode("UnitsRecruits", value = "thousands")
#
## add options:
#~#  if(Custom == T) provide a data.frame or vector
#~#  CustomName = string the ncols of Custom df
#~#  FLRxml$addNode("TypeLandings", value = catchType)
#~#  FLRxml$addNode("CustomName1", value = catchType)
#~#  FLRxml$addNode("CustomName2", value = catchType)
#~#  FLRxml$addNode("CustomName3", value = catchType)
#~#  FLRxml$addNode("CustomName4", value = catchType)
#~#  FLRxml$addNode("CustomName5", value = catchType)
#~#  FLRxml$addNode("VersionStock", value = catchType)
#~#  FLRxml$addNode("NameSystemProducedFile", value = catchType)
#~#  FLRxml$addNode("FishData", value = catchType)
## add from FLR:
#~#  FLRxml$addNode("Sensitivity_Data", value = catchType)
#
# Change Log:
#
#
flr2xml <- function(FLRobject, AssessmentYear, icesID, Fage, RecruitmentAge,
                    catchType = c("official", "ICES estimates", "Model estimates"),
                    outputDirectory = "~/",
                    FLim = NULL, Fpa = NULL, Bpa = NULL, Blim = NULL,
                    FMSY = NULL, MSYBtrigger = NULL, Fmanagement = NULL,
                    Bmanagement  = NULL, RecruitmentLength = NULL,
                    Custom = FALSE, CustomName = FALSE, ...) {
#   AssessmentYear <- 2015
#   icesID <- "ple-nsea"
#   Fage <- c(2:6)
#   RecruitmentAge <- 1
#   outputDirectory <- "~/Data Products/FLR2XML/"
#   FLim = NULL
#   Fpa = NULL
#   Bpa = NULL
#   Blim = NULL
#   FMSY = NULL
#   MSYBtrigger = NULL
#   Fmanagement = NULL
#   Bmanagement  = NULL
#   RecruitmentLength = NULL
#   Custom = FALSE
#   CustomName = FALSE
  #
  if(class(FLRobject) != "FLStock") stop("FLRobject must be a FLStock Object from FLCore...")
  #
  icesID <- tolower(icesID)
  #
#~# if(Custom)...
#~# if(CustomName)...
  #
  # Identify Stock Assessment Graphs "key" to identify previous year's Reference Table
  keys <- data.table(t(xmlSApply(xmlRoot(xmlTreeParse(paste0("http://standardgraphs.ices.dk/StandardGraphsWebServices.asmx/getListStocks?year=",
                                                             AssessmentYear - 1),
                                                      isURL = T,
                                                      options = HUGE,
                                                      useInternalNodes =  T)),
                                 function(x) xmlSApply(x, xmlValue))))
  #
#~#
#   Verify ICES Stock Code
#   if(nrow(keys[keys$FishStockName == icesID & keys$Status == " Published ",]) == 0 ) {
#     stop("type 'binary' is not supported on this platform")
#   }
#~#
  #
  # Get published Reference Table from previous year (i.e., AssessmentYear - 1)
  stockKey <- paste0("http://standardgraphs.ices.dk/StandardGraphsWebServices.asmx/getFishStockReferencePoints?key=",
                     keys$key[keys$FishStockName == icesID &
                     keys$Status ==" Published "])
  #
  # Get previous reference points
  refNames <-  xmlRoot(xmlTreeParse(stockKey, isURL = T))
  #
  # Modify previous reference points (if provided)
  if(is.null(FLim)) {
    flim <- ifelse(length(xmlValue(refNames[["FishSettingsList"]][["FLim"]])),
                   xmlValue(refNames[["FishSettingsList"]][["FLim"]]),
                   "")
  } else {
    flim <- FLim
  }
  if(is.null(Fpa)) {
    fpa <- ifelse(length(xmlValue(refNames[["FishSettingsList"]][["Fpa"]])),
                  xmlValue(refNames[["FishSettingsList"]][["Fpa"]]),
                  "")
  } else {
    fpa <- Fpa
  }
  if(is.null(Bpa)) {
    bpa <- ifelse(length(xmlValue(refNames[["FishSettingsList"]][["Bpa"]])),
                  xmlValue(refNames[["FishSettingsList"]][["Bpa"]]),
                  "")
  } else {
    bpa <- Bpa
  }
  if(is.null(Blim)) {
    blim <- ifelse(length(xmlValue(refNames[["FishSettingsList"]][["Blim"]])),
                   xmlValue(refNames[["FishSettingsList"]][["Blim"]]),
                   "")
  } else {
    blim <- Blim
  }
  if(is.null(FMSY)) {
    fmsy <- ifelse(length(xmlValue(refNames[["FishSettingsList"]][["FMSY"]])),
                   xmlValue(refNames[["FishSettingsList"]][["FMSY"]]),
                   "")
  } else {
    fmsy <- FMSY
  }
  if(is.null(MSYBtrigger)) {
    msybtrigger <- ifelse(length(xmlValue(refNames[["FishSettingsList"]][["MSYBtrigger"]])),
                          xmlValue(refNames[["FishSettingsList"]][["MSYBtrigger"]]),
                          "")
  } else {
    msybtrigger <- MSYBtrigger
  }
  if(is.null(Fmanagement)) {
    fmanagement <- ifelse(length(xmlValue(refNames[["FishSettingsList"]][["Fmanagement"]])) > 0,
                          xmlValue(refNames[["FishSettingsList"]][["Fmanagement"]]),
                          "")
  } else {
    fmanagement <- Fmanagement
  }
  if(is.null(Bmanagement)) {
    bmanagement <- ifelse(length(xmlValue(refNames[["FishSettingsList"]][["Bmanagement"]])),
                          xmlValue(refNames[["FishSettingsList"]][["Bmanagement"]]),
                          "")
  } else {
    bmanagement <- Bmanagement
  }
  if(is.null(RecruitmentAge)) {
    recruitmentage <- ifelse(length(xmlValue(refNames[["FishSettingsList"]][["RecruitmentAge"]])),
                             xmlValue(refNames[["FishSettingsList"]][["RecruitmentAge"]]),
                             "")
  } else {
    recruitmentage <- RecruitmentAge
  }
  if(is.null(RecruitmentLength)) {
    recruitmentlength <- ifelse(length(xmlValue(refNames[["FishSettingsList"]][["RecruitmentLength"]])),
                                xmlValue(refNames[["FishSettingsList"]][["RecruitmentLength"]]),
                                0)
  } else {
    recruitmentlength <- RecruitmentLength
  }
  #
  #
#~# Can remove?
#
#  summaryList <- paste0("http://standardgraphs.ices.dk/StandardGraphsWebServices.asmx/getSummaryTable?key=",
#                        keys$key[keys$FishStockName == icesID &
#                                   keys$Status ==" Published "])
#  summaryNames <-  xmlRoot(xmlTreeParse(summaryList, isURL = T))
#
#~#
  #
  FishDataDF <- data.frame(Year = seq(slot(FLRobject, "range")["minyear"], slot(FLRobject, "range")["maxyear"]),
                      Recruitment = an(stock.n(FLRobject)[RecruitmentAge,]),
                      High_Recruitment = NA,
                      Low_Recruitment = NA,
                      Low_SSB = NA,
                      SSB = an(ssb(FLRobject)),
                      High_SSB = NA,
                      Catches = an(catch(FLRobject)),
                      Landings = an(landings(FLRobject)),
                      Discards = an(discards(FLRobject)),
                      Low_F = NA,
                      F = an(fbar(FLRobject)),
                      F_Landings = an(quantMeans(((landings.n(FLRobject)/catch.n(FLRobject)) * harvest(FLRobject))[as.character(Fage),])),
                      F_Discards =  an(quantMeans(((discards.n(FLRobject)/catch.n(FLRobject)) * harvest(FLRobject))[as.character(Fage),])),
                      high_F = NA)
  #
#~# Add these
# Low_TBiomass TBiomass High_TBiomass IBC Unallocated_Removals YieldSSB F_IBC F_Unallocated SoP
# Custom1 Custom2 Custom3 Custom4 Custom5
#~#
  #
#~# Sensitivity Data
#   Sensitivity_Data <- data.frame(Age = ,
#                                  M = ,   # Mortality
#                                  Mat = , # Maturity
#                                  PF = ,
#                                  PM = ,
#                                  WeSt = , # Weight at age in the stock
#                                  F = ,
#                                  WeCa = ,
#                                  Fd = ,
#                                  WeCad = ,
#                                  Fi = ,
#                                  WeCai = )
#~#
  #
#
# Age  M  Mat	WeSt	F	        WeCa	Fd	WeCad	Assessment_Id
# 1	  0.1	0	  0.048	0.003043	0.207	0.22322917	0.025	0
# 2	  0.1	0.5	0.104	0.0050133	0.252	0.17927048	0.089	0
# 3	  0.1	0.5	0.158	0.066679	0.285	0.19057395	0.132	0
# 4	  0.1	1	  0.202	0.10379	  0.318	0.07363649	0.162	0
# 5	  0.1	1	  0.312	0.12988	  0.368	0.04756366	0.18	0
# 6	  0.1	1	  0.38	0.093546	0.418	0.01113261	0.212	0
# 7	  0.1	1	  0.439	0.083648	0.479	0.00308646	0.3	  0
# 8	  0.1	1	  0.484	0.061454	0.543	0.00049091	0.37	0
# 9	  0.1	1	  0.458	0.029205	0.628	0	          0.255	0
# 10	0.1	1	  0.615	0.029205	0.65	0	          0	    0
# 11	0.1	1	  0.501	0.029205	0.65	0         	0	    0
# 12	0.1	1	  0.729	0.029205	0.65	0         	0	    0
# 13	0.1	1	  0.673	0.029205	0.65	0          	0	    0
# 14	0.1	1	  0.513	0.029205	0.65	0	          0	    0
# 15	0.1	1	  1.058	0.029205	0.65	0
# str(mat(FLRobject)@.Data$year)

  # Create new Assessment node
  FLRxml <- xmlTree("Assessment",
                    namespaces = c('xsi' = 'http://www.w3.org/2001/XMLSchema-instance'),
                    attrs = c('xsi:noNamespaceSchemaLocation' = 'ICES_Standard_Graphs.xsd'),
                    doc = newXMLDoc())

  # <Assessment xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'
  # xsi:noNamespaceSchemaLocation='ICES_Standard_Graphs.xsd'>
  #
  # Add Reference Table nodes
  FLRxml$addNode("FishStock", value = icesID)
  FLRxml$addNode("AssessmentYear", value = AssessmentYear)
  FLRxml$addNode("Flim", value = flim)
  FLRxml$addNode("Fpa", value = fpa)
  FLRxml$addNode("Blim", value = blim)
  FLRxml$addNode("Bpa", value = bpa)
  FLRxml$addNode("FMSY", value = fmsy)
  FLRxml$addNode("MSYBtrigger", value = msybtrigger)
  FLRxml$addNode("Fmanagement", value = fmanagement)
  FLRxml$addNode("Bmanagement", value = bmanagement)
  FLRxml$addNode("RecruitmentAge", value = RecruitmentAge)
  FLRxml$addNode("FAge", value = ifelse(length(Fage) > 1, paste0(min(Fage), "-", max(Fage)), Fage) )
  FLRxml$addNode("RecruitmentLength", value = recruitmentlength)
  FLRxml$addNode("UnitsWeigths", value = "tonnes") # Contacted Carlos about misspelling
  FLRxml$addNode("UnitsRecruits", value = "thousands")
#   FLRxml$addNode("TypeLandings", value = catchType)
#   FLRxml$addNode("CustomName1", value = catchType)
#   FLRxml$addNode("CustomName2", value = catchType)
#   FLRxml$addNode("CustomName3", value = catchType)
#   FLRxml$addNode("CustomName4", value = catchType)
#   FLRxml$addNode("CustomName5", value = catchType)
#   FLRxml$addNode("VersionStock", value = catchType)
#   FLRxml$addNode("NameSystemProducedFile", value = catchType)
#   FLRxml$addNode("FishData", value = catchType)
#   FLRxml$addNode("Sensitivity_Data", value = catchType)
  #
  # Add FishData nodes
  # Remove empty columns from FishDataDF and convert remaining empty nodes into ""
  FishDataDF <- FishDataDF[apply(FishDataDF, 2, function(x) all(!is.na(x)))]
  FishDataDF[is.na(FishDataDF)] <- ""
  #
  for(i in 1:nrow(FishDataDF)) {
    FLRxml$addNode("FishData", close=FALSE)
    for(j in names(FishDataDF)) {
      FLRxml$addNode(j, FishDataDF[i, j])
    } # Close j loop
    FLRxml$closeTag()
  } # Close i loop
  #
  #
#~# Add Sensitivity_DataDF
#   for(k in 1:nrow(Sensitivity_DataDF)) {
#     FLRxml$addNode("Sensitivity_Data", close=FALSE)
#     for(l in names(Sensitivity_DataDF)) {
#       FLRxml$addNode(l, Sensitivity_DataDF[k, l])
#     } # Close k loop
#     FLRxml$closeTag()
#   } # Close l loop
#~#
  #
  # Save XML and export.
  saveXML(doc = FLRxml,
          indent = TRUE,
          file =  paste0(outputDirectory, icesID, "TESTv1.xml"),
          prefix = "<?xml version='1.0' encoding='utf-8' standalone='no'?>\n<?xml-stylesheet type='text/xsl' href='StandrdGraphsStyle.xsl'?>\n")
#
#~# Consider a text or plot output to verify the data
#
}
#
#


# refDat <- xmlSApply(refNames[["FishSettingsList"]], xmlValue)
# refDat[sapply(refDat, function(x) length(x) == 0)] <- NA
# data.frame(t(refDat))



