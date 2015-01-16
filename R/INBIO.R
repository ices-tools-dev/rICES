# rm(list = ls())
#
# library(taxize)
# library(taxizesoap)
# # library(ices)
# library(doParallel)
# library(foreach)
# library(parallel)
# library(XML)
# # library(ggplot2)
#
# source("~/git/ices/R/getCAfun.R")
# source("~/git/ices/R/getHHfun.R")
# source("~/git/ices/R/getHLfun.R")
# source("~/git/ices/R/getDATRAS.R")
#
# # getDATRAS(survey = "NS-IBTS",
# #           startYear = 1965,
# #           endYear = 2014,
# #           startQuarter = 1,
# #           endQuarter = 4,
# #           dataDir = "~/git/ices/data",
# #           inParallel = T)
#
# # setwd("D:/Profiles/Scott/My Documents/git/ices/R")
# # sourceDir <- "D:/Profiles/Scott/My Documents/git/ices/"
#
# dataDir <- "~/git/ices/data"
# load("D:/Profiles/Scott/My Documents/git/ices/data/allDATRAS_1965-2014.Rdata")
#
# unique(CArecs$Year)
# unique(HLrecs$Year)
# unique(HHrecs$Year)
# #
# wormsID <- unique(HLrecs$Valid_Aphia[toupper(HLrecs$SpecCodeType)=="W"])
# wormsID <- wormsID[!is.na(wormsID)]
#
# wormsID <- data.frame(wormsID = wormsID,
#                       scientificName = worms_name(wormsID))
#
# tsnID <- unique(HLrecs$SpecCode[toupper(HLrecs$SpecCodeType)=="T"])
# tsnID <- tsnID[!is.na(tsnID)]
#
# tsnID <- data.frame(tsnID = tsnID,
#                     scientificName = sapply(tsnID, function(x) itis_getrecord(x)$scientificName$combinedName))
#
# tt <- merge(wormsID, tsnID, c("scientificName"), all = T)
#
# tsnMiss <- tt[is.na(tt$tsnID),]
# wormMiss <- tt[is.na(tt$wormsID),]
#
# tsnMiss$check <- ifelse(is.na(get_tsn(tsnMiss$scientificName, ask = FALSE, accepted = FALSE)), "NP", "P")
# tsnMiss$tsnID[tsnMiss$check == "P"] <- sapply(tsnMiss$scientificName[tsnMiss$check == "P"],
#                                                function(x) as.numeric(itis_getrecord(get_tsn(x))$acceptedNameList$tsn))
#
#
# wormMiss$check <- ifelse(is.na(get_wormsid(wormMiss$scientificName, ask = F)), "NP", "P")
# wormMiss$wormsID[wormMiss$check == "P"] <- sapply(wormMiss$scientificName[wormMiss$check == "P"],
#                                                    function(x) as.numeric(worms_records(get_wormsid(x))$inputid))
#
#
# tn <- merge(tsnMiss[,c(1:3)], wormMiss[,c(1:3)], by = c("scientificName", "wormsID", "tsnID"), all = T)
#
# tsnMiss2 <- tn[is.na(tn$tsnID),]
# wormMiss2 <- tn[is.na(tn$wormsID),]
#
# tts <- merge(tn, tt, by = c("scientificName", "wormsID", "tsnID"), all = T)
# tsnMiss <- tt[is.na(tt$tsnID),]
# wormMiss <- tt[is.na(tt$wormsID),]
#
# head(HLrecs)
#
# unique(HLrecs$Year[HLrecs$SpecCodeType == "T"])
# unique(HLrecs$Year[HLrecs$SpecCodeType %in% c("T", "W")])
# unique(HLrecs$Quarter)
# head(tt)
#
# tsnMiss$tsnID <- tsnMiss$tsnID2
# wormMiss$wormsID <- wormMiss$wormID2
#
#
#
# tn <- merge(tsnMiss, wormMiss, c("scientificName", "wormsID", "tsnID"), all = T)
