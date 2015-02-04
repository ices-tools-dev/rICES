# rm(list = ls())

library(data.table)
# library(nlme)
# library(plyr)
# load("./HH-26012015_NS-IBTS.Rdat")
# load("./HL-26012015_NS-IBTS.Rdat")
#
# data <- HH
# ###################################
# # Clean up raw data from getHHfun #
# ###################################
# # Remove extra columns
# data[, ":=" (V1 = NULL)]
# # Remove extra spaces from character strings
# cnames <- colnames(data)
# for(cname in cnames) set(data, j = cname, value = gsub("[[:space:]]", "", data[[cname]]))
# # Change columns to numeric
# numCols <- c("Quarter", "SweepLngt", "HaulNo", "Year", "month", "Day", "TimeShot", "Stratum",
#              "HaulDur", "ShootLat", "ShootLong", "HaulLat", "HaulLong", "Depth",
#              "StdSpecRecCode", "Netopening", "Rigging", "Tickler", "Distance",
#              "Warplngt", "Warpdia", "DoorSurface", "DoorWgt", "DoorSpread", "WingSpread",
#              "Buoyancy", "KiteDim", "WgtGroundRope", "TowDir", "GroundSpeed",
#              "SpeedWater", "SurCurDir", "SurCurSpeed", "BotCurDir",  "BotCurSpeed",
#              "WindDir", "WindSpeed",  "SwellDir",  "SwellHeight",	"SurTemp",	"BotTemp",
#              "SurSal", "BotSal")
# data[, (numCols) := lapply(.SD, as.numeric), .SDcols = numCols]
# # Change -9 values to NA
# for(cname in cnames) {
#   set(data, i = which(data[[cname]] == -9), j = cname, value = NA)
# }
# # Select only valid hauls
# hauls <- data[data$HaulVal=='V',]
# ########################
# # Calculate swept area #
# ########################
# #
# # Linear model: WingSpread ~ a * log(Depth)) + b
# wingLM <- lm(hauls$WingSpread~I(log(hauls$Depth)), na.action = na.exclude)
# # WingSpread by Ship, Year, Quarter, and StatRec
# hauls[, WingSpreadRect := mean(WingSpread, na.rm = T),
#               by = .(Ship, Year, Quarter, StatRec)]
# # WingSpread lm by depth
# hauls[, WingSpreadLM := coef(wingLM)[2] * log(Depth) + coef(wingLM)[1]]
# # Median WingSpread
# hauls[, WingSpreadMed := median(WingSpread, na.rm = T)]
# #
# # Use mean WingSpreads to fill in NA, in decreasing order of precision
# hauls[is.na(WingSpread), WingSpread := WingSpreadRect]
# hauls[is.na(WingSpread), WingSpread := WingSpreadLM]
# hauls[is.na(WingSpread), WingSpread := WingSpreadMed]
# #
# hauls[, GroundSpeed := GroundSpeed * 1852 / 60]
# # Mean GroundSpeed for Ship, Year, and Quarter
# hauls[, speedest := mean(GroundSpeed, na.rm = T),
#               by = .(Ship, Year, Quarter)]
# # Mean GroundSpeed for Ship and Quarter
# hauls[, speedvessel := mean(GroundSpeed, na.rm = T),
#                by = .(Ship, Quarter)]
# # Mean GroundSpeed for Year and Quarter
# hauls[, speednoship := mean(GroundSpeed, na.rm = T),
#                by = .(Year, Quarter)]
# # Mean GroundSpeed for Quarter
# hauls[, speednoshipnoyear := mean(GroundSpeed, na.rm = T),
#                by = .(Quarter)]
# # Calculate GroundSpeed when Distance and HaulDur are available
# hauls[!is.na(Distance) & !is.na(HaulDur) & is.na(GroundSpeed), GroundSpeed := Distance / HaulDur]
# # Use mean GroundSpeeds to fill in the rest, in decreasing order of precision
# hauls[is.na(GroundSpeed), GroundSpeed := speedest]
# hauls[is.na(GroundSpeed), GroundSpeed := speedvessel]
# hauls[is.na(GroundSpeed), GroundSpeed := speednoship]
# hauls[is.na(GroundSpeed), GroundSpeed := speednoshipnoyear]
# # Calculate Distance with HaulDur and GroundSpeed
# hauls[is.na(Distance), Distance := HaulDur * GroundSpeed]
# # Swept Area
# hauls[, GearSwept := Distance * DoorSpread]
# hauls[, NetSwept := Distance * WingSpread]
#
# ######################################################################
# #
# # keyCols <- c("Year","Quarter","Ship","Gear","StNo","HaulNo")
# # colnames(HH)
# # setkeyv(HH, keyCols)
# # setkeyv(HL, keyCols)
# #
