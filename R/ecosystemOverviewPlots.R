rm(list = ls())
################
# library(rICES)
library(plyr)
library(reshape2)
library(RColorBrewer)
library(extrafont)
library(XML)
# library(parallel)
# library(foreach)
# library(data.table)
plotDir = "output/"
# This may take a minute to get the correct font
fontTable <- fonttable()
colList <- brewer.pal(n = 9, name = 'Set1')
ltyList <- c(1,3:6)
#
if(!"Calibri" %in% fontTable$FamilyName) font_import(pattern="[C/c]alibri", prompt = FALSE)
#
setwd("~/git/ices-dk/rStockOverview")
# Load data on ecosystem, books, guilds, etc.
load("allDataPlotOverview_v001.rdat")
#
# Load most recent data from Stock Assessment Graphs database
source("getTestStockSummary.R")
stockTable <- getTestSummaryTable(year = 2015)
# 
stockTable <- merge(stockTable, 
                    stockInfo[,c("speciesID", "Type")],
                    by = c("speciesID"), all.x = F, all.y = F)
stockTable <- stockTable[!duplicated(stockTable),]
# 
# Clean up the fishing pressure descriptions and add a column describing the type of Fmsy
stockTable$fishingPressureDescription <- gsub("Fishing Pressure: " , "", stockTable$fishingPressureDescription)
stockTable$fishingPressureDescription <- gsub("Fishing pressure: " , "", stockTable$fishingPressureDescription)
stockTable$FmsyDescription <- "FMSY"
stockTable$FmsyDescription[!is.na(stockTable$Fcap)] <- "Fcap"
stockTable$FMSY[stockTable$FmsyDescription == "Fcap"] <- stockTable$Fcap[stockTable$FmsyDescription == "Fcap"]
# Clean up the stock size descriptions and add a column describing the type of MSYBtrigger
stockTable$stockSizeDescription[stockTable$stockSizeDescription == "NA"] <- "Stock Size: Relative"
stockTable$stockSizeDescription <- gsub("Stock Size: ", "", stockTable$stockSizeDescription)
stockTable$BmsyDescription <- "MSYBtrigger"
stockTable$BmsyDescription[!is.na(stockTable$MSYBescapement)] <- "MSYBescapement"
stockTable$MSYBtrigger[stockTable$BmsyDescription == "MSYBescapement"] <- stockTable$MSYBescapement[stockTable$BmsyDescription == "MSYBescapement"]
# Drop relative stocks
stockTable <- stockTable[stockTable$stockSizeDescription != "Relative" |
                         stockTable$fishingPressureDescription != "Relative",]

# To make the SSB/MSYBtrigger comparison, B/BMSY stocks have already been calculated, so for B/BMSY stocks MSYBtrigger == 1
stockTable$MSYBtrigger[stockTable$stockSizeDescription == "B/BMSY"] <- 1
# 
df <- melt(stockTable, 
           id.vars = c("AssessmentYear", "EcoRegion", "Type", "STOCKID", "Year"),
           measure.vars = c("F", "SSB", "FMSY", "MSYBtrigger"),
           variable.name = "METRIC",
           value.name = "VALUE")
#
colnames(df) <- c("ASSESSMENTYEAR", "ECOREGION", "GUILD", "STOCKID", "YEAR", "METRIC", "VALUE")
#
# Clean up MSYBtrigger == 0
df$VALUE[df$VALUE == 0 &
           df$METRIC == "MSYBtrigger"] <- NA
# 
df$VALUE[df$VALUE == 0 &
           df$METRIC == "FMSY"] <- NA


# unique(df$ECOREGION)
ecoregion = "Barents Sea and Norwegian Sea"
guildAvg = T 
guild = "All guilds"

sapply(unique(df$ECOREGION), plotMSY, dat = df, guildAvg = TRUE, plotDir = plotDir, summaryOutput = FALSE, VERSION = 6)
#
# summaryOut <- ddply(df, .(ECOREGION, GUILD), summarize,
#                     out = unique(STOCKID))
# 
# write.csv(summaryOut, file= paste0(plotDir, "plotSummary2.csv"))



plotMSY <- function(dat, ecoregion, guildAvg, plotDir, summaryOutput = FALSE, VERSION = 6) {
  ############
  # PLOT msy #
  ############
  #   
  FmsyDat <- dat[dat$METRIC %in% c("F", "FMSY"),]
  # 
  FmsyDat <- dcast(FmsyDat, ASSESSMENTYEAR + ECOREGION + GUILD + STOCKID + YEAR ~ METRIC, value.var = "VALUE")
  FmsyDat$F.Fmsy <- FmsyDat$F / FmsyDat$FMSY
  FmsyDat <- melt(FmsyDat, id.vars = c("ASSESSMENTYEAR", "ECOREGION", "GUILD", "STOCKID", "YEAR"),
                  measure.vars = c("F", "FMSY", "F.Fmsy"),
                  variable.name = "METRIC",
                  value.name = "VALUE")
  Fmsy <- FmsyDat[FmsyDat$METRIC == "F.Fmsy",]
  FmsyNA <- sapply(unique(Fmsy$STOCKID), function(x) all(is.na(Fmsy$VALUE[Fmsy$STOCKID == x])))
  Fmsy <- Fmsy[Fmsy$STOCKID %in% names(FmsyNA[FmsyNA == "FALSE"]),]
  FmsyStockID <- unique(Fmsy$STOCKID[Fmsy$STOCKID %in% names(FmsyNA[FmsyNA == "FALSE"])])
  #
  BmsyDat <- dat[dat$METRIC %in% c("SSB", "MSYBtrigger"),]
  BmsyDat <- dcast(BmsyDat, ASSESSMENTYEAR + ECOREGION + GUILD + STOCKID + YEAR ~ METRIC, value.var = "VALUE")
  BmsyDat$SSB.Btrigger <- BmsyDat$SSB / BmsyDat$MSYBtrigger
  BmsyDat <- melt(BmsyDat, id.vars = c("ASSESSMENTYEAR", "ECOREGION", "GUILD", "STOCKID", "YEAR"),
                  measure.vars = c("SSB", "MSYBtrigger", "SSB.Btrigger"),
                  variable.name = "METRIC",
                  value.name = "VALUE")
  Bmsy <- BmsyDat[BmsyDat$METRIC == "SSB.Btrigger",]
  BmsyNA <- sapply(unique(Bmsy$STOCKID), function(x) all(is.na(Bmsy$VALUE[Bmsy$STOCKID == x])))
  Bmsy <- Bmsy[Bmsy$STOCKID %in% names(BmsyNA[BmsyNA == "FALSE"]),]
  #
  msyDat <- rbind(Fmsy, Bmsy)
  msyDat <- msyDat[,c("YEAR","ECOREGION", "STOCKID", "GUILD", "VALUE", "METRIC")]
  #   
  ## TO MAKE THE SUMMARY TABLE ##
  rj <-  ddply(msyDat, .(ECOREGION, GUILD,STOCKID, METRIC, YEAR), summarize,
               out = VALUE)
  sumOut <- data.frame()
  for(i in unique(rj$STOCKID)) {
    rj.i <-  rj[rj$STOCKID == i &
                complete.cases(rj),]
    # 
    if(nrow(rj.i[rj.i$METRIC == "F.Fmsy",]) == 0){
      fMax <- data.frame(rj.i[1,1:4], 
                         F.Year = NA, 
                         F.Fmsy = NA)
    # 
      } else {
    fMax <- rj.i[rj.i$METRIC == "F.Fmsy",]
    fMax <- fMax[fMax$YEAR == max(fMax$YEAR, na.rm = T),]
    colnames(fMax)[5:6] <- c("F.Year", "F.Fmsy")
    }
    #
    if(nrow(rj.i[rj.i$METRIC == "SSB.Btrigger",]) == 0){
      bMax <- data.frame(rj.i[1,1:4],
                         SSB.Year = NA, 
                         SSB.Btrigger = NA)
    } else {
      bMax <- rj.i[rj.i$METRIC == "SSB.Btrigger",]
      bMax <- bMax[bMax$YEAR == max(bMax$YEAR, na.rm = T),]
      colnames(bMax)[5:6] <- c("SSB.Year", "SSB.Btrigger")
      }
    sumOut <- rbind(sumOut, cbind(fMax[-4], bMax[5:6]))
    }
  #
  if(summaryOutput == TRUE) {
    write.csv(sumOut, file = paste0(plotDir, "plotSummary_v", VERSION, ".csv"), row.names = F)
    cat("Summary saved as: ", paste0(plotDir, "plotSummary_v", VERSION, ".csv"))
  }
  # 
  msyDat <- msyDat[msyDat$ECOREGION == ecoregion,]
  #   
  if(nrow(msyDat) > 0) {
    # 
    if(guildAvg == T){
      overallMSY <- ddply(msyDat, .(METRIC, YEAR), summarize,
                          VALUE = mean(VALUE, na.rm = T),
                          STOCKID = "MEAN")
      
      guildMSY <- ddply(msyDat, .(METRIC, YEAR, GUILD), summarize,
                        VALUE = mean(VALUE, na.rm = T))
      colnames(guildMSY)[colnames(guildMSY) == "GUILD"] <- "STOCKID"
      msyDat <- rbind(overallMSY, guildMSY)
    #
    } else {      
      overallMSY <- ddply(msyDat, .(METRIC, YEAR), summarize,
                          VALUE = mean(VALUE, na.rm = T),
                          STOCKID = "MEAN",
                          GUILD = "NA")
      msyDat <- rbind(overallMSY, msyDat)
    }
    #
    stocks.msy <- data.frame("CODE" = unique(msyDat$STOCKID),
                             "COLOR" = c("grey40", colList[1:length(unique(msyDat$STOCKID)) -1]),
                             "LWD" = c(4, rep(2, length(unique(msyDat$STOCKID)) -1)))
    # 
    stocks.msy <- lapply(stocks.msy, as.character)
    #
    METRICList_MSY <- c("F.Fmsy", "SSB.Btrigger")
    # 
    METRICFactor_MSY <- factor(unique(msyDat$METRIC[!is.na(msyDat$VALUE)]),
                               levels = c("F.Fmsy", "SSB.Btrigger"))
    #
    METRICList_MSY <- METRICList_MSY[METRICList_MSY %in% METRICFactor_MSY]
    #
    xRange_MSY <- range(msyDat$YEAR, na.rm = TRUE)
    # 
    png(filename = paste0(plotDir, ecoregion, "_", guild, "_MSY_v", VERSION, ".png"),
        width = 172.4,
        height = 81.3 * length(METRICList_MSY),
        units = "mm",
        res = 600)
    #
    par(mfrow = c(length(METRICList_MSY), 1),
        mar=c(2.15, 2.25, 0.45, 0.25),
        oma = c(0, 0, 0, 0),
        usr = c(0, 1, 0, 1),
        mgp=c(3, .35, 0),
        tck=-0.01,
        family = "Calibri")
 # k = 1
    for(k in 1:length(METRICList_MSY)) {
      #             met <- levels(metricList_MSY)
      #             if(met[k] == "F") {
      #               par(mfg = c(1,1))
      #             }
      #             if(met[k] == "SSB") {
      #               par(mfg = c(2,1))
      #             }
      ## PLOT F Avg ##
      msyDat.k <- msyDat[msyDat$METRIC == METRICList_MSY[k],]
      # Identify plotting parameters
      yRange.k <- c(0, max(msyDat.k$VALUE, na.rm =T) + max(msyDat.k$VALUE, na.rm = T) * .15)
#       xRange.k <- c(min(msyDat.k$YEAR[!is.na(msyDat.k$VALUE)]),
#                     max(msyDat.k$YEAR[!is.na(msyDat.k$VALUE)]))
      stocks.k <- relevel(factor(unique(msyDat.k$STOCKID), ordered = F),
                          ref = "MEAN")
      stocks.msy.k <- stocks.msy
      #
      if(length(stocks.k) >= 10) {
        #               if(nrow(stocks.msy) >= 10) {
        levels(stocks.msy.k$COLOR) <- c(levels(stocks.msy.k$COLOR), "grey80")
        stocks.msy.k$COLOR[stocks.msy.k$CODE != "MEAN"] <- "grey80"
        #                         }
        stocks.k <- rev(levels(stocks.k))
      }
      if(length(stocks.k) < 10) {
        stocks.k <- levels(stocks.k)
      }
      if(length(stocks.k) <= 2) {
        stocks.k <- stocks.k[stocks.k != "MEAN"]
        msyDat.k <- msyDat.k[msyDat.k$STOCKID %in% stocks.k,]
      }
      #
      lab.k <- METRICList_MSY[k]
      #
      plot(NA,
           type = "l",
           ylim = yRange.k,
           xlim = xRange_MSY,
           yaxt = "n",
           xaxt = "n",
           ann = FALSE)
      abline(h = 1.0, lty = 2, col = "black", lwd = 1)
      #
      for(l in 1:length(stocks.k)) {
        if(all(is.na(msyDat.k$VALUE[msyDat.k$STOCKID == stocks.k[l]]))) {
          stocks.k[l] <- NA
          next
        }
        if(!all(is.na(msyDat.k$VALUE[msyDat.k$STOCKID == stocks.k[l]]))) {
          d <- data.frame(msyDat.k$VALUE[msyDat.k$STOCKID == stocks.k[l]],
                          msyDat.k$YEAR[msyDat.k$STOCKID == stocks.k[l]])
          d <- d[order(d[,2]),]
          col.d <- as.character(stocks.msy.k$COLOR[stocks.msy.k$CODE == stocks.k[l]])
          lin.d <- as.numeric(as.character(stocks.msy.k$LWD[stocks.msy.k$CODE == stocks.k[l]]))
          lines(d[,2], d[,1], col = col.d, lwd = lin.d)
        }
      }
      axis(1, at = pretty(xRange_MSY), cex.axis = .85)
      #
      mtext("Year", side = 1, line = 1.25, cex= 1)
      #
      axis(2, at = pretty(yRange.k), cex.axis = .75, las = 1)
      #
      if(lab.k == "F.Fmsy") {
        mtext(expression("F/F"["MSY"]), side = 2, line = 1, cex= 1)
      }
      if(lab.k == "SSB.Btrigger") {
        mtext(expression("SSB/MSY B"["trigger"]), side = 2, line = 1, cex= 1)
      }
      # Legend
      if(length(stocks.k) <= 9) {
        legend("topright",
               legend = stocks.msy.k$CODE[stocks.msy.k$CODE %in% stocks.k],
               fill = stocks.msy.k$COLOR[stocks.msy.k$CODE %in% stocks.k],
               bty = "n",
               ncol = 3,
               cex = .85)
      }
      #
      if(length(stocks.k) >= 10) {
        legend("topright",
               legend = "MEAN",
               fill = "grey40",
               bty = "n",
               ncol = 1,
               cex = .85)
      }
      #           if(logo == T) {
      #             logoFun(img, x = 0.5, y = 0.5, size = .5, alpha = alpha)
      #           }
    } # Close l loop
    #       mtext(paste0(ecoregion, ", ", guild), side = 3, outer = T, cex= 1.5, font = 2)
    dev.off()
  } # Close Fmsy and Btrigger "if" statement
} # Close plotMSY function

# Close no guild data "if" statement
#
# } 
