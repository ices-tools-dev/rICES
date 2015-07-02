#' icesLogo.R

#' Adds ICES logo to a base R plot
#' @param logoType = c("acronym", "fullText"), x, y, size, alpha = 1
#' @return none
#' @seealso none
#' @details none
#' @keywords plot
#' @examples \dontrun{
#'  needList <- c("png")
#'  new.packages <- needList[!(needList %in% installed.packages()[,"Package"])]
#'  if(length(new.packages)) install.packages(new.packages)
#'  library(png)
#'
#'  width <- 172.4
#'  height <- 162.6
#'  rows <- 2
#'  cols <- 1
#'  png(filename = "~/TESTlogo.png",
#'      width = width,
#'      height = height,
#'      units = "mm",
#'      res = 600)
#'
#'  par(mar = c(2.15, 2.25, 0.45, 0.25),
#'      mfrow = c(rows, cols),
#'      oma = c(0, 0, 1.25, 0),
#'      mgp = c(3, .35, 0),
#'      tck = -0.01,
#'      family = "Calibri")
#'
#'  plot(1:2,type="n")
#'  icesLogo(logoType = "acronym", x = 0.1, y = 0.9, size = .25, alpha = 1)
#'  plot(1:2,type="n")
#'  icesLogo(logoType = "fullText", x = 0.5, y = 0.5, size = 1, alpha = .1)
#'  dev.off()
#'  
#'
#' # devtools::install_github("ices-dk/rICES")
#' library(rICES)
#' library(extrafont)
#' library(png)
#' library(RCurl)
#' 
#' icesOrange <- "#F15D2A"
#' 
#' png(filename = "~/git/ices-dk/rICES/rICESlogo.png",
#'     width = 25.4,
#'     height = 25.4,
#'     units = "mm",
#'     res = 600)
#
#' par(mar = c(0, 0, 0, 0),
#'     oma = c(0, 0, 0, 0),
#'     family = "Calibri", # requires library(extrafont), but matches ICES Advice format
#'     mgp = c(3, .35, 0))
#' 
#' plot(x = 0.55, y = 0.5, type="p", pch = "R", cex = 9, 
#'      col = icesOrange, xlim = c(0,1), ylim = c(0,1),
#'      axes = F, bty = "n")
#' icesLogo(logoType = "acronym", x = 0.5, y = 0.5, size = .55, alpha = .9)
#' #
#' dev.off()
#' 
#' 
#'}
#' @export
#
icesLogo <- function(logoType = c("acronym", "fullText"), x, y, size, alpha = 1) {
  #
#   needList <- c("RCurl", "extrafont")
#   new.packages <- needList[!(needList %in% installed.packages()[,"Package"])]
#   if(length(new.packages)) install.packages(new.packages)
#   #
#   library(RCurl)
#   library(extrafont)
  #
#   fontTable <- fonttable()
#   if(!"Calibri" %in% fontTable$FamilyName) font_import(pattern="[C/c]alibri", prompt = FALSE)
#   #
  if(!logoType %in% c("acronym", "fullText")) stop("Must specify 'fullText' or 'acronym' logoType")
  if(logoType == "acronym") {
    imgURL <- "http://www.ices.dk/SiteCollectionImages/ICES%20logos/ICES-logo%20acronym%20PNG%20format.png"
  }
  if(logoType == "fullText") {imgURL <- "http://www.ices.dk/SiteCollectionImages/ICES%20logos/ICES-logo%20full%20text%20.PNG%20format.png"
  }
  imgICES <-  readPNG(getURLContent(imgURL))
  #
  logoFun(logo = imgICES, x = x, y = y, size = size, alpha = alpha)
}

logoFun <- function(logo, x, y, size, alpha = 1){
  #
  if(length(dev.list()) < 1 ) stop("Must have an active plotting device...")
  # Aspect ratio for the plot
  arSize <- dev.size("cm") / c(par()$mfrow[2], par()$mfrow[1])
  #
  arPlot <- data.frame(HEIGHT = 1,
                       WIDTH = 1)
  arPlot$WIDTH <- ifelse(arSize[1] > arSize[2],
                          arSize[2] / arSize[1],
                          1)
  arPlot$HEIGHT <- ifelse(arSize[2] > arSize[1],
                         arSize[1] / arSize[2],
                         1)
  #
  dims <- dim(logo)[1:2]
  arLogo <-   data.frame(HEIGHT = 1,
                         WIDTH = 1)
  # Aspect ratio for the logo
  arLogo$WIDTH <- ifelse(dim(logo)[1] > dim(logo)[2],
                         dim(logo)[2] / dim(logo)[1],
                         1)
  #
  arLogo$HEIGHT <- ifelse(dim(logo)[2] > dim(logo)[1],
                          dim(logo)[1] / dim(logo)[2],
                         1)
  #
  par(usr = c(0, 1, 0, 1))
  # Adjust alpha
  img <- matrix(rgb(logo[,,1],
                    logo[,,2],
                    logo[,,3],
                    logo[,,4] * alpha),
                nrow = dim(logo)[1])
  rasterImage(img,
              x - (arLogo$WIDTH * arPlot$WIDTH * size),
              y - (arLogo$HEIGHT * arPlot$HEIGHT * size),
              x + (arLogo$WIDTH * arPlot$WIDTH * size),
              y + (arLogo$HEIGHT * arPlot$HEIGHT * size),
              interpolate=TRUE)
} # Close function
