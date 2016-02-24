#' readSpatial.R
#' Download and read data from the ICES Spatial Facility
#' @param dataset, ...
#' @return none
#' @seealso none
#' @details Reads ICES Statistical areas, ICES Statistical Rectangles,ICES Ecoregions, HELCOM sub-basins, or OSPAR Regions as 'rgdal' SpatialPolygonsDataFrame objects.
#'   Note: You will be prompted to read and agree to the ICES Data Policy before the download will begin.
#' @keywords download, spatial
#' @examples \dontrun{
#'  readSpatial(dataset = "ices_areas")
#' }
#' @export
readSpatial <- function(dataset = c("ices_areas", "ices_rectangles",
                                    "ices_ecoregions", "helcom_subbasins",
                                    "ospar_regions"), ...) {
  #library(rgdal)
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
    if(!dataset %in% c("ices_areas", "ices_rectangles",
                       "ices_ecoregions", "helcom_subbasins",
                       "ospar_regions")){
      #
      cat("Please choose a valid dataset: \n ices_areas,\n ices_rectangles,\n ices_ecoregions,\n helcom_subbasins, or \n ospar_regions)")
      #
    } else {    
      refs <- ifelse(dataset %in% c("ices_areas", "ices_rectangles",
                                    "ices_ecoregions"), "ices_ref", "ext_ref")
      dataset[dataset == "ospar_regions"] <- "ospar_regions_without_coastline"
      urls <- paste0("http://geo.ices.dk/download.php?dataset=", refs, ":", dataset, # Identify the dataset
                     "&accepted=", accepted)
      # Create temporary storage and download .zip file
      tmpDir <- tempdir()
      tmpFile <- tempfile(fileext = ".zip", tmpdir = tmpDir)
      download.file(urls, destfile = tmpFile, mode = "wb")
      unzip(zipfile = tmpFile, exdir = tmpDir)
      # read file as SpatialPolygonsDataFrame with "rgdal"
      areaDat <- readOGR(dsn = tmpDir, layer = dataset, ...)
      return(areaDat)
    }  # Close else dataset statement
  } # Close answer == YES if statement
  if(answer != "YES") {
    cat("To download data, you must read the ICES Data Policy and type 'yes' into the console window.")
  } # Close answer != YES if statement
}