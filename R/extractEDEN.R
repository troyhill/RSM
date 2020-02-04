#' extractSimData
#'
#' @description This function extracts data and aggregates Ever4Cast simulation data (contained in a list of netCDFs) for spatial locations of interest. The output is a list of four permutations of the same data. This function can take a very long time depending on the number of regions-of-interest and their size (e.g., 2 hours for ~50 Combined Operations Plan indicator regions). NOTE: This is based on availability of data on the home page of EDEN. If data aren't there, they aren't accessible for this code.
#' 
#' @param targetLocations      locations to extract data from; must be class SpatialPointsDataFrame or SpatialPolygonsDataFrame
#' @param targetLocationNames  option to specify the name of target locations (e.g., pts$gage)
#' @param dateRange            Date range for which to download EDEN data
#' @param needEDEN             whether EDEN data is needed. either TRUE, or the name of the list of EDEN data generated from lapply(dateRange, fireHydro::getEDEN). 
#'
#' @return a dataframe formatted identically to the traceDataLong output from extractSimData()
#'
#' @importFrom fireHydro getEDEN
#' @importFrom sf st_intersection
#' @importFrom sf st_set_crs
#' @importFrom sf st_as_sf
#' @importFrom sf st_crs
#' 
#' @examples 
#' 
#' dateList <- c(20200101:20200123)
#' newDat   <- getEDENbyROI(targetLocations = pts,
#'                          dateRange = dateList)
#' 
#' @export
#' 



getEDENbyROI <- function(targetLocations,
                         targetLocationNames,
                         dateRange,
                         needEDEN = TRUE # either TRUE, or the name of the list of EDEN data generated from lapply(dateRange, fireHydro::getEDEN). 
) {
  if (typeof(needEDEN) == logical && needEDEN) {
    test <- lapply(dateRange, fireHydro::getEDEN)
  } else if(is.list(needEDEN)) {
    test <- needEDEN
  } else {
    stop("needEDEN argument not recognized. It should be either 'TRUE' (signaling that EDEN data should be pulled) or a list of EDEN data.")
  }
  
  IR.means <- sapply(test, function(x) {
    x.sf  <- x$data
    a.sub <- sf::st_intersection(x.sf, sf::st_set_crs(sf::st_as_sf(targetLocations), sf::st_crs(x.sf)))
    mean(a.sub$WaterDepth, na.rm = TRUE)
  })
  # beepr::beep()
  dat <- data.frame(cluster  = "observed",
                    name     = targetLocationNames,
                    date     = as.Date(as.character(sapply(test, "[[", 1)), format = "%Y%m%d"),
                    ave      = IR.means, 
                    stdev    = 0)
  invisible(dat)
}