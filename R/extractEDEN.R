#' getEDENbyROI
#'
#' @description This function extracts EDEN data for spatial locations of interest. This function can take a very long time depending on the number of regions-of-interest and their size, and if EDEN data is downloaded within the function call. NOTE: This is based on availability of data on the home page of EDEN. If data aren't there, they aren't accessible for this code.
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
  if (length(needEDEN) == 1 && typeof(needEDEN) == logical && needEDEN) {
    test <- lapply(dateRange, fireHydro::getEDEN)
  } else if(is.list(needEDEN)) {
    test <- needEDEN
  } else {
    stop("needEDEN argument not recognized. It should be either 'TRUE' (signaling that EDEN data should be pulled) or a list of EDEN data.")
  }
  for (i in 1:length(targetLocations)) {
    IR.tmp <- sapply(test, function(x) {
      x.sf  <- x$data
      a.sub <- sf::st_intersection(x.sf, sf::st_set_crs(sf::st_as_sf(targetLocations[i, ]), sf::st_crs(x.sf)))
      mean(a.sub$WaterDepth, na.rm = TRUE)
    })
    dat.tmp <- data.frame(cluster  = "observed",
                          name     = targetLocationNames[i],
                          date     = as.Date(as.character(sapply(test, "[[", 1)), format = "%Y%m%d"),
                          ave      = IR.tmp, 
                          stdev    = 0)
    if (i == 1) {
      dat <- dat.tmp
    } else {
      dat <- rbind(dat, dat.tmp)
    }
  }
  # IR.means <- sapply(test, function(x) {
  #   x.sf  <- x$data
  #   a.sub <- sf::st_intersection(x.sf, sf::st_set_crs(sf::st_as_sf(targetLocations), sf::st_crs(x.sf)))
  #   mean(a.sub$WaterDepth, na.rm = TRUE)
  # })
  # # beepr::beep()
  # dat <- data.frame(cluster  = "observed",
  #                   name     = targetLocationNames,
  #                   date     = as.Date(as.character(sapply(test, "[[", 1)), format = "%Y%m%d"),
  #                   ave      = IR.means, 
  #                   stdev    = 0)
  invisible(dat)
}
