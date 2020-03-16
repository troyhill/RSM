#' getEDENbyROI
#'
#' @description This function extracts EDEN data for spatial locations of interest. This function can take a very long time depending on the number of regions-of-interest and their size, and if EDEN data is downloaded within the function call. NOTE: This is based on availability of data on the home page of EDEN. If data aren't there, they aren't accessible for this code.
#' 
#' @param targetLocations      locations to extract data from; must be class SpatialPointsDataFrame or SpatialPolygonsDataFrame
#' @param targetLocationNames  option to specify the name of target locations (e.g., pts$gage)
#' @param dateRange            Date range to download EDEN data. Must be in \%Y\%m\%d (e.g., "20201231") format. This can be a character vector.
#' @param needEDEN             whether EDEN data is needed. either TRUE, or the name of the list of EDEN data generated from lapply(dateRange, fireHydro::getEDEN). Specifically, object is a list of lists with (1) a date (YYYYMMDD) and (2) EDEN data (sf multipolygon object). 
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
#' \dontrun{
#' load("L:/Restoration Assessments/Non CERP Projects/COP/Position_analysis/data/data_indicatorRegions_20200204.RData")
#' 
#' IR_pattern <- "WCA-1"
#' dateList <- format(seq.Date(from = as.Date("20200101", format = "%Y%m%d"), 
#'             to = as.Date("20200109", format = "%Y%m%d"), by = "day"), format = "%Y%m%d")
#' 
#' ### assign cluster categories to trace data
#' sim_means <- polyDat$traceDataLong[!is.na(polyDat$traceDataLong$value), ] 
#' ### add grouping variable 
#' sim_means$cluster <- sim_clusters[match(sim_means$simulation, sim_clusters$simulation), 1]
#' 
#' ROIs <- IRMap[[2]][grepl(IRMap[[2]]$NAME, pattern = IR_pattern), ]
#' targetLocationNames <- as.character(IRMap[[2]]$NAME[grepl(IRMap[[2]]$NAME, 
#'                         pattern = IR_pattern)])
#' 
#' ### best practice: pull EDEN data separately  so it's retained in the global environment
#' EDEN_data <- lapply(dateList, fireHydro::getEDEN)
#' EDEN_by_IR <- getEDENbyROI(targetLocations = ROIs,
#'                            targetLocationNames = targetLocationNames,
#'                            dateRange = dateList, 
#'                            needEDEN = EDEN_data)
#' t.plot <- tracePlot(namesMatching = IR_pattern, simMeansObject = sim_means)
#' 
#' t.plot
#' 
#' t.plot + geom_line(data = EDEN_by_IR, mapping = aes(x = date, y = ave), size = 1.5)
#' 
#' ### Because EDEN data are now present in the working environment, 
#' ### it's trivial to repeat this for a new region of interest
#' IR_pattern <- "WCA-3A North Central"
#' ROIs <- IRMap[[2]][grepl(IRMap[[2]]$NAME, pattern = IR_pattern), ]
#' targetLocationNames <- as.character(IRMap[[2]]$NAME[grepl(IRMap[[2]]$NAME, 
#'                                     pattern = IR_pattern)])
#' 
#' EDEN_by_IR <- getEDENbyROI(targetLocations = ROIs,
#'               targetLocationNames = targetLocationNames,
#'               dateRange = dateList, 
#'               needEDEN = EDEN_data)
#'               
#' tracePlot(namesMatching = IR_pattern, simMeansObject = sim_means) + 
#'    geom_line(data = EDEN_by_IR, mapping = aes(x = date, y = ave), size = 1.5)
#' }
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
