#' ncApply
#'
#' @description applies a function to a netCDF object
#' 
#'  
#' @param  data          target object. function is applied to each row.
#' @param  cellIDs       which cellIDs to use? A numeric vector or "all"
#' @param  dates         a POSIXlt vector of dates
#' @param  returnSpatial if TRUE, a joined spdf is returned. If FALSE, a dataframe is returned           type of output desired. can be either "spdf" or "df"
#' @param  spdf          the spdf to join
#' @param  yearBegin     first month of year
#' @param  yearlength    length of "year" (units = months)
#' @param  func          function to apply to each year and each cell
#' 
#'
#' @return output is a vector with the value returned by "func" applied to each year
#'
#' @importFrom zoo    as.yearmon
#' @importFrom parallel makeCluster
#' @importFrom parallel stopCluster
#' @importFrom parallel detectCores
#' @importFrom parallel parApply
#' @importFrom sp merge
#' @importFrom sp spplot
#' 
#' @export
#' 



ncApply <- function(data, # = chead,    # function applied to each row in dataframe. units = cm w.r.t. sediment surface. 
                     ### this function takes input functions that handle multiple arguments
                     cellIDs = "all", # which cellIDs to use? A numeric vector or "all"
                     dates, # = dateVec, # must be posixlt?
                     returnSpatial = FALSE, # if TRUE, a joined spdf is returned. If FALSE, a dataframe is returned
                     spdf = NULL, # the spdf to join
                     yearBegin = 5, yearlength = 12, # first month and length of time period of interest
                     func) { # = func_contHP, ...) {
  # output is a vector with the longest continuous inundation period for each year
  # threshold should be in the same units as depth data
  # time units are time units in data - days per year
  
  ### reduce cells considered
  if(is.numeric(cellIDs)) {
    data <- data[cellIDs, ]
  } else {
    cellIDs <- 1:nrow(data) # needed for returnSpatial = TRUE
  }
  
  ### if year used isn't calendar year, adjust dates. Note: dates$mo ranges from 0 - 11
  ### could also move this outside the function
  if (yearBegin == 0) {
    dateOffset <- 0 # this ensures that dateOffset is zero when calendar years are sought
  } else {
    dateOffset <- (yearBegin - 1)/12 # units = fractional years. verified that this results in correct offset.
  }
  # library(zoo)
  dates2     <- as.POSIXlt(zoo::as.yearmon(dates) + dateOffset) # days are irrelevant after this - all days set to 1st of month.
  ### TODO: remove any years <12 months
  # table(dates2)
  ### dates are now "water year" style - year 2005 starts in may 2004, ends in april 2005
  
  # print(expand_ellipsis(...))
  
  # library(parallel)
  cl <- parallel::makeCluster(parallel::detectCores())
  # parallel::clusterExport(cl=cl, varlist= names(expand_ellipsis(...)), envir = environment())# .GlobalEnv)
  
  for (i in 1:length(unique(dates2$year))) {
    
    ### parallelized version
    yrVals <- parallel::parApply(cl = cl, MARGIN = 1,
                                 X = data[, dates2$year == unique(dates2$year)[i]], # export "threshold", "output"
                                 FUN = func) #, ...)
    
    if(i > 1) {
      returnDat <- cbind(returnDat, yrVals)
    } else {
      returnDat <- yrVals
    }
  }
  parallel::stopCluster(cl)
  returnDat        <- as.data.frame(returnDat)
  names(returnDat) <- paste0("yr", (1899 + unique(dates2$year)[1]) + 1:length(unique(dates2$year))) # I go back and forth about whether it's best to use dates (calendar year) or dates2 (WY/transformed year). 
  
  if ((returnSpatial == TRUE) && !is.null(spdf)) {
    yrDat <- cbind(cellIDs, returnDat)
    names(yrDat)[1] <- "CellId"
    ### merge data with spdf
    returnDat <- sp::merge(spdf, yrDat, by = 'CellId', duplicateGeoms = TRUE)
    print(sp::spplot(returnDat, zcol = names(yrDat)[2])) # an example plot
  }
  
  invisible(returnDat)
}
