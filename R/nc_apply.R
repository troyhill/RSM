#' nc_apply
#'
#' @description applies a function to a netCDF object
#' 
#'  
#' @param  data          target object. function is applied to each row. If an `rsm` object is provided, several arguments are ignored: `cellIDs`, `dates`, `spdf`, `returnSpatial`
#' @param  cellIDs       which cellIDs to use? A numeric vector or "all"
#' @param  dates         a POSIXlt vector of dates
#' @param  returnSpatial if TRUE, a joined sf is returned. If FALSE, a dataframe is returned.
#' @param  spdf          the spatial object to join output to
#' @param  yearBegin     first month of year
#' @param  yearlength    length of "year" (units = months)
#' @param  includeMean    if TRUE, a column is included that averages across all non-ID columns in the dataset (this is typically an annual average)
#' @param  func          function to apply to each year and each cell
#' 
#'
#' @return output is a vector with the value returned by "func" applied to each year
#'
#'#' @examples
#' 
#' \dontrun{
#' ### default behavior is to return data from the most recent date
#' 
#' 
#' }
#' 
#' 
#' @importFrom zoo    as.yearmon
#' @importFrom parallel makeCluster
#' @importFrom parallel stopCluster
#' @importFrom parallel detectCores
#' @importFrom parallel parApply
#' @importFrom terra merge
#' 
#' @export
#' 

### TODO: have option to summarize by year, year-mo, etc. 

nc_apply <- function(data, #= chead.ecb,    # function applied to each row in dataframe. units = cm w.r.t. sediment surface. 
                       ### this function takes input functions that handle multiple arguments
                       cellIDs = "all", # which cellIDs to use? A numeric vector of index values to use, or "all"
                       dates   = 'all', # = dateVec, # must be posixlt?
                       returnSpatial = FALSE, # if TRUE, a joined spdf is returned. If FALSE, a dataframe is returned
                       spdf    = NULL, # the spdf to join
                       yearBegin     = 5, yearlength = 12, # first month and length of time period of interest
                       includeMean   = TRUE, # includes a column averaging annual values if TRUE
                       func    = mean, ...) {
    # output is a vector with the longest continuous inundation period for each year
    # threshold should be in the same units as depth data
    # time units are time units in data - days per year
    
    # TODO: fix hardcoded reference to cellId column name at end of function
  # data = altq.dat
    if (suppressWarnings(is.rsm(data))) {
      cellIDs <- data$cellIDs #which(data$cellMap[1,] %in% data$cellIDs) # data$cellIDs # why isn't cellsToUse working here? need an index
      dates   <- data$dates
      returnSpatial <- TRUE
      spdf    <- data$spdf
      data    <- data$stage
    }
  
  
    ### reduce cells considered
    if(is.numeric(cellIDs)) {
      data <- data[cellIDs, ] # rows are mesh cells, columns are days. `cellIDs` here needs to be an index of rows to use
    } else { # if 'all'
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
                                   X = data[, which(dates2$year == unique(dates2$year)[i])], # export "threshold", "output"
                                   #1:nrow(data[1:2, dates2$year == i]),
                                   FUN = func, ...) # , na.rm = TRUE) #
      # yrVals <- parallel::parApply(cl = cl, MARGIN = 1,
      #                              X = data[, which(dates2$year == unique(dates2$year)[i])], # export "threshold", "output"
      #                              #1:nrow(data[1:2, dates2$year == i]),
      #                              FUN = hydroperiod, continuous = FALSE)
      # yrVals <- parallel::clusterMap(cl = cl, #MARGIN = 1, 
      #                                X = data[, dates2$year == unique(dates2$year)[i]], # export "threshold", "output"
      #                                #1:nrow(data[1:2, dates2$year == i]), 
      #                                fun = func, ...) 
      # all.equal(yrVals, yrVals2)
      
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
      ### 
      # cellNames <- names(spdf)[1]# "CellId" column
      # yrDat <- cbind(cellIDs, returnDat)
      # names(yrDat)[1] <- cellNames # label "CellId" column
      ### merge data with spdf
      # returnDat2 <- terra::merge(spdf, returnDat)#, by = cellNames)
      returnDat <- cbind(spdf, returnDat)#, by = cellNames)
      # plot(returnDat2, 'returnDat')
      # print(sp::spplot(returnDat, zcol = names(yrDat)[2])) # an example plot
    }
    
    # if ((returnSpatial == TRUE) && !is.null(spdf)) {
    #   cellNames <- names(spdf)[1]# "CellId" column
    #   yrDat <- cbind(cellIDs, returnDat)
    #   names(yrDat)[1] <- cellNames # label "CellId" column
    #   ### merge data with spdf
    #   returnDat <- terra::merge(spdf, yrDat, by = cellNames)
    #   # print(sp::spplot(returnDat, zcol = names(yrDat)[2])) # an example plot
    # }
    
    if (includeMean) {
      returnDat$mean <- rowMeans(data.frame(returnDat[, grep(x = names(returnDat), pattern = 'yr')]), na.rm = TRUE)
    }
    
    invisible(returnDat)
  }
  
