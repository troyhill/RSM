#' rsm_apply
#'
#' @description applies a function to RSM netcdf data
#' 
#'  
#' @param  data          target object. function is applied to each row. If an `rsm` object is provided, the object is used for `cellMap` and `dates` arguments
#' @param  dates         a POSIXlt vector of dates in the ncdf data
#' @param  cellMap       if all cellIDs are not used, a cellMap (from ncvar_get(nc_cop, "cellmap") or loadRSM) should be provided here. This links cellIDs to the correct row in the netcdf. The first row of a cellMap must have the CellID.
#' @param  variable      name of variable extracted from the netCDF by loadRSM()
#' @param  cellIDs       which cellIDs to use in analysis? A numeric vector or NULL to use all cellIDs in the mesh. There must be a column named 'CellId' (case sensitive) in the mesh.
#' @param  mesh          the spatial object to join output to. There must be a column named 'CellId' (case sensitive) in the mesh.
#' @param  yearBegin     first month of year.
#' @param  yearLength    length of "year" (units = months).
#' @param  aggregation    Temporal scale used for aggregating data: 'yr', 'mo', 'da'. Note: returning daily data will require a substantial amount of RAM and may crash R.
#' @param  includeMean    if TRUE, a column is included that averages across all non-ID columns in the dataset
#' @param  func          function applied to the daily time series of data for each cell.
#' 
#'
#' @return output is a terra SpatVector polygon object with the values returned by "func" applied to each year and each cell
#'
#' @examples
#' 
#' \dontrun{
#' altq <- loadRSM(ncdf_address = "D:/data/models/COP/ALTQ/globalmonitors.nc")
#' 
#' # 6719 features
#' copMesh <- vect(system.file("extdata/gis/COP_mesh", "mesh.shp", package="RSM"),"mesh")
#' 
#' ### a simple function for discontinuous hydroperiod
#' hp_discont <- function(x, threshold = 0) {
#' outDat      <- length(which(as.numeric(x[!is.na(x)]) >= threshold))
#' return(outDat)
#' }
#' 
#' hp.altq <- rsm_apply(data = altq,
#' yearBegin = 1,
#' yearLength = 12,
#' mesh = copMesh,
#' func = function(x) {hp_discont(x, threshold = 0)})
#' 
#' }
#' 
#' 
#' @importFrom zoo    as.yearmon
#' @importFrom terra crop
#' @importFrom terra plot
#' @importFrom terra vect
#' 
#' @export
#' 

### TODO: have option to summarize by year, year-mo, etc. 

rsm_apply <- function(data,#    = altq$data, 
                     dates = NULL,#   = altq$dateVec,
                     cellMap = NULL,# = altq$cellMap,
                     variable = NULL,
                     mesh,# = copMesh, #  ENP_cellIDs$mesh, 
                     cellIDs = NULL,# = ENP_cellIDs$cellIDs, # commonCells2, #targetHydro$CellId, # 
                     yearBegin  = 1, 
                     yearLength = 12,
                     aggregation = 'yr', # 'yr', 'mo', 'da'
                     includeMean = TRUE,
                     # returnSpatial = TRUE,
                     func = function(x) {mean(x, na.rm = TRUE)}) {
  if (any(is.rsm(data))) {
    # cellIDs <- data$cellIDs #which(data$cellMap[1,] %in% data$cellIDs) # data$cellIDs # why isn't cellsToUse working here? need an index
    # returnSpatial <- TRUE
    # mesh    <- data$mesh # mesh not presently included in loadrsm output
    cellMap <- data$cellMap
    dates   <- data$dates
    variable <- data$variable
    data    <- data$data
  }
  
  ### input checks
  if(!any(grepl(x = class(mesh), pattern = 'SpatVector'))) {
    mesh <- terra::vect(mesh)
  }
  if (!any(grepl(x = names(mesh), pattern = 'CellId'))) {
    stop("mesh must have a column named 'CellId'. This is case-sensitive.\n")
  }
  
  ### if `cellIDs` argument is null, pull them from the mesh
  if (all(is.null(cellIDs))) {
    ### gotta figure out how to get vect columns by index
    # cellidColumn <- grep(x = tolower(names(mesh)), pattern = 'cellid')
    cellIDs <- as.integer(mesh$CellId) # as.integer(mesh[, cellidColumn])
  }
  
  cellValue  <- sort(as.integer(cellIDs)) # must be integer, must be in ascending order.
  
  if (grepl(x = variable, pattern = 'olvector|gwvector')) {
    ### for datatypes with a three-dimensional matrix (direction and magnitude vectors)
    rowToUse   <- which(cellMap[1, ] %in% cellValue)
    dat        <- data.frame(t(data[1, rowToUse, ])) # not sure if direction or magnitude is first
    names(dat) <- cellValue
    
    ### do same for second quantity
    mag        <- data.frame(t(data[2, rowToUse, ]))
    names(mag) <- cellValue
    mag$date   <- dates
    mag$year   <- as.numeric(format(mag$date, '%Y'))
  } else {
    ### for datatypes with a single quantity (two-dimensional matrix)
    rowToUse   <- which(cellMap[1, ] %in% cellValue)
    dat        <- data.frame(t(data[rowToUse, ]))
    names(dat) <- cellValue
    mag        <- NULL # maybe have it be a df of 1s?
  }
  
  
  ### deal with dates
  dat$date   <- dates
  dat$year   <- as.numeric(format(dat$date, '%Y'))
  ### now select data that are in the desired months, adjust year if needed to accommodate water years
  month_vector <- as.numeric(format(dates ,"%m"))
  year_vector  <- as.numeric(format(dates ,"%Y"))
  
  monthsToUse    <- yearBegin:(yearBegin+yearLength-1) %% 12
  # monthsToUse    <- 10:(10+12-1) %% 12
  monthsToUse[monthsToUse == 0] <- 12 # where modulo = 0, month = 12
  
  ### adjust years if needed
  dat$adjusted.year   <- dat$year   <- as.numeric(format(dat$date, '%Y'))
  if ((yearLength + yearBegin) > 13)  {
    ### add 1 to years where month <= 12
    dat$adjusted.year[(as.numeric(format(dat$date, "%m")) >= yearBegin)] <- dat$year[(as.numeric(format(dat$date, "%m")) >= yearBegin)] + 1
  }
  
  ### now remove all data outside months of interest
  dat <- dat[as.numeric(format(dat$date, "%m")) %in% monthsToUse, ]
  
  
  # head(dat[, 1:5])
  ### year/season conversions
  ### adjust dates
  # yearBegin <- yearBegin - 1 # change to 0-11 scale
  # # dates3    <- zoo::as.yearmon(dates) - yearBegin/12 # water years end aligned with calendar year
  # if (yearBegin > 0) {# if year does not start in January
  #   dates3    <- zoo::as.yearmon(dates) + (12-yearBegin)/12 # FL water year: April becomes December, Dec becomes August
  #   ### this corrects months so Jan is always the beginning of a 'year'
  #   ### but can produce incorrect year data when seasonal data are targeted (i.e. all months come from same calendar year)
  # } else {
  #   dates3    <- zoo::as.yearmon(dates)
  # }
  # dates4    <- dates3[as.numeric(format(dates3, format = "%m")) %in% 1:yearLength]
  # dat$adjusted.year <- format(dates3, format = "%Y")
  # if ((yearLength + yearBegin + 1) > 12) {
  #   ### this if statement corrects year labeling problem noted above with seasonal data
  #   ### solution is to use calendar year if time period requested doesn't span more than one calendar years
  #   dat$adjusted.year <- dat$year
  #   # dates4 <- dates4 - 1 # subtracting 1 makes seasons log correctly
  # }
  # dat       <- dat[as.numeric(format(dates3, format = "%m")) %in% 1:yearLength, ]
  ## head(dat[, (ncol(dat)-5):ncol(dat)])
  
  if (!is.null(mag)) {
    mag <- mag[as.numeric(format(mag$date, "%m")) %in% monthsToUse, ]
    mag$adjusted.year <- dat$adjusted.year #format(dates3, format = "%Y")
    # mag <- mag[as.numeric(format(dates3, format = "%m")) %in% 1:yearLength, ]
  }
  
  # dat[1:180, c('date', 'year', 'adjusted.year')]
  
  ### remove incomplete adjusted years
  yrsToRemove <- unique(format(dat$adjusted.year, format = "%Y"))[which(table(format(dat$adjusted.year, format = "%Y")) < yearLength*29)]
  if (length(yrsToRemove) > 0) {
    message('based on yearBegin and yearLength arguments, the following adjusted years were removed for incompleteness: ', paste0(yrsToRemove, collapse = ","))
  }
  newDatYears <- format(dat$adjusted.year, format = '%Y') # these are adjusted years; water year, etc.
  newDatYears <- newDatYears[!(newDatYears %in% yrsToRemove)]
  
  if (all(grepl(x = aggregation, pattern = 'yr'))) {
    for (i in 1:length(unique(newDatYears))) {
      newDat <- dat[as.numeric(dat$adjusted.year) == as.numeric(unique(newDatYears)[i]), !grepl(x = tolower(names(dat)), pattern = 'year|date')]
      # head(newDat[, (ncol(newDat)-5):ncol(newDat)])
      dat.tmp <- data.frame(t(apply(newDat, 2, FUN = func)))
      dat.tmp$year <- unique(newDatYears)[i] # adjusted year
      # head(dat.tmp[, (ncol(dat.tmp)-5):ncol(dat.tmp)])
      if (i == 1) {
        dat.fin <- dat.tmp
      } else {
        dat.fin <- rbind(dat.fin, dat.tmp)
      }
    }
    timeNames  <- paste0(aggregation, unique(newDatYears))
  } else if (all(grepl(x = aggregation, pattern = 'da'))) {
    dat.fin      <- dat[, -c(grep(x = names(dat), pattern = 'year|date'))]
    dat.fin$year <- format(dat$date, format = "%Y%m%d")
    ### these are adjusted years - calendar, water etc.
    dat.fin$year <- paste0(dat$adjusted.year, substr(x = dat.fin$year, start = 5, stop = 8))
    timeNames <- paste0(aggregation, dat.fin$year)
  } else if (all(grepl(x = aggregation, pattern = 'mo'))) {
    stop ('`aggregation` must be set to `yr` or `da`; other aggregation intervals not currently supported.')
  } else {
    stop ('`aggregation` must be set to `yr` or `da`; other aggregation intervals not currently supported.')
  }
  
  # summary(dat.fin) # every column is a cell, rows are years
  dat.fin2 <- data.frame(t(dat.fin[, !grepl(x = names(dat.fin), pattern = 'year')])) # head(dat.fin2[, 1:5])
  names(dat.fin2) <- timeNames # I go back and forth about whether it's best to use dates (calendar year) or dates2 (WY/transformed year). 
  
  dat.fin2$CellId <- gsub(x = row.names(dat.fin2), pattern = "X|x", replacement = "")
  
  # if (grepl(x = aggregation, pattern = "yr")) {
  #   # length(unique(row.names(dat.fin2)))
  #   # head(row.names(dat.fin2))
  #   ### why is this needed for yearly data? Bc aggregation method adds an 'X' to the rownames
  #   dat.fin2$CellId <- substr(row.names(dat.fin2), 2, nchar(row.names(dat.fin2)))
  # }
  # if (grepl(x = aggregation, pattern = "da")) {
  #   # length(unique(row.names(dat.fin2)))
  #   # head(row.names(dat.fin2))
  #   dat.fin2$CellId <- row.names(dat.fin2)
  #   # length(unique(dat.fin2$CellId)) # == 6719
  # }
  # length(unique(dat.fin2$CellId)) # 1110??? yet nrow(dat.fin2) = 6719
  # dat.fin2$CellId[match(cellMap[1, ], dat.fin2$CellId)]
  # head(dat.fin2$CellId)
  # head(mesh$CellId)
  # all((dat.fin2$CellId %in% dat.fin2$CellId))
  # head(dat.fin2$CellId[!(dat.fin2$CellId %in% dat.fin2$CellId)])
  ### trying to get daily data to load correctly
  # dat.fin2_tmp <- dat.fin2[order(ordered(dat.fin2$CellId, levels = mesh$CellId)), ] # https://stackoverflow.com/a/42605492/3723870
  # dat.fin2 <- mesh
  # values(dat.fin2) <- dat.fin2_tmp
  # dat.fin2 <- terra::merge(mesh, dat.fin2, by = "CellId")
  # plot(dat.fin2, 500, type = 'continuous', range = c(-5, 5)) # garbage
  
  ### this works for yr but not da? why? because multiple rows per cell.
  dat.fin2 <- terra::merge(mesh, dat.fin2, by = "CellId")
  # plot(dat.fin2, 500, type = 'continuous', range = c(-5, 5)) # nice
  dat.fin2 <- terra::crop(dat.fin2, mesh)
  dat.fin2$mean <- rowMeans(data.frame(dat.fin2[, timeNames]),
                            na.rm = TRUE)
  terra::plot(dat.fin2, 'mean', type = 'continuous', axes = FALSE, #range = c(5, 5), 
              main = paste0('Mean value\n(n = ', length(timeNames)," obs per cell)"))
  outData <- dat.fin2
  
  if (!is.null(mag)) {
    if (all(grepl(x = aggregation, pattern = 'yr'))) {
      for (i in 1:length(unique(newDatYears))) {
        newDat <- mag[as.numeric(mag$adjusted.year) == as.numeric(unique(newDatYears)[i]), !grepl(x = tolower(names(dat)), pattern = 'year|date')]
        # head(newDat[, (ncol(newDat)-5):ncol(newDat)])
        dat.tmp <- data.frame(t(apply(newDat, 2, FUN = func)))
        dat.tmp$year <- unique(newDatYears)[i] # adjusted year
        # head(dat.tmp[, (ncol(dat.tmp)-5):ncol(dat.tmp)])
        if (i == 1) {
          dat.fin <- dat.tmp
        } else {
          dat.fin <- rbind(dat.fin, dat.tmp)
        }
      }
      timeNames  <- paste0(aggregation, unique(newDatYears))
    } else if (all(grepl(x = aggregation, pattern = 'da'))) {
      dat.fin      <- mag[, -c(grep(x = names(mag), pattern = 'year|date'))]
      dat.fin$year <- format(mag$date, format = "%Y%m%d")
      ### these are adjusted years - calendar, water etc.
      dat.fin$year <- paste0(mag$adjusted.year, substr(x = dat.fin$year, start = 5, stop = 8))
      timeNames <- paste0(aggregation, dat.fin$year)
    } else if (all(grepl(x = aggregation, pattern = 'mo'))) {
      stop ('`aggregation` must be set to `yr` or `da`; other aggregation intervals not currently supported.')
    } else {
      stop ('`aggregation` must be set to `yr` or `da`; other aggregation intervals not currently supported.')
    }
    
    # summary(dat.fin) # every column is a cell, rows are years
    mag.fin2 <- data.frame(t(dat.fin[, !grepl(x = names(dat.fin), pattern = 'year')])) # head(mag.fin2[, 1:5])
    names(mag.fin2) <- timeNames # I go back and forth about whether it's best to use dates (calendar year) or dates2 (WY/transformed year). 
    
    mag.fin2$CellId <- gsub(x = row.names(mag.fin2), pattern = "X|x", replacement = "")
    
    ### this works for yr but not da? why? because multiple rows per cell.
    mag.fin2 <- terra::merge(mesh, mag.fin2, by = "CellId")
    # plot(mag.fin2, 500, type = 'continuous', range = c(-5, 5)) # nice
    mag.fin2 <- terra::crop(mag.fin2, mesh)
    mag.fin2$mean <- rowMeans(data.frame(mag.fin2[, grep(x = names(mag.fin2), pattern = aggregation)]), na.rm = TRUE)
    terra::plot(mag.fin2, 'mean', type = 'continuous', axes = FALSE, #range = c(5, 5), 
                main = paste0('Mean value\n(n = ', sum(grepl(x = names(mag.fin2), pattern = aggregation))," obs per cell)"))
    outData <- list(dir = dat.fin2, 
                    magnitude = mag.fin2)
  }
  
  return(outData) # 'year' is adjusted per user inputs; may be calendar, water, fiscal year, etc. 
}

