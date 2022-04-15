#' rsm_apply
#'
#' @description applies a function to RSM netcdf data
#' 
#'  
#' @param  data          target object. function is applied to each row. If an `rsm` object is provided, several arguments are ignored: `cellIDs`, `dates`, `spdf`, `returnSpatial`
#' @param  cellIDs       which cellIDs to use? A numeric vector or "all"
#' @param  cellMap       if all cellIDs are not used, a cellMap (from ncvar_get(nc_cop, "cellmap") or loadRSM) should be provided here. This links cellIDs to the correct row in the netcdf. The first row of a cellMap must have the CellID.
#' @param  dates         a POSIXlt vector of dates
#' @param  spdf          the spatial object to join output to
#' @param  yearBegin     first month of year
#' @param  yearLength    length of "year" (units = months)
#' @param  includeMean    if TRUE, a column is included that averages across all non-ID columns in the dataset (this is typically an annual average)
#' @param  func          function to apply to each year and each cell
#' 
#'
#' @return output is a terra SpatVector polygon object with the values returned by "func" applied to each year and each cell
#'
#' @examples
#' 
#' \dontrun{
#' altq <- loadRSM(ncdf_address = "G:/data/models/COP/ALTQ/globalmonitors.nc")
#' 
#' copMesh <- vect(system.file("extdata/gis/COP_mesh", "mesh.shp", package="RSM"),"mesh") # 6719 features
#' 
#' hp.altq <- rsm_apply(data = altq$data,
#' dates = altq$dateVec,
#' cellMap = altq$cellMap,
#' cellIDs = copMesh$CellId,
#' yearBegin = 1,
#' yearlength = 12,
#' spdf = copMesh,
#' func = function(x) {hydroperiod(as.numeric(x), threshold = 0, continuous = FALSE)})  
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
                     dates   = NULL,#   = altq$dateVec,
                     cellMap = NULL,# = altq$cellMap,
                     cellIDs,# = ENP_cellIDs$cellIDs, # commonCells2, #targetHydro$CellId, # 
                     yearBegin  = 1, 
                     yearLength = 12,
                     spdf,# = copMesh, #  ENP_cellIDs$mesh, 
                     returnSpatial = TRUE,
                     func = function(x) {mean(x)}) {
  # if (suppressWarnings(is.rsm(data))) {
  #   #cellIDs <- data$cellIDs #which(data$cellMap[1,] %in% data$cellIDs) # data$cellIDs # why isn't cellsToUse working here? need an index
  #   dates   <- data$dateVec
  #   returnSpatial <- TRUE
  #   #spdf    <- data$spdf # not currently included in rsm objects
  #   data    <- data$data
  #   cellMap <- data$cellMap
  # }
  
  ### input checks
  if(!any(grepl(x = class(spdf), pattern = 'SpatVector'))) {
    spdf <- terra::vect(spdf)
  }
  
  cellValue <- sort(as.integer(cellIDs)) # must be integer, must be in ascending order.
  rowToUse <- which(cellMap[1, ] %in% cellValue)
  dat      <- data.frame(t(data[rowToUse, ]))
  names(dat) <- cellValue
  dat$date <- dates
  dat$year <- as.numeric(format(dat$date, '%Y'))
  # head(dat)
  
  ### year/season conversions
  ### adjust dates
  yearBegin <- yearBegin - 1 # change to 0-11 scale
  dates3 <- zoo::as.yearmon(dates) - yearBegin/12
  dates4 <- dates3[as.numeric(format(dates3, format = "%m")) %in% 1:yearLength]
  dat$adjusted.year <- format(dates3, format = "%Y")
  dat <- dat[as.numeric(format(dates3, format = "%m")) %in% 1:yearLength, ]
  
  ### remove incomplete adjusted years
  yrsToRemove <- unique(format(dates4, format = "%Y"))[which(table(format(dates4, format = "%Y")) < yearLength*30)]
  if (length(yrsToRemove) > 0) {
    message('based on yearBegin and yearLength arguments, the following adjusted years were removed for incompleteness: ', paste0(yrsToRemove, collapse = ","))
  }
  newDatYears <- format(dates4, format = '%Y')
  newDatYears <- newDatYears[!(newDatYears %in% yrsToRemove)]
  
  for (i in 1:length(unique(newDatYears))) {
    newDat <- dat[as.numeric(dat$adjusted.year) == as.numeric(unique(newDatYears)[i]), !grepl(x = tolower(names(dat)), pattern = 'year|date')]
    dat.tmp <- data.frame(t(apply(newDat, 2, FUN = func)))
    dat.tmp$year <- unique(newDatYears)[i] # adjusted year
    if (i == 1) {
      dat.fin <- dat.tmp
    } else {
      dat.fin <- rbind(dat.fin, dat.tmp)
    }
  }
  
  # summary(dat.fin) # every column is a cell, rows are years
  dat.fin2 <- data.frame(t(dat.fin[, !grepl(x = names(dat.fin), pattern = 'year')])) # head(dat.fin2[, 1:5])
  names(dat.fin2) <- paste0("yr", unique(newDatYears)) # I go back and forth about whether it's best to use dates (calendar year) or dates2 (WY/transformed year). 
  dat.fin2$CellId <- substr(row.names(dat.fin2), 2, nchar(row.names(dat.fin2)))
  # dat.fin2[match(cellMap[1, ], dat.fin2$CellId), ]
  dat.fin2 <- terra::merge(spdf, dat.fin2, by = "CellId")
  dat.fin2 <- terra::crop(dat.fin2, spdf)
  dat.fin2$mean <- rowMeans(data.frame(dat.fin2[, grep(x = names(dat.fin2), pattern = 'yr')]), na.rm = TRUE)
  terra::plot(dat.fin2, 'mean', type = 'continuous', axes = FALSE, main = 'Annual mean')
  return(dat.fin2)
}
