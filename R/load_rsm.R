#' loadRSM
#'
#' @description pulls stage data from an RSM output netCDF file
#' 
#'  
#' @param  ncdf_address   address of the netCDF file
#' @param  variable       name of variable to extract from the netCDF
#' @param  subtractTopo   should topography be subtracted? i.e., should water depths (`subtractTopo = TRUE`; units = feet relative to soil surface) or stages (`subtractTopo = FALSE`; units = feet NGVD29) be returned?
#' @param  divideByArea   In development - do not use. should quantity be divided by cell area? Useful for volumetric quantities: rainfall/ET. Note that this assumes congruent units in both quantities, e.g., rainfall (ft3) / area (ft2) = rainfall (ft)
#'
#' @return a list with class `rsm` containing three elements: (1) a vector of dates in the time series, (2) stage data matrices (not explicitly geospatial data), and (3) a cell map that links data to cells in the RSM mesh
#'
#' @examples
#' 
#' \dontrun{
#' addr <- "G:/data/models/COP/ALTQ/globalmonitors.nc"
#' altq <- loadRSM(ncdf_address = addr, 
#'                 variable     = 'ComputedHead', 
#'                 subtractTopo = TRUE)
#' }
#' 
#' 
#' @importFrom ncdf4 nc_open
#' @importFrom ncdf4 ncvar_get
#' @importFrom ncdf4 nc_close
#' 
#' @export
#' 

### consider including mesh here? idk

loadRSM <- function(ncdf_address, variable = 'ComputedHead', subtractTopo = TRUE,
                    divideByArea = FALSE) {
  nc_cop     <- ncdf4::nc_open(ncdf_address) #e.g. "G:/data/models/COP/ALTQ/globalmonitors.nc"
  cellMap    <- ncdf4::ncvar_get(nc_cop, "cellmap") # 6719 columns: 1 column per cell. first row appears to be CellIds, 2nd row is an index starting at zero
  if (!variable %in% names(nc_cop$var)) {
    stop(variable, ' not found in netcdf. please check variable names/spelling: \n', names(nc_cop$var))
  }
  chead.altq <- ncdf4::ncvar_get(nc_cop, variable, verbose = TRUE)
  
  if (subtractTopo) {
    topo       <- ncdf4::ncvar_get(nc_cop, "Topography") # I think units are feet NGVD29??
    # subtract topography to get water depths. 
    chead.altq <- sweep(x = chead.altq, MARGIN = 1, STATS = topo, FUN = `-`)
  }
  if (divideByArea) {
    areaVals       <- ncdf4::ncvar_get(nc_cop, "cellarea") # convert area from square meters to sq feet
    # divide volume by area to get depth units 
    ### this doesn't work bc there are extra features in wbudget.nc: 7964 rows instead of 6719
    ### are the extra features appended or intermixed?
    ### reduce reported cells to ncol(cellMap) - this assumes additional features were appended and are not intermixed
    chead.altq <- sweep(x = chead.altq[1:ncol(cellMap), ], MARGIN = 1, STATS = areaVals, FUN = `/`)
    ### ^produces values too high and variable, though spatially looks ok. something is wrong. Area units are square feet...
    
    ### development
    # sqft <- expanse(copMesh, unit = 'm') *10.764 
    # summary(sqft)
    # summary(areaVals) # identical - confirms cellarea units are sq ft
    # chead.altq2 <- chead.altq
    # chead.altq2[1:ncol(cellMap), ] <- sweep(x = chead.altq[1:ncol(cellMap), ], MARGIN = 1, STATS = areaVals, FUN = `/`)
    # # identical(rain$data[1,] / areaVals[1], chead.altq2[1, ]) # rain = from untitled script
    # sum(chead.altq[1, 1:365]) / areaVals[1] # ft in first year of first cell
    # sum(chead.altq2[1, 1:365]) # matches. so, find differences
    # for (i in 1:ncol(cellMap)) {
    #   chead.adj <- chead.altq2[i, ]
    #   chead     <- chead.altq[i, ]  / areaVals[i]
    #   if (identical(chead.adj, chead)) {
    #     # cat('cell ', i, ' ok\n')
    #   } else {
    #     cat('cell ', i, ' conversion is not identical\n')
    #   }
    # } # they're all identical. not surprising, this is exactly what sweep does. error is in aggregation?
    
    
    
  }
  ### Generate date vector 
  dateVec     <- RSM::getDateVector(nc_cop)
  
  rsmList <- list(dates   = dateVec,
                  data    = chead.altq,
                  cellMap = cellMap)
  class(rsmList) <- c("rsm", grep(x = class(rsmList), pattern = "rsm", invert = TRUE, value = TRUE)) 
  
  ncdf4::nc_close(nc_cop)
  
  return(rsmList)
}

