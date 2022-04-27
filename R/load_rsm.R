#' loadRSM
#'
#' @description pulls stage data from an RSM output netCDF file
#' 
#'  
#' @param  ncdf_address   address of the netCDF file
#' @param  variable       name of variable to extract from the netCDF
#' @param  subtractTopo   should topography be subtracted? i.e., should water depths (`subtractTopo = TRUE`; units = feet relative to soil surface) or stages (`subtractTopo = FALSE`; units = feet NGVD29) be returned?
#' 
#'
#' @return a list with class `rsm` containing three elements: (1) a vector of dates in the time series, (2) stage data matrices (not explicitly geospatial data), and (3) a cell map that links data to cells in the RSM mesh
#'
#' @examples
#' 
#' \dontrun{
#' addr <- "G:/data/models/COP/ALTQ/globalmonitors.nc"
#' altq <- loadRSM(ncdf_address = addr)
#' }
#' 
#' 
#' @importFrom ncdf4 nc_open
#' @importFrom ncdf4 ncvar_get
#' 
#' @export
#' 

### consider including mesh here? idk

loadRSM <- function(ncdf_address, variable = 'ComputedHead', subtractTopo = TRUE) {
  nc_cop     <- ncdf4::nc_open(ncdf_address) #e.g. "G:/data/models/COP/ALTQ/globalmonitors.nc"
  cellMap    <- ncdf4::ncvar_get(nc_cop, "cellmap") # 6719 columns: 1 column per cell. first row appears to be CellIds, 2nd row is an index starting at zero
  chead.altq <- ncdf4::ncvar_get(nc_cop, variable, verbose = TRUE)
  
  if (subtractTopo) {
    topo       <- ncdf4::ncvar_get(nc_cop, "Topography") # I think units are feet NGVD29??
    # subtract topography to get water depths. 
    chead.altq <- sweep(chead.altq, 1, topo)
  }
  ### Generate date vector 
  dateVec     <- RSM::getDateVector(nc_cop)
  
<<<<<<< HEAD
  rsmList <- list(dates   = dateVec,
                  data    = chead.altq,
                  cellMap = cellMap)
  class(rsmList) <- c("rsm", grep(x = class(rsmList), pattern = "rsm", invert = TRUE, value = TRUE)) 
  
  return(rsmList)
=======
  outList <- list(dateVec = dateVec,
                  data    = chead.altq,
                  cellMap = cellMap)
  class(outList) <- c("rsm", grep(x = class(outList), pattern = "rsm", invert = TRUE, value = TRUE)) 
  
  return(outList)
>>>>>>> 3e5fdd705b4b3cb1c634d6a6e745198f8309e8b3
}

