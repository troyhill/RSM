#' loadRSM
#'
#' @description pulls stage data from an RSM output netCDF file
#' 
#'  
#' @param  ncdf_address   address of the netCDF file
#' @param  variable       name of variable to extract from the netCDF
#' @param  subtractTopo   should topography be subtracted? i.e., should water depths (`subtractTopo = TRUE`) or stages (`subtractTopo = FALSE`) be returned?
#' 
#'
#' @return a list with three elements: (1) a vector of dates in the time series, (2) stage data matrices (not explicitly geospatial data), and (3) a cell map that links data to cells in the RSM mesh
#'
#' @examples
#' 
#' \dontrun{
#' addr <- "G:/data/models/COP/ALTQ/globalmonitors.nc"
#' altq <- loadRSM(ncdf_address = "G:/data/models/COP/ALTQ/globalmonitors.nc")
#' 
#' WCA3_cellIDs <- getFeatureIDs(shpFeature = wca3,
#'   as(fireHydro::BICY_EVER_PlanningUnits,  "Spatial"),
#'   mesh = copMesh, cellIDcolumn = "CellId")
#'   
#'   hp.altq <- nc_apply(data = altq$data, # chead.altq,
#'   dates = altq$dateVec,
#'   yearBegin = 1, 
#'   yearlength = 12,
#'   spdf = ENP_cellIDs$mesh, returnSpatial = TRUE,
#'   func = hydroperiod, threshold = 0, continuous = FALSE) 
#
#' }
#' 
#' 
#' @importFrom ncdf4 nc_open
#' @importFrom ncdf4 ncvar_get
#' 
#' @export
#' 

### TODO: have option to summarize by year, year-mo, etc. 

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
  
  return(list(dateVec = dateVec,
              data    = chead.altq,
              cellMap = cellMap))
}

