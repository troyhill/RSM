#' getDEM
#'
#' @description Get land surface elevation data from RSM netCDF.
#' 
#'  
#' @param  ncdf_address   address of the netCDF file
#' @param  mesh          the spatial object to join output to. There must be a column named 'CellId' (case sensitive) in the mesh.
#' 
#'
#' @return output is a terra SpatVector polygon object with the DEM values in native units and datum.
#'
#' @examples
#' 
#' \dontrun{
#' altq_addr <- "G:/data/models/COP/ALTQ/globalmonitors.nc"
#' copMesh  <- vect(system.file("extdata/gis/COP_mesh", "mesh.shp", package="RSM"),"mesh") # 6719 features
#' 
#' altq_dem <- getDEM(ncdf_address = altq_addr, mesh = copMesh)
#' }
#' 
#' 
#' @importFrom ncdf4 nc_open
#' @importFrom ncdf4 ncvar_get
#' @importFrom ncdf4 nc_close
#' @export
#' 

getDEM <- function(ncdf_address, mesh) {
  nc_cop     <- ncdf4::nc_open(ncdf_address) #e.g. "G:/data/models/COP/ALTQ/globalmonitors.nc"
  topo       <- ncdf4::ncvar_get(nc_cop, "Topography") # I think units are feet NGVD29??
  ncdf4::nc_close(nc_cop)
  
  cellValue  <- sort(as.integer(mesh$CellId)) # must be integer, must be in ascending order.
  rowToUse   <- which(altq$cellMap[1, ] %in% cellValue)
  topo_df    <- data.frame(topo = topo, CellId = cellValue)
  mesh$dem   <- topo_df$topo[order(match(topo_df$CellId, mesh$CellId))]
  plot(mesh, 'dem', type = 'continuous', axes = FALSE)
  return(mesh)
}