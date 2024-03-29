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
#' ### example comparing two RSM DEMs
#' cop_address   <- "D:/data/models/COP/ALTQ/globalmonitors.nc"
#' losom_address <- 
#'  "D:/data/models/LOSOM/Iteration_3/preferred_alt_RSMGL/Model_Output/ECB19/globalmonitors.nc"
#' 
#' # 6719 features
#' copMesh     <- vect(system.file("extdata/gis/COP_mesh", "mesh.shp", package="RSM"),"mesh") 
#' 
#' copDEM       <- getDEM(cop_address, copMesh)
#' losom_topo   <- getDEM(losom_address, copMesh) 
#' losom_topo$DEM_diff <- losom_topo$dem - copDEM$dem
#' 
#' par(fig = c(0, 0.5, 0, 1))
#' plot(losom_topo, "DEM_diff", type = 'continuous', main = 'LOSOM DEM - COP DEM (ft.)', 
#'   axes = FALSE)
#' plot(eden_domain + wca3 + wca3b + wca1 + wca2b, add = TRUE, lwd = 2)
#' par(new = T, fig = c(0.5, 1, 0, 1), mar = c(3, 5, 0.5, 0.5))
#' boxplot(losom_topo$DEM_diff, ylab = 'LOSOM DEM - COP DEM (ft.)')
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
  cellMap    <- ncdf4::ncvar_get(nc_cop, "cellmap") # 6719 columns: 1 column per cell. first row appears to be CellIds, 2nd row is an index starting at zero
  topo       <- ncdf4::ncvar_get(nc_cop, "Topography") # I think units are feet NGVD29??
  ncdf4::nc_close(nc_cop)
  
  cellValue  <- sort(as.integer(mesh$CellId)) # must be integer, must be in ascending order.
  rowToUse   <- which(cellMap[1, ] %in% cellValue)
  topo_df    <- data.frame(topo = topo, CellId = cellValue)
  mesh$dem   <- topo_df$topo[order(match(topo_df$CellId, mesh$CellId))]
  plot(mesh, 'dem', type = 'continuous', axes = FALSE)
  return(mesh)
}