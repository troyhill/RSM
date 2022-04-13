#' getFeatureIDs
#'
#' @description Extracts cell IDs from a model mesh for each indicator region. Currently only supports sf objects
#' 
#'  
#' @param  shpFeature       an sf object with the indicator region, transect, point
#' @param  mesh             sf object with model mesh cells and cell ID data (mesh must have a column named "CellId")
#' @param  cellIDcolumn     name of column containing cell identifiers. Case insensitive.
#'
#' @return a list with three elements: a vector of cell IDs, (2) a vector indexing selected cells in the original mesh, and (3) a SpatVector object with the intersection of the mesh and the input shapefile
#'
#' @importFrom terra  project
#' @importFrom terra  crop
#' @importFrom terra  vect
#' 
#' @export
#' 

### add RTools directory to PATH 
# Sys.getenv("PATH")
# old_path <- Sys.getenv("PATH") 
# Sys.setenv(PATH = paste(old_path, "C:\\Rtools\\bin;C:\\Rtools\\gcc-4.6.3\\bin;C:\\Rtools\\perl\\bin;C:\\Rtools\\MinGW\\bin", sep = ";"))


getFeatureIDs <- function(shpFeature, # = fireHydro::BICY_EVER_PlanningUnits, # test that this works on points, lines, polygons. MUST BE A spatial*DF  
                          mesh = vect(system.file("extdata/gis/nsrsm_v352", "nsrsm_mesh_landuse.shp", package="RSM"),"nsrsm_mesh_landuse"),# mesh must have a column named "CellId"
                          cellIDcolumn = "CellId") { 
  # ### make CRS of shpFeature == mesh
  # shpFeature <- as(fireHydro::BICY_EVER_PlanningUnits, 'Spatial')
  # mesh <- readOGR(system.file("extdata/gis/nsrsm_v352", "nsrsm_mesh_landuse.shp", package="RSM"),"nsrsm_mesh_landuse")
  # shpFeature <- sp::spTransform(shpFeature, crs(mesh))
  # ### clip
  # int <- raster::intersect(x = mesh,  y = shpFeature) 
  # ### pull and return cellIDs
  # plot(int)
  # getName <- grep(x = toupper(names(int)),   pattern = toupper(cellIDcolumn))  
  # returnIDs <- as.data.frame(int[, getName]@data)
  # invisible(list(cellIDs = as.numeric(returnIDs[, 1]), spdf = int[, getName]))
  
  
  ##### new version
  # options(sf_max.plot=1)
  # shpFeature <- fireHydro::BICY_EVER_PlanningUnits
  # mesh <- sf::read_sf(system.file("extdata/gis/nsrsm_v352", "nsrsm_mesh_landuse.shp", package="RSM"),"nsrsm_mesh_landuse")
  if (!class(mesh) == 'SpatVector') {
    mesh <- terra::vect(mesh)
  }
  if (!class(shpFeature) == 'SpatVector') {
    shpFeature <- terra::vect(shpFeature)
  }
  
  shpFeature <- terra::project(x = shpFeature, y = mesh) #, crs = terra::crs(mesh, proj = TRUE))
  
  # plot(st_geometry(shpFeature))
  # plot(mesh, add = TRUE)
  # plot(st_geometry(shpFeature), add = TRUE)
  ### clip
  int <- terra::crop(x = mesh,  y = shpFeature) 
  ### pull and return cellIDs
  plot(int)
  getName <- grep(x = toupper(names(int)),   pattern = toupper(cellIDcolumn)) 
  returnIDs <- as.data.frame(int[, getName])
  
  ### which cells were selected (by index)?
  # returnIDs
  cellIndex <- which(as.integer(terra::values(mesh[, grep(x = toupper(names(mesh)),   pattern = toupper(cellIDcolumn))])[, 1]) %in% as.integer(returnIDs[, 1]))
  invisible(list(cellIDs = as.integer(returnIDs[, 1]), 
                 cellIndex = cellIndex,
                 mesh = int[, getName]))
  
}


