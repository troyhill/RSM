#' getFeatureIDs
#'
#' @description Extracts cell IDs from a model mesh for each indicator region. Currently only supports sf objects
#' 
#'  
#' @param  shpFeature       an sf object with the indicator region, transect, point
#' @param  mesh             sf object with model mesh cells and cell ID data (mesh must have a column named "CellId")
#' @param  cellIDcolumn     name of column containing cell identifiers. Case insensitive.
#' @param returnType        class of spatial data returned. Can be 'sf' or 'spatVector' (default is the latter)
#'
#' @return a list with two elements: a vector of cell IDs, and (2) a spatial object (sf or spatVector) with the intersection of the mesh and the input shapefile
#'
#' @importFrom sf     st_transform
#' @importFrom sf     st_crs
#' @importFrom terra  crop
#' @importFrom terra  vect
#' @importFrom sf     st_as_sf
#' 
#' @export
#' 

### add RTools directory to PATH 
# Sys.getenv("PATH")
# old_path <- Sys.getenv("PATH") 
# Sys.setenv(PATH = paste(old_path, "C:\\Rtools\\bin;C:\\Rtools\\gcc-4.6.3\\bin;C:\\Rtools\\perl\\bin;C:\\Rtools\\MinGW\\bin", sep = ";"))


getFeatureIDs <- function(shpFeature, # = fireHydro::BICY_EVER_PlanningUnits, # test that this works on points, lines, polygons. MUST BE A spatial*DF  
                          mesh = sf::read_sf(system.file("extdata/gis/nsrsm_v352", "nsrsm_mesh_landuse.shp", package="RSM"),"nsrsm_mesh_landuse"),# mesh must have a column named "CellId"
                          cellIDcolumn = "CellId",
                          returnType = 'sf') { 
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
  
  shpFeature <- sf::st_transform(x = shpFeature, crs = sf::st_crs(mesh))
  
  # plot(st_geometry(shpFeature))
  # plot(mesh, add = TRUE)
  # plot(st_geometry(shpFeature), add = TRUE)
  ### clip
  int <- terra::crop(x = terra::vect(mesh),  y = terra::vect(shpFeature)) 
  ### pull and return cellIDs
  plot(int)
  getName <- grep(x = toupper(names(int)),   pattern = toupper(cellIDcolumn)) 
  returnIDs <- as.data.frame(int[, getName])
  
  if (returnType = 'sf') { 
    int <- sf::st_as_sf(int)
  }
  invisible(list(cellIDs = as.numeric(returnIDs[, 1]), mesh = int[, getName]))
  
}


