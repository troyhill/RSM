#' getFeatureIDs
#'
#' @description Extracts cell IDs from a model mesh for each indicator region. Currently only supports spatialPolygonDataframes; spatialLines and spatialPoints have not been tested.
#' 
#'  
#' @param  shpFeature       a spatial*Dataframe for the indicator region, transect, point
#' @param  mesh             Shapefile with model mesh cells and cell ID data (mesh must have a column named "CellId")
#'
#' @return a list with two elements: a vector of cell IDs, and (2) a spatial dataframe with the intersection of the mesh and the input shapefile
#'
#' @importFrom sp     spTransform
#' @importFrom raster intersect
#' @importFrom raster crs
#' @importFrom rgdal  readOGR
#' 
#' @export
#' 

### add RTools directory to PATH 
# Sys.getenv("PATH")
# old_path <- Sys.getenv("PATH") 
# Sys.setenv(PATH = paste(old_path, "C:\\Rtools\\bin;C:\\Rtools\\gcc-4.6.3\\bin;C:\\Rtools\\perl\\bin;C:\\Rtools\\MinGW\\bin", sep = ";"))


getFeatureIDs <- function(shpFeature, # = as(fireHydro::BICY_EVER_PlanningUnits,  "Spatial"), # test that this works on points, lines, polygons. MUST BE A spatial*DF  
                          mesh = rgdal::readOGR(system.file("extdata/gis/nsrsm_v352", "nsrsm_mesh_landuse.shp", package="RSM"),"nsrsm_mesh_landuse")) { # mesh must have a column named "CellId"
  ### make CRS of shpFeature == mesh
  shpFeature <- sp::spTransform(shpFeature, raster::crs(mesh))
  ### clip
  int        <- raster::intersect(x = mesh,  y = shpFeature) 
  ### pull and return cellIDs
  plot(int)
  getName   <- grep(x = toupper(names(int)),   pattern = "CELLID")  
  returnIDs <- as.data.frame(int[, getName]@data)
  invisible(list(cellIDs = returnIDs[, 1], spdf = int[, getName]))
}


