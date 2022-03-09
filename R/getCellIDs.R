#' getCellIDs
#'
#' @description Extracts cell IDs from a model mesh for each indicator region. Currently only supports spatialPolygonDataframes; spatialLines and spatialPoints have not been tested.
#' 
#'  
#' @param  modelMesh        a spatialPolygonDataframe with model mesh cells and cell ID data (or, a character vector of the file address to be imported internally via rgdal::readOGR)
#' @param  indicatorRegions          a spatialPolygonDataframe with indicator region data
#' @param  overlay       specifies the method used to identify cells. "any" identifies cells that have any fraction falling within an indicator region (default protocol; cells are clipped to IR extents), "centroid" identifies included cells based on whether their centroid falls within an IR (cells are not currently clipped to IR extents) 
#' @param modelName      name of the model mesh used (e.g., rsm, COP)
#' @param versionName    name of the model version used (e.g., WERP)
#'
#' @return a dataframe with cell IDs, the corresponding indicator region names and other data. Specifically, the following columns must be present: from the indicator region data: "INDICATOR", "NAME". Columns required in the mesh data: "CellId", "Node1", "Node2", "Node3", "Topo_avg", "landuse"
#'
#' @importFrom rgdal  readOGR
#' @importFrom graphics par
#' @importFrom sp     spTransform
#' @importFrom sp     SpatialPointsDataFrame
#' @importFrom sp     plot
#' @importFrom rgeos  gCentroid
#' @importFrom raster intersect
#' @importFrom raster crs
#' 
#' @export
#' 

### add RTools directory to PATH 
# Sys.getenv("PATH")
# old_path <- Sys.getenv("PATH") 
# Sys.setenv(PATH = paste(old_path, "C:\\Rtools\\bin;C:\\Rtools\\gcc-4.6.3\\bin;C:\\Rtools\\perl\\bin;C:\\Rtools\\MinGW\\bin", sep = ";"))

getCellIDs <- function(modelMesh, indicatorRegions = RSM::IRMap[[2]], overlay = "any",
                       modelName, versionName) {
  ### function to generate a list of cell IDs from (1) a spatialPolygonDataFrame of indicator regions, and (2) a shapefile with a model mesh
  ### TODO: add options to restrict criteria for qualifying cells - any fraction falling within an IR (current protocol; cells are clipped to IR extents), 
  ### (2) based on cell centroids (cells are *not* currently clipped to IR extents)
  
  if(is.character(modelMesh)) {
    meshDat <- rgdal::readOGR(modelMesh) # paste(getwd(), "inst/extdata/gis/nsrsm_v352/nsrsm_mesh_landuse.shp", sep = "/"))
    # meshDat <-  sf::read_sf(modelMesh) #system.time(test <- read_sf(dsn = dsn, layer = lay))
  } else if (grepl(x = class(modelMesh), pattern = "sf|SpatialPolygonsDataFrame")) {
    meshDat <- modelMesh
  } else {
    stop("modelMesh input is not supported. Object must be an sf or spatialPolygonDataframe object representing the model mesh cells or a character vector of the address of the shapefile to be imported")
  }
  IRDat   <- sp::spTransform(indicatorRegions, raster::crs(meshDat))
  # IRDat   <- sp::st_transform(x = indicatorRegions, crs = sf::st_crs(meshDat))
  # plot(meshDat)
  # plot(IRDat, add = TRUE, col = "red")
  # modelOutput         <- nc_open(paste(baseDir, modelVec, modelVersion, modelAlt, 'globalmonitors.nc', sep='/'))
  
  if (overlay %in% "centroid") {
    meshDat_pts <- rgeos::gCentroid(meshDat, byid=TRUE) # removes data. need to add back in via merge. Too elaborate.
    # meshDat_pts <- sf::st_centroid(x = meshDat, of_largest_polygon = FALSE)
    meshDat_pts <- sp::SpatialPointsDataFrame(rgeos::gCentroid(meshDat, byid=TRUE), 
                                              meshDat@data, match.ID=FALSE)
    # meshDat_pts <- sf::st_sf(sf::st_centroid(x = meshDat, of_largest_polygon = FALSE), 
    #                                           meshDat@data, match.ID=FALSE)
    
    ### TODO (low priority): identify/return polygons whose centroids fall inside the indicator regions (rather than returning the centroids themselves)
    IRs_only    <- raster::intersect(y = IRDat, x = meshDat_pts) 
    # IRs_only    <- terra::intersect(y = IRDat, x = meshDat_pts) 
  } else if (overlay %in% "any") {
    IRs_only <- raster::intersect(y = IRDat, x = meshDat)
    # IRs_only <- terra::intersect(y = IRDat, x = meshDat) 
  } else {
    stop("overlay argument must be 'any' or 'centroid'. see ?getCellIDs for more detail.")
  }
  
  ### this nomenclature might differ. create new columns
  IRs_only@data$model   <- modelName
  IRs_only@data$version <- versionName
  
  graphics::par(mar= c(0,0,0,0))
  # sf::plot(IRDat, col = "skyblue")
  # sf::plot(IRs_only, pch = 19, cex = 0.4, add = TRUE)
  
  invisible(IRs_only@data[, c("model", "version", "INDICATOR", "CellId", "NAME", "Node1", "Node2", "Node3", "Topo_avg", "landuse")])
}


