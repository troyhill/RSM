#' getCellIDs
#'
#' @description Extracts cell IDs from a model mesh for each indicator region. Currently only supports spatialPolygonDataframes; spatialLines and spatialPoints have not been tested.
#' 
#'  
#' @param  modelMesh        a SpatVector with model mesh cells and cell ID data (or, a character vector of the file address to be imported internally via rgdal::readOGR)
#' @param  indicatorRegions          a SpatVector with indicator region data
#' @param  returnType       specifies whether centroid points or partial mesh polygons are returned.
#' @param modelName      name of the model mesh used (e.g., rsm, COP)
#' @param versionName    name of the model version used (e.g., WERP)
#'
#' @return a dataframe with cell IDs, the corresponding indicator region names and other data. Specifically, the following columns must be present: from the indicator region data: "INDICATOR", "NAME". Columns required in the mesh data: "CellId", "Node1", "Node2", "Node3", "Topo_avg", "landuse"
#'
#' @importFrom terra     vect
#' @importFrom graphics  par
#' @importFrom terra     project
#' @importFrom terra     crs
#' @importFrom terra     plot
#' @importFrom terra     centroids
#' @importFrom terra     intersect
#' 
#' @export
#' 

### add RTools directory to PATH 
# Sys.getenv("PATH")
# old_path <- Sys.getenv("PATH") 
# Sys.setenv(PATH = paste(old_path, "C:\\Rtools\\bin;C:\\Rtools\\gcc-4.6.3\\bin;C:\\Rtools\\perl\\bin;C:\\Rtools\\MinGW\\bin", sep = ";"))

getCellIDs <- function(modelMesh, indicatorRegions = RSM::IRMap[[2]], returnType = "polygons",
                       modelName = NA, versionName = NA) {
  ### function to generate a list of cell IDs from (1) a spatialPolygonDataFrame of indicator regions, and (2) a shapefile with a model mesh
  ### TODO: add options to restrict criteria for qualifying cells - any fraction falling within an IR (current protocol; cells are clipped to IR extents), 
  ### (2) based on cell centroids (cells are *not* currently clipped to IR extents)
  # modelMesh <-  sf::read_sf(system.file("extdata/gis/nsrsm_v352", "nsrsm_mesh_landuse.shp", package="RSM"),"nsrsm_mesh_landuse")
  
  if (!any(grepl(x = class(modelMesh), pattern = 'SpatVector'))) {
    modelMesh <- terra::vect(modelMesh)
  }
  if (!any(grepl(x = class(indicatorRegions), pattern = 'SpatVector'))) {
    indicatorRegions <- terra::vect(indicatorRegions)
  }
  if(is.character(modelMesh)) {
    modelMesh <- terra::vect(modelMesh) # paste(getwd(), "inst/extdata/gis/nsrsm_v352/nsrsm_mesh_landuse.shp", sep = "/"))
    # meshDat <-  sf::read_sf(modelMesh) #system.time(test <- read_sf(dsn = dsn, layer = lay))
  } 
  if (grepl(x = class(modelMesh), pattern = "SpatVector")) {
    meshDat <- modelMesh
  } else {
    stop("modelMesh input is not supported. Object must be an sf or spatialPolygonDataframe object representing the model mesh cells or a character vector of the address of the shapefile to be imported")
  }
  IRDat   <- terra::project(x = indicatorRegions, y = terra::crs(meshDat, proj = TRUE))
  # IRDat   <- sp::st_transform(x = indicatorRegions, crs = sf::st_crs(meshDat))
  # plot(meshDat)
  # plot(IRDat, add = TRUE, col = "red")
  # modelOutput         <- nc_open(paste(baseDir, modelVec, modelVersion, modelAlt, 'globalmonitors.nc', sep='/'))
  
  if (returnType %in% "points") {
    meshDat_pts  <- terra::centroids(x = meshDat) 
    IRs_only     <- terra::intersect(x = IRDat, y = meshDat_pts) #, function(z) if (length(z)==0) NA_integer_ else z[1])
    
    # meshDat_pts <- rgeos::gCentroid(meshDat, byid=TRUE) # removes data. need to add back in via merge. Too elaborate.
    # meshDat_pts <- sf::st_centroid(x = meshDat, of_largest_polygon = FALSE)
    # meshDat_pts <- sp::SpatialPointsDataFrame(rgeos::gCentroid(meshDat, byid=TRUE), 
    #                                           meshDat@data, match.ID=FALSE)
    # meshDat_pts <- sf::st_sf(sf::st_centroid(x = meshDat, of_largest_polygon = FALSE), 
    #                                           meshDat@data, match.ID=FALSE)
    
    ### TODO (low priority): identify/return polygons whose centroids fall inside the indicator regions (rather than returning the centroids themselves)
    # IRs_only    <- raster::intersect(y = IRDat, x = meshDat_pts) 
    # IRs_only    <- terra::intersect(y = IRDat, x = meshDat_pts) 
  } else if (returnType %in% "polygons") {
    IRs_only         <- terra::intersect(x = IRDat, y = meshDat) #, function(z) if (length(z)==0) NA_integer_ else z[1])
    # IRs_only <- terra::intersect(y = IRDat, x = meshDat) 
  } else {
    stop("overlay argument must be 'any' or 'centroid'. see ?getCellIDs for more detail.")
  }
  
  ### this nomenclature might differ. create new columns
  IRs_only$model   <- modelName
  IRs_only$version <- versionName
  
  graphics::par(mar= c(0,0,0,0))
  terra::plot(IRDat, col = "skyblue")
  terra::plot(IRs_only, pch = 19, cex = 0.4, add = TRUE)
  
  ### wtf is this?
  name_order <- c("model", "version", "INDICATOR", "CellId", "NAME", "Node1", "Node2", "Node3", "Topo_avg", "landuse")
  IRs_only <- IRs_only[, which(names(IRs_only) %in% name_order), drop = FALSE]
  invisible(IRs_only)
  # invisible(IRs_only[, c("model", "version", "INDICATOR", "CellId", "NAME", "Node1", "Node2", "Node3", "Topo_avg", "landuse")])
}


