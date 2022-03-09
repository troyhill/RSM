#' convertMesh
#'
#' @description Converts cell meshes using cellID centroids from meshToConvert and returns cellIDs from meshToMatch.
#' 
#'  
#' @param  meshToMatch      spatialpolygons to be returned. Must have "cellID" column.
#' @param  meshToConvert    cellIDs to be returned.  Must have "cellID" column.
#' @param  returnOriginalCellIDs should MeshToMatch cellIDs be returned?
#' @param  joinBy           name of column used to link meshes
#' @param  output           type of output desired. can be either "sf" or "df"
#'
#' @return an sf or dataframe with the cellIDs in meshToMatch that 
#'
#' @importFrom sf    st_centroid
#' @importFrom sf    st_intersects
#' 
#' @export
#' 


convertMesh <- function(meshToMatch, # polygons to be returned. Must have "cellID" column.
                        meshToConvert, # cellIDs to be returned.  Must have "cellID" column.
                        returnOriginalCellIDs = FALSE, # should MeshToMatch cellIDs be returned?
                        joinBy = 'cellID',
                        output = "spdf") { # "spdf" or "df" 
  # meshToConvert <-  sf::read_sf(system.file("extdata/gis/nsrsm_v352", "nsrsm_mesh_landuse.shp", package="RSM"),"nsrsm_mesh_landuse")
  # meshToMatch <- sf::st_crop(sf::read_sf(system.file("extdata/gis/nsrsm_v352", "nsrsm_mesh_landuse.shp", package="RSM"),"nsrsm_mesh_landuse"), 
  #                       y = sf::st_transform(x = fireHydro::BICY_EVER_PlanningUnits, crs = sf::st_crs(meshToMatch)))

  ### map cellIDs based on centroid.
  ### function allows two disparate meshes to be made consistent with each other.  
  ### best if the input meshes are simple - containing just a cellID column
  # identify CellID columns
  cellIDs_match   <- grep(x = toupper(names(meshToMatch)),   pattern = toupper(joinBy))  
  cellIDs_convert <- grep(x = toupper(names(meshToConvert)), pattern = toupper(joinBy))  
  
  ### find polygon (in meshToMatch) closest to each MeshToConvert centroid. 
  # cellCentroid <- rgeos::gCentroid(meshToMatch, byid=TRUE) # every centroid needs cellID from meshToConvert
  # test    <- sp::over(cellCentroid, meshToConvert) # each row is a feature in meshToMatch
  ### possible replacements, need to be tested:
  cellCentroid <- sf::st_centroid(x = meshToMatch, of_largest_polygon = FALSE) # every centroid needs cellID from meshToConvert
  test         <- sapply(sf::st_intersects(x = cellCentroid, y = meshToConvert), function(z) if (length(z)==0) NA_integer_ else z[1])
  test         <- test[!is.na(test)]
  outMesh      <- meshToMatch[which(!is.na(test)),]
  plot(outMesh)
  outMesh$newCellID <- test[!is.na(test)] # length(unique(test$CellId)) << nrow(test) # some cells from meshToMatch are used in multiple meshToConvert cells
  
  # nrow(outMesh) == nrow(meshToMatch)
  if (returnOriginalCellIDs) {
    # outMesh@data$CellID_original <- meshToMatch@data[, cellIDs_match]
    outMesh$CellID_original <- meshToMatch[, cellIDs_match]
  }
  if (grepl(x = output, pattern = "df|dataframe")) {
    retDat <- data.frame(outMesh)
  } else if (grepl(x = output, pattern = "sf")) {
    retDat <- outMesh
  } else {
    stop("output type not recognized. Output can be either a dataframe ('df') or sf")
  }
  invisible(retDat)
}
