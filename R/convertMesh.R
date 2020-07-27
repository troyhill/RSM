#' convertMesh
#'
#' @description Converts cell meshes using cellID centroids from meshToConvert and returns cellIDs from meshToMatch.
#' 
#'  
#' @param  meshToMatch      spatialpolygons to be returned. Must have "cellID" column.
#' @param  meshToConvert    cellIDs to be returned.  Must have "cellID" column.
#' @param  returnOriginalCellIDs should MeshToMatch cellIDs be returned?
#' @param  output           type of output desired. can be either "spdf" or "df"
#'
#' @return a spatialpolygondataframe or dataframe with the cellIDs in meshToMatch that 
#'
#' @importFrom sp    over
#' @importFrom rgeos gCentroid
#' 
#' @export
#' 


convertMesh <- function(meshToMatch, # polygons to be returned. Must have "cellID" column.
                        meshToConvert, # cellIDs to be returned.  Must have "cellID" column.
                        returnOriginalCellIDs = FALSE, # should MeshToMatch cellIDs be returned?
                        output = "spdf") { # "spdf" or "df" 
  ### map cellIDs based on centroid.
  ### function allows two disparate meshes to be made consistent with each other.  
  ### best if the input meshes are simple - containing just a cellID column
  # identify CellID columns
  cellIDs_match   <- grep(x = toupper(names(meshToMatch)),   pattern = "CELLID")  
  cellIDs_convert <- grep(x = toupper(names(meshToConvert)), pattern = "CELLID")  
  
  ### find polygon (in meshToMatch) closest to each MeshToConvert centroid. 
  cellCentroid <- rgeos::gCentroid(meshToMatch, byid=TRUE) # every centroid needs cellID from meshToConvert
  # summary(apply(gDistance(cellCentroid, meshToConvert, byid=TRUE),2,min)) # some cells are excluded - distances > 0
  test    <- sp::over(cellCentroid, meshToConvert) # each row is a feature in meshToMatch
  outMesh <- meshToMatch
  outMesh@data <- test # length(unique(test$CellId)) << nrow(test) # some cells from meshToMatch are used in multiple meshToConvert cells
  
  if (returnOriginalCellIDs) {
    outMesh@data$CellID_original <- meshToMatch@data[, cellIDs_match]
  }
  if (output == "df") {
    retDat <- data.frame(outMesh@data)
  } else if (output == "spdf") {
    retDat <- outMesh
  } else {
    stop("output type not recognized. Output can be either a dataframe ('df') or spatialpolygondataframe ('spdf') ")
  }
  invisible(retDat)
}
