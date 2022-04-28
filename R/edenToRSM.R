#' edenToRSM
#'
#' @description Coarsen eden data to match RSM mesh cells. Function extracts data are mesh cell locations and returns a SpatVector object with the result.
#' 
#'  
#' @param  data   Input data, either SpatRaster or eden class. 
#' @param  mesh       Geometry used for output. A SpatVector with mesh cells.
#' @param  maskToEDEN   Should data be masked to the EDEN domain? If FALSE the entire mesh is returned, including cells outside EDEN grid. If TRUE, mesh is masked to EDEN grid.
#' @param  func       Function to apply when multiple EDEN cells are within a mesh cell. Typically, a measure of central tendency.
#'
#' @return Depending on the class of the input data, either a SpatVector or an eden object (list with date and data elements)
#'
#' @examples
#' 
#' \dontrun{
#' edenDat <- fireHydro::getEDEN(returnType = 'terra', quarterly = TRUE)
#' 
#' eden_mesh <- edenToRSM(data = edenDat, maskToEDEN = TRUE)
#' plot(eden_mesh$data, ncol(eden_mesh$data), axes = FALSE, type = 'continuous')
#' }
#' 
#' 
#' @importFrom terra rast
#' @importFrom terra extract
#' @importFrom terra subset
#' @importFrom terra crop
#' @importFrom terra mask
#' @importFrom terra as.polygons 
#' @importFrom terra classify
#' 
#' @export
#' 


edenToRSM <- function(data, 
                      mesh =  vect(system.file("extdata/gis/COP_mesh", "mesh.shp", package="RSM"),"mesh"),
                      maskToEDEN = FALSE, # if FALSE the entire mesh is returned, including cells outside EDEN grid. If TRUE, mesh is masked to EDEN grid.
                      func = mean) {
  ### function converts EDEN spatRaster to RSM mesh resolution
  ### applies func when multiple EDEN cells are within a mesh cell
  convertBack <- FALSE # should data be converted back to eden object?
  if (any(class(data) %in% 'eden')) {
    EDENdate    <- data$date
    data    <- data$data
    convertBack <- TRUE
  }
  if (any(class(data) %in% 'SpatVector')) {
    ### if EDEN data are a SpatVector, do not proceed; assume they have already
    ### been converted to RSM grid. Although they may just have been polygonized.
    ### TODO: check if cells are identical? need to mask them both to common extent before checking?
    data <- terra::rast(data * 1)
  }
  
  if (!any(class(data) %in% 'SpatRaster')) {
    data <- terra::rast(data * 1)
  }
  message('Upscaling EDEN grid data to match RSM mesh. This may take a while.\n')
  ### produces data frame
  data2    <- terra::extract(x = data, y = mesh, fun = func, na.rm = TRUE)
  message('Upscaling done.\n')
  ### merge dataframe with mesh
  all_sds      <- mesh
  values(all_sds) <- data2[, 2:ncol(data2)] # first column is an ID variable
  
  if(maskToEDEN == TRUE) {
    eden_domain <- terra::as.polygons(terra::classify(terra::subset(x = data, subset = 1), cbind(-Inf, Inf, 1)), dissolve=TRUE)
    # plot(mask(crop(all_sds, eden_domain), eden_domain), 5)
    all_sds <- terra::mask(terra::crop(all_sds, eden_domain), eden_domain)
  }
  
  if (convertBack == TRUE) {
    ### convert back to eden object, if one was provided as input
    ### NOTE: some of the eden S3 methods may break when fed a spatvector?
    all_sds <- list(date = EDENdate,
                    data = all_sds)
    class(all_sds) <- c("eden", grep(x = class(all_sds), pattern = "eden", invert = TRUE, value = TRUE))
  }
  return(all_sds)
}


