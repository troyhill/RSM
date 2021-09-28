#' getDifference
#'
#' @description gets the difference between two spatial*dataframes
#' 
#' @param  spdf1         baseline. SPDFs can be dataframes or spatial*dataframes
#' @param  spdf2         alternative (positive value = alternative higher than baseline)
#' @param  cellIDcolumn  name of column containing cell identifiers. Case insensitive. 
#'
#' @return output output is a spatial*dataframe with the geometry of spdf1. Values are differences (spdf2 - spdf1) where positive values are higher in the alternative spdf2
#'
#' @importFrom sp merge
#' 
#' @export
#' 




getDifference <- function(spdf1, # baseline. SPDFs can be dataframes or spatial*dataframes
                          spdf2, # alternative (positive value = alternative higher than baseline)
                          cellIDcolumn = "objectid" # this is case insensitive. 
) {
  cellIDcolumn <- tolower(cellIDcolumn)
  spdf1_ID <- grep(x = tolower(names(spdf1@data)), pattern = cellIDcolumn)
  spdf2_ID <- grep(x = tolower(names(spdf2@data)), pattern = cellIDcolumn)
  
  geometry_obj <- spdf1[, spdf1_ID] # just use geometry from spdf1. easy peasy.
  geometry_ID <- grep(x = tolower(names(geometry_obj@data)), pattern = cellIDcolumn)
  
  tst1 <- as.data.frame(spdf1@data)
  tst1 <- tst1[order(tst1[, spdf1_ID]), ]
  
  tst2 <- as.data.frame(spdf2@data)
  tst2 <- tst2[order(tst2[, spdf2_ID]), ]
  
  cellIDs <- tst1[, spdf1_ID] # this must match polygon data
  tst1 <- tst1[, grep(x = tolower(names(tst1)), pattern = cellIDcolumn, invert = TRUE)] 
  tst2 <- tst2[, grep(x = tolower(names(tst2)), pattern = cellIDcolumn, invert = TRUE)]
  
  tst <- tst2 - tst1 # is this a cell-wise subtraction?
  tst[, names(geometry_obj)[geometry_ID]] <- cellIDs  # set cellIDs according to spdf1
  
  column_select_tst  <- which(names(tst) %in% names(geometry_obj)) # which column in tst has cell IDs
  column_select_geom <- which(names(geometry_obj) %in% names(tst)) # which column in tst has cell IDs
  ### add data back into spdf
  geometry_obj@data <- data.frame(geometry_obj@data, tst[match(geometry_obj@data[, column_select_geom], tst[, column_select_tst]),])
  ### remove extra column ID column...
  geometry_obj@data <- geometry_obj@data[, -ncol(geometry_obj@data)]
  
  invisible(geometry_obj)
}
