#' getDifference
#'
#' @description gets the difference between two spatial*dataframes
#' 
#' @param  spdf1         baseline. SPDFs can be dataframes or spatial*dataframes
#' @param  spdf2         alternative (positive value = alternative higher than baseline)
#' @param  cellIDcolumn  cellID column, case insensitive. 
#'
#' @return output output is a spatial*dataframe with the geometry of spdf1. Values are differences; spdf2 - spdf1
#'
#' @importFrom sp merge
#' 
#' @export
#' 




getDifference <- function(spdf1, # baseline. SPDFs can be dataframes or spatial*dataframes
                          spdf2, # alternative (positive value = alternative higher than baseline)
                          cellIDcolumn = "cellid_original" # this is case insensitive. 
) {
  cellIDcolumn <- tolower(cellIDcolumn)
  spdf1_ID <- grep(x = tolower(names(spdf1@data)), pattern = cellIDcolumn)
  spdf2_ID <- grep(x = tolower(names(spdf2@data)), pattern = cellIDcolumn)
  
  geometry <- spdf1[, spdf1_ID] # just use geometry from spdf1. easy peasy.
  geometry_ID <- grep(x = tolower(names(geometry@data)), pattern = cellIDcolumn)
  
  tst1 <- as.data.frame(spdf1@data)
  tst1 <- tst1[order(tst1[, spdf1_ID]), ]
  
  tst2 <- as.data.frame(spdf2@data)
  tst2 <- tst2[order(tst2[, spdf2_ID]), ]
  
  cellIDs <- tst1[, spdf1_ID] # this must match polygon data
  tst1 <- tst1[, grep(x = tolower(names(tst1)), pattern = "cellid", invert = TRUE)] # i don't like this "cellid" piece. shouldn't assume it's presence.
  tst2 <- tst2[, grep(x = tolower(names(tst2)), pattern = "cellid", invert = TRUE)]
  
  tst <- tst2 - tst1
  tst[, names(geometry)[geometry_ID]] <- cellIDs  # set cellIDs according to spdf1
  
  tst.spdf <- sp::merge(geometry,  # how does this end up with 2358 rows, more than either layer
                        tst, by = names(geometry)[geometry_ID], 
                        duplicateGeoms = TRUE)
  # tail(spdf2[, 1:5])
  # tail(spdf1[, 1:5])
  # tail(tst.spdf[, 1:5])
  
  invisible(tst.spdf)
}
