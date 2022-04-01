#' getDifference
#'
#' @description gets the difference between two SpatVector objects.
#' 
#' @param  spdf1         baseline. SPDFs can be dataframes or spatial*dataframes
#' @param  spdf2         alternative (positive value = alternative higher than baseline)
#' @param  valueName     name of column containing the quantity to compare. Should be the same in both SpatVector objects. If this argument is NULL, the first column is used
#'
#' @return output output is a SpatVector with the geometry of spdf1. Values are differences (spdf2 - spdf1) where positive values are higher in the alternative spdf2
#'
#' @examples
#' 
#' \dontrun{
#' }
#' 
#' 
#' @export
#' 




getDifference <- function(spdf1, # baseline. SPDFs can be dataframes or spatial*dataframes
                          spdf2, # alternative (positive value = alternative higher than baseline)
                          valueName = NULL # case insensitive, if provided
) {
  
  
}
