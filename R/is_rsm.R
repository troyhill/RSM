#' Check whether an object is of the `rsm` class
#'
#' Checks object to see if it is an `rsm` object. An `rsm` object is a list with elements `cellIDs`, `cellsToUse`, `mesh`, `dates`, and `stage`
#'
#' @param x object to check
#' @export
#' @return logical
#' @examples
#' is.rsm(month.abb) # FALSE
#' 
#' @export



is.rsm <- function(x) {
  
  return(any(class(x) %in% 'rsm'))
}
