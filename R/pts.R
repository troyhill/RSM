#' @title Points of interest: Everglades water level monitoring
#'
#' @description A SpatialPointsDataFrame with eight points
#'
#' \itemize{
#'   \item gage      gage name
#'   \item region    region where station is located
#'   \item field.test.threshold  a water level depth threshold for concern
#'   \item NAVD_ft   station elevation
#'   \item Notes     notes
#'   }
#' @name pts
#' @details 
#' ### to create object: 
#' # pts <- do.call(rbind, lapply(X = c("SITE_62", "SITE_63", 
#' #  "SITE_64", "SITE_65", '3AS3W1'), FUN = SFNRC::getCoords_EDEN))
#'  # tst <- pts
#'  # pts <- wrap(tst)
#'  # save(pts, file = 'data/pts.RData')
#' # tools::resaveRdaFiles(list.files("data/", full.names= TRUE),compress="xz")
#' # tools::checkRdaFiles("data/")
#' 
#' @format A SpatVector
NULL