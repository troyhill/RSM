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
#' #pts        <- read.delim("L:/Restoration Assessments/Non CERP Projects/COP/Position_analysis/
#' # data/data_drought_gage_table_20200109.txt")
#' #coordinates(pts) <- c("UTM_east", "UTM_north")
#' #proj4string(pts) <- proj4string(IRMap[[2]]) # "simulation netcdf data") 
#' #save(pts, file = paste0(getwd(), "/data/data_pts_20200316.RData"))
#' 
#' @format A SpatialPointsDataFrame
NULL