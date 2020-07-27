#' getDateVector
#'
#' @description This function generates a vector of dates from the timestamps of an input NetCDF file. 
#' 
#'  
#' @param  ncdf       an ncdf4 object (must already be loaded into environment using ncdf4 package)
#' @param  varName    name of timestamp variable in the ncdf4 object
#'
#' @return a POSIXlt vector of dates 
#'
#' @importFrom ncdf4     ncvar_get
#' 
#' @export
#' 

### add RTools directory to PATH 
# Sys.getenv("PATH")
# old_path <- Sys.getenv("PATH") 
# Sys.setenv(PATH = paste(old_path, "C:\\Rtools\\bin;C:\\Rtools\\gcc-4.6.3\\bin;C:\\Rtools\\perl\\bin;C:\\Rtools\\MinGW\\bin", sep = ";"))


getDateVector <- function(ncdf, varName = "timestamps") {
  ### returns a posixlt vector of dates from a netCDF (converting units from "days from XXXX-XX-XX")
  tdstr   <- strsplit(unlist(strsplit(ncatt_get(ncdf, varName, "units")$value, " "))[3], "-")
  dateVec <- as.POSIXlt(as.POSIXlt(paste(as.integer(unlist(tdstr)[1]), 
                                         as.integer(unlist(tdstr)[2]), 
                                         as.integer(unlist(tdstr)[3]), sep = "-"), format = "%Y-%m-%d") + 
                          ncdf4::ncvar_get(ncdf, varName)*60*60*24)
  
}


