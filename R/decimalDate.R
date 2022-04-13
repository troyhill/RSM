#' decimalDate
#'
#' @description Create a decimal date or date/time from a vector. (function credit to EGRET)
#' 
#'  
#' @param  rawData	 vector of dateTimes
#' 
#'
#' @return vector of decimal dates
#'
#'#' @examples
#' 
#' \dontrun{
#' dateTime <- c('1984-02-28 13:56',
#' '1984-03-01 00:00',
#' '1986-03-01 00:00',
#' '1986-10-15 00:00')
#' 
#' decimalDate(dateTime)
#' 
#' dateTime <- c('1984-02-28', '1984-03-01',
#' '1986-03-01', '1986-10-15')
#' 
#' decimalDate(dateTime)
#' 
#' }
#' 
#' 
#' 
#' @export
#' 


decimalDate  <- function (rawData)  {
  dateTime   <- as.POSIXlt(rawData)
  year       <- dateTime$year + 1900
  startYear  <- as.POSIXct(paste0(year, "-01-01 00:00"))
  endYear    <- as.POSIXct(paste0(year + 1, "-01-01 00:00"))
  DecYear    <- year + as.numeric(difftime(dateTime, startYear, 
                                        units = "secs"))/as.numeric(difftime(endYear, startYear, 
                                                                             units = "secs"))
  return(DecYear)
}

