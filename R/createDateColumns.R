#' createDateColumns
#'
#' @description Creates various date columns (function credit to EGRET)
#' 
#'  
#' @param  rawData	 vector of dateTimes
#' 
#'
#' @return dataframe
#'
#' @examples
#' 
#' \dontrun{
#' dateTime <- c('1984-02-28 13:56',
#' '1984-03-01 00:00',
#' '1986-03-01 00:00',
#' '1986-10-15 00:00')
#' 
#' expandedDateDF <- populateDateColumns(dateTime)
#' expandedDateDF
#' 
#' dateTime <- c('1984-02-28', '1984-03-01',
#' '1986-03-01', '1986-10-15')
#' 
#' expandedDateDF <- populateDateColumns(dateTime)
#' expandedDateDF
#' 
#' }
#' 
#' 
#' 
#' @export
#' 


createDateColumns  <- function (rawData)  {
  DateFrame        <- as.data.frame(matrix(ncol = 1, nrow = length(rawData)))
  colnames(DateFrame) <- c("Date")
  DateFrame$Date   <- rawData
  dateTime         <- as.POSIXlt(rawData)
  DateFrame$Julian <- as.numeric(julian(dateTime, origin = as.Date("1850-01-01")))
  DateFrame$Month  <- dateTime$mon + 1
  DateFrame$Day    <- dateTime$yday + 1
  year   <- dateTime$year + 1900
  hour   <- dateTime$hour
  minute <- dateTime$min
  if (sum(hour) == 0 & sum(minute) == 0) {
    dateTime$hour <- rep(12, length(dateTime))
  }
  leapOffset      <- ifelse((year%%4 == 0) & ((year%%100 != 0) | 
                                           (year%%400 == 0)), 0, 1)
  DateFrame$Day[DateFrame$Day > 59] <- DateFrame$Day[DateFrame$Day > 
                                                       59] + leapOffset[DateFrame$Day > 59]
  DateFrame$DecYear   <- decimalDate(dateTime)
  DateFrame$MonthSeq  <- ((year - 1850) * 12) + DateFrame$Month
  DateFrame$waterYear <- as.integer(DateFrame$DecYear)
  DateFrame$waterYear[DateFrame$Month %in% c(10:12)] <- DateFrame$waterYear[DateFrame$Month %in% 
                                                                              c(10:12)] + 1
  return(DateFrame) # decimalDate
}

