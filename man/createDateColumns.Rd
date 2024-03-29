% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/createDateColumns.R
\name{createDateColumns}
\alias{createDateColumns}
\title{createDateColumns}
\usage{
createDateColumns(rawData)
}
\arguments{
\item{rawData}{vector of dateTimes}
}
\value{
dataframe
}
\description{
Creates various date columns (function credit to EGRET)
}
\examples{

\dontrun{
dateTime <- c('1984-02-28 13:56',
'1984-03-01 00:00',
'1986-03-01 00:00',
'1986-10-15 00:00')

expandedDateDF <- populateDateColumns(dateTime)
expandedDateDF

dateTime <- c('1984-02-28', '1984-03-01',
'1986-03-01', '1986-10-15')

expandedDateDF <- populateDateColumns(dateTime)
expandedDateDF

}



}
