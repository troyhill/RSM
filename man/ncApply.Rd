% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nc_apply.R
\name{ncApply}
\alias{ncApply}
\title{ncApply}
\usage{
ncApply(
  data,
  cellIDs = "all",
  dates,
  returnSpatial = FALSE,
  spdf = NULL,
  yearBegin = 5,
  yearlength = 12,
  func
)
}
\arguments{
\item{data}{target object. function is applied to each row.}

\item{cellIDs}{which cellIDs to use? A numeric vector or "all"}

\item{dates}{a POSIXlt vector of dates}

\item{returnSpatial}{if TRUE, a joined spdf is returned. If FALSE, a dataframe is returned           type of output desired. can be either "spdf" or "df"}

\item{spdf}{the spdf to join}

\item{yearBegin}{first month of year}

\item{yearlength}{length of "year" (units = months)}

\item{func}{function to apply to each year and each cell}
}
\value{
output is a vector with the value returned by "func" applied to each year
}
\description{
applies a function to a netCDF object
}
