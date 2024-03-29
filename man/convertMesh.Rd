% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convertMesh.R
\name{convertMesh}
\alias{convertMesh}
\title{convertMesh}
\usage{
convertMesh(
  meshToMatch,
  meshToConvert,
  returnOriginalCellIDs = FALSE,
  joinBy = "cellID",
  output = "SpatVector"
)
}
\arguments{
\item{meshToMatch}{polygon mesh to be subset. Must have "cellID" column.}

\item{meshToConvert}{polygon mesh to be returned.  Must have "cellID" column.}

\item{returnOriginalCellIDs}{should MeshToMatch cellIDs be returned?}

\item{joinBy}{name of column used to link meshes}

\item{output}{type of output desired. can be either "SpatVector" or "df" for dataframe}
}
\value{
a SpatVector or dataframe with the cellIDs in meshToMatch that
}
\description{
Converts cell meshes using cellID centroids from meshToConvert and returns cellIDs from meshToMatch.
}
