% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/meancoordinates.R
\name{meancoordinates}
\alias{meancoordinates}
\title{Wrapper to use QGIS native:meancoordinates function}
\usage{
meancoordinates(df = NULL, root = NULL)
}
\arguments{
\item{df}{data frame}

\item{root}{root of QGIS installation. By default attemtps to find it}
}
\value{
data frame with mean x and y coordinates
}
\description{
Creates a SpatialPointsDataFrame from a data frame and then uses the
meancoordinates function of QGIS vias RQGIS3 to calculate the mean
coordinates of each territory.
}
