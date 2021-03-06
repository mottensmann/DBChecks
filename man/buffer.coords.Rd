% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/buffer.coords.R
\name{buffer.coords}
\alias{buffer.coords}
\title{buffer around point}
\usage{
buffer.coords(input = NULL, radius = 500, out = NULL, root = "C:/OSGeo4W64")
}
\arguments{
\item{input}{shape file with points}

\item{radius}{integer}

\item{out}{shape file}

\item{root}{root of QGIS installation. By default attemtps to find it}
}
\description{
creates a buffer of radius n around points within a shape
}
\details{
radius within units of the coordinate system (i.e. apply using UTM32N)
}
