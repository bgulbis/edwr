% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/warfarin.R
\docType{data}
\name{warfarin}
\alias{warfarin}
\title{Sample warfarin goal data}
\format{A data frame with sample warfarin data, including indication,
  duration, and goal INR range. The data has been read in via
  \code{\link{read_data}} but no further tidying or transformation has been
  performed.
\describe{
  \item{pie.id}{encounter identifier}
  \item{warfarin.datetime}{date/time warfarin data was entered}
  \item{warfarin.event}{type of warfarin data being documented}
  \item{warfarin.result}{value of the data}
}}
\usage{
warfarin
}
\description{
A dataset containing sample warfarin goal data
}
\keyword{datasets}
