% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{format_dates}
\alias{format_dates}
\title{Convert date/time variables to POSIXct}
\usage{
format_dates(x, date_col, tz = NULL, date_only = FALSE)
}
\arguments{
\item{x}{character vector of date/time data}

\item{date_col}{character of column names to be converted}

\item{tz}{optional string with a properly formatted time zone; if given, will
override the default time zone assignments for EDW and MBO data}

\item{date_only}{logical indicating if there is only a date without time}
}
\value{
A tibble
}
\description{
Converts date/time variables to POSIXct and adjusts to US/Central timezone.
}
