#' Sample location data
#'
#' A dataset containing sample hospital location data
#'
#' @format A data frame with sample hospital location data, including the
#'   arrival and departure time from each hospital unit. The data has been read
#'   in via \code{\link{read_data}} but no further tidying or transformation has
#'   been performed.
#' \describe{
#'   \item{pie.id}{encounter identifier}
#'   \item{unit.from}{hospital unit patient is coming from}
#'   \item{unit.to}{hospital unit patient is going to}
#'   \item{arrive.datetime}{date/time of arrival in hospital unit}
#'   \item{depart.datetime}{date/time of departure from hospital unit}
#' }
#'
"locations"
