#' Sample warfarin goal data
#'
#' A dataset containing sample warfarin goal data
#'
#' @format A data frame with sample warfarin data, including indication,
#'   duration, and goal INR range. The data has been read in via
#'   \code{\link{read_data}} but no further tidying or transformation has been
#'   performed.
#' \describe{
#'   \item{pie.id}{encounter identifier}
#'   \item{warfarin.datetime}{date/time warfarin data was entered}
#'   \item{warfarin.event}{type of warfarin data being documented}
#'   \item{warfarin.result}{value of the data}
#' }
#'
"warfarin"