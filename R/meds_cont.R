#' Sample continuous medication data
#'
#' A dataset containing sample continuous medication data
#'
#' @format A data frame with sample continuous medication administration data
#'   for 3 patients. The data has been read in via \code{\link{read_data}} but
#'   no further tidying or transformation has been performed.
#' \describe{
#'   \item{pie.id}{encounter identifier}
#'   \item{order.id}{id for the order which is tied to the medication}
#'   \item{event.id}{id for the administration event}
#'   \item{med.datetime}{date/time of medication administration}
#'   \item{med}{name of medication}
#'   \item{med.rate}{infusion rate}
#'   \item{med.rate.units}{infusion rate units (e.g., mg/kg/hr)}
#'   \item{route}{route of administration}
#'   \item{event.tag}{administration event (e.g., Begin Bag)}
#' }
#'
"meds_cont"
