#' Sample intermittent medication data
#'
#' A dataset containing sample intermittent medication data
#'
#' @format A data frame with sample intermittent medication administration data
#'   for 3 patients. The data has been read in via \code{\link{read_data}} but
#'   no further tidying or transformation has been performed.
#' \describe{
#'   \item{pie.id}{encounter identifier}
#'   \item{order.id}{id for the order which is tied to the medication}
#'   \item{med.datetime}{date/time of medication administration}
#'   \item{med}{name of medication}
#'   \item{med.dose}{infusion rate}
#'   \item{med.dose.units}{dosage units (e.g., mg, units)}
#'   \item{med.route}{route of administration}
#'   \item{event.type}{administration event (e.g., Administered)}
#'   \item{event.id}{id for the administration event}
#' }
#'
"meds_sched"
