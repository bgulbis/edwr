#' Sample home medication data
#'
#' A dataset containing sample home medication data
#'
#' @format A data frame with sample home medication / discharge prescription
#'   data. The data has been read in via \code{\link{read_data}} but no further
#'   tidying or transformation has been performed.
#' \describe{
#'   \item{pie.id}{encounter identifier}
#'   \item{med}{name of medication}
#'   \item{order.name}{order mnemonic used in EMR}
#'   \item{type.flag}{flag numeric value}
#'   \item{med.type}{flag indicating a home medication or discharge prescription}
#' }
#'
"meds_home"
