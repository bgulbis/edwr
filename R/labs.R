#' Sample lab result data
#'
#' A dataset containing results for coagulation labs
#'
#' @format A data frame with sample laboratory data for 15 patients. The data
#'   has been read in via \code{\link{read_data}} but no further tidying or
#'   transformation has been performed.
#' \describe{
#'   \item{pie.id}{encounter identifier}
#'   \item{lab.datetime}{date / time the lab was drawn at}
#'   \item{lab}{name of the lab drawn}
#'   \item{lab.result}{result of the lab, reported as a character}
#' }
#'
"labs"
