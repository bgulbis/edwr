# calc_runtime.R

#' Calculate the running time for continuous data
#'
#' \code{calc_runtime} calculates the duration of time at current value
#'
#' This function takes a data frame with serial measurement data (e.g.,
#' continuous medications or repeated lab data) and produces a data frame with
#' the with the duration of time at each value and cumulative run time.
#'
#' This could be used to then calculate the AUC or to summarize the continuous
#' data.
#'
#' For continuous medications, the data will be grouped into distinct sets of
#' infusions, for patients who may have been restarted on the drip one or more
#' times.
#'
#' @param x A data frame with serial measurement data
#' @param ... additional arguments passed on to individual methods
#' @param drip.off An optional numeric indicating the number of hours a
#'   medication infusion should be off to count as a new infusion, defaults to
#'   12 hours
#' @param no.doc An optional numeric indicating the number of hours without
#'   documentation which will be used to indicate a drip has ended, defaults to
#'   24 hours
#' @param units An optional character string specifying the time units to use in
#'   calculations, default is hours
#'
#' @return A data frame
#'
#' @export
calc_runtime <- function(x, ...) {
    UseMethod("calc_runtime")
}

#' @export
#' @rdname calc_runtime
calc_runtime.default <- function(x, ...) {
    warning(paste("No calc_runtime method available for class", class(x)))
    x
}

#' @export
#' @rdname calc_runtime
calc_runtime.meds_cont <- function(x, drip.off = 12, no.doc = 24, units = "hours", ...) {

}

#' @export
#' @rdname calc_runtime
calc_runtime.labs <- function(x, units = "hours", ...) {

}