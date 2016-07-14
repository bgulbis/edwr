# calc_perctime.R

#' Calculate proportion of time above or below a threshold
#'
#' \code{calc_perctime} calculates percent time above / below a threshold
#'
#' This function takes a data frame with serial measurement data and produces a
#' data frame with percent time above or below a threshold for each infusion.
#'
#' The argument \code{thrshld} must be provided as a list with one or more
#' criteria, which indicate whether to calculate time above, below, or between
#' the threshold values. The criteria should be written as a formula, of the
#' form: \code{list(~variable > value)} or \code{list(~variable < value1,
#' ~variable > value2)}
#'
#' @param x A data frame with serial measurement data
#' @param ... additional arguments passed on to individual methods
#' @param thrshld A list of the criteria
#'
#' @return A data frame
#'
#' @examples
#' # make a reference data frame for tidying meds
#' ref <- tibble::tibble(
#'   name = c("heparin", "warfarin", "antiplatelet agents"),
#'   type = c("med", "med", "class"),
#'   group = c("cont", "sched", "sched")
#' )
#'
#' # tidy continuous medications; will keep only heparin drips
#' x <- tidy_data(meds_cont, ref, meds_sched)
#' y <- calc_runtime(x)
#'
#' # calculate the proportion of time the infusion rate was > 10 units/kg/hour
#' print(head(
#'   calc_perctime(y, list(~med.rate > 10))
#' ))
#'
#' @export
calc_perctime <- function(x, ...) {
    UseMethod("calc_perctime")
}

#' @export
#' @rdname calc_perctime
calc_perctime.default <- function(x, ...) {
    warning(paste("No calc_perctime method available for class", class(x)))
    x
}

#' @export
#' @rdname calc_perctime
calc_perctime.meds_cont <- function(x, thrshld, ...) {
    # essentially a wrapper for perctime
    cont <- perctime(x, thrshld, vars = c("pie.id", "med", "drip.count"))

    # keep original class
    class(cont) <- class(x)
    cont
}

#' @export
#' @rdname calc_perctime
calc_perctime.labs <- function(x, thrshld, ...) {
    # essentially a wrapper for perctime
    cont <- perctime(x, thrshld, vars = c("pie.id", "lab"))

    # keep original class
    class(cont) <- class(x)
    cont
}

# function to calculate percent time above or below a threshold
perctime <- function(x, thrshld, vars) {
    cont <- dplyr::group_by_(x, .dots = as.list(vars))

    # find all values within threshold and calculate the total time at goal
    goal <- dplyr::filter_(cont, .dots = thrshld) %>%
        dplyr::summarize_(.dots = purrr::set_names(
            x = list(~sum(duration, na.rm = TRUE)),
            nm = "time.goal"
        ))

    # get the total duration of data
    cont <- dplyr::summarize_(cont, .dots = purrr::set_names(
        x = list(~dplyr::last(run.time)),
        nm = "total.dur"
    )) %>%
        dplyr::full_join(goal, by = vars) %>%
        dplyr::mutate_(.dots = purrr::set_names(
            x = list(~dplyr::coalesce(time.goal, 0),
                     ~dplyr::if_else(total.dur > 0, time.goal / total.dur, 0)),
            nm = list("time.goal", "perc.time")
        ))
}