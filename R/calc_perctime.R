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
#' library(dplyr)
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
    warning("No method available for objects of this class")
    x
}

#' @export
#' @rdname calc_perctime
calc_perctime.meds_cont <- function(x, thrshld, ...) {
    # a wrapper for perctime
    id <- set_id_name(x)
    perctime(x, thrshld, vars = c(id, "med", "drip.count"))
}

#' @export
#' @rdname calc_perctime
calc_perctime.labs <- function(x, thrshld, ...) {
    # a wrapper for perctime
    id <- set_id_name(x)
    perctime(x, thrshld, vars = c(id, "lab"))
}

#' Calculate percent time above or below a threshold
#'
#' @param x data_frame
#' @param thrshld list of criteria
#' @param vars character vector with columns for grouping
#'
#' @return data_frame
#'
#' @keywords internal
perctime <- function(x, thrshld, vars) {
    cont <- group_by_(x, .dots = as.list(vars))

    # find all values within threshold and calculate the total time at goal
    goal <- filter_(cont, .dots = thrshld) %>%
        summarise_(.dots = set_names(
            x = list(~sum(duration, na.rm = TRUE)),
            nm = "time.goal"
        ))

    # get the total duration of data
    cont <- summarise_(cont, .dots = set_names(
        x = list(~dplyr::last(run.time)),
        nm = "total.dur"
    )) %>%
        full_join(goal, by = vars) %>%
        group_by_(.dots = as.list(vars)) %>%
        mutate_(.dots = set_names(
            x = list(~dplyr::coalesce(time.goal, 0),
                     ~dplyr::if_else(total.dur > 0, time.goal / total.dur, 0)),
            nm = list("time.goal", "perc.time")
        )) %>%
        ungroup()
}