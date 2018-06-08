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
#' @param thrshld A charactor vector of the criteria
#' @param cont A logical, if TRUE (default), treat the data as continuous
#'   when calculating the percent time
#' @param ... additional arguments passed on to individual methods
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
#' x <- tidy_data(meds_cont, meds_sched, ref)
#' y <- calc_runtime(x)
#'
#' # calculate the proportion of time the infusion rate was > 10 units/kg/hour
#' print(head(
#'   calc_perctime(y, "med.rate > 10")
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
    id <- set_id_quo(x)
    perctime(x,
             !!id,
             !!sym("med"),
             !!sym("drip.count"),
             thrshld = thrshld
    )
}

#' @export
#' @rdname calc_perctime
calc_perctime.meds_inpt <- function(x, thrshld, cont = TRUE, ...) {
    # a wrapper for perctime
    id <- set_id_quo(x)

    if (cont) {
        calc_perctime.meds_cont(x, thrshld = thrshld, ...)
    } else {
        calc_perctime.meds_sched(x, thrshld = thrshld, ...)
    }
}

#' @export
#' @rdname calc_perctime
calc_perctime.meds_sched <- function(x, thrshld, ...) {
    # a wrapper for perctime
    id <- set_id_quo(x)
    perctime(x,
             !!id,
             !!sym("med"),
             thrshld = thrshld
    )
}

#' @export
#' @rdname calc_perctime
calc_perctime.events <- function(x, thrshld, ...) {
    # a wrapper for perctime
    id <- set_id_quo(x)
    perctime(x,
             !!id,
             !!sym("event"),
             thrshld = thrshld
    )
}

#' @export
#' @rdname calc_perctime
calc_perctime.labs <- function(x, thrshld, ...) {
    # a wrapper for perctime
    id <- set_id_quo(x)
    perctime(x,
             !!id,
             !!sym("lab"),
             thrshld = thrshld
    )
}

#' @export
#' @rdname calc_perctime
calc_perctime.vitals <- function(x, thrshld, ...) {
    # a wrapper for perctime
    id <- set_id_quo(x)
    perctime(x,
             !!id,
             !!sym("vital"),
             thrshld = thrshld
    )
}

#' Calculate percent time above or below a threshold
#'
#' @param x data_frame
#' @param ... optional grouping variables
#' @param thrshld list of criteria
#'
#' @return data_frame
#'
#' @importFrom rlang .data
#' @keywords internal
perctime <- function(x, ..., thrshld) {
    # turn off scientific notation
    options(scipen = 999)

    group_var <- quos(...)

    # find all values within threshold and calculate the total time at goal
    goal <- x %>%
        group_by(!!!group_var) %>%
        filter(!!!rlang::parse_exprs(thrshld)) %>%
        summarize(!!"time.goal" := sum(!!sym("duration"), na.rm = TRUE))

    # get the total duration of data
    df <- x %>%
        group_by(!!!group_var) %>%
        summarize(!!"total.dur" := dplyr::last(!!sym("run.time"))) %>%
        dplyr::mutate_at("total.dur", as.numeric) %>%
        full_join(goal, by = purrr::map_chr(group_var, rlang::quo_text)) %>%
        group_by(!!!group_var) %>%
        dplyr::mutate_at(
            "time.goal",
            dplyr::funs(
                dplyr::coalesce(.data, 0)
            )
        ) %>%
        mutate(
            !!"perc.time" := dplyr::if_else(
                !!sym("total.dur") > 0,
                !!sym("time.goal") / !!sym("total.dur"),
                0
            )
        ) %>%
        ungroup()

    reclass(x, df)
}
