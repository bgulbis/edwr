# calc_runtime.R

#' Calculate the running time for serial measurement data
#'
#' \code{calc_runtime} calculates the duration of time at current value and
#' total run time
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
#' times. Use the \code{drip.off} argument to modify the criteria for
#' determining distinct infusions.
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
#'
#' # calculate the runtime for continuous heparin infusion
#' print(head(
#'   calc_runtime(x)
#' ))
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
#' @importFrom magrittr %>%
calc_runtime.meds_cont <- function(x, drip.off = 12, no.doc = 24, units = "hours", ...) {
    # group the data by pie.id and med
    cont <- dplyr::group_by_(x, .dots = list("pie.id", "med")) %>%
        dplyr::arrange_(.dots = list("pie.id", "med", "med.datetime")) %>%
        # determine if it's a valid rate documentation
        dplyr::mutate_(.dots = purrr::set_names(
            x = list(~dplyr::if_else(is.na(med.rate.units), FALSE, TRUE),
                     ~cumsum(rate.change)),
            nm = list("rate.change", "change.num")
        )) %>%
        dplyr::group_by_("change.num", add = TRUE) %>%
        # fill in missing rates
        dplyr::mutate_(.dots = purrr::set_names(
            # x = list(~dplyr::coalesce(med.rate, dplyr::first(med.rate))),
            x = list(~dplyr::if_else(is.na(med.rate.units),
                                     dplyr::first(med.rate),
                                     med.rate)),
            nm = "rate"
        )) %>%
        dplyr::group_by_(.dots = list("pie.id", "med")) %>%
        # calculate time between rows and order of rate changes
        dplyr::mutate_(.dots = purrr::set_names(
            x = list(
                ~difftime(dplyr::lead(med.datetime), med.datetime,
                          units = units),
                ~dplyr::if_else(is.na(dplyr::lag(rate)) |
                                    rate != dplyr::lag(rate),
                                TRUE, FALSE),
                ~cumsum(rate.change)
            ),
            nm = list("time.next", "rate.change", "change.num")
        )) %>%
        dplyr::group_by_(.dots = list("pie.id", "med", "change.num")) %>%
        # calculate how long the drip was at each rate
        dplyr::summarize_(.dots = purrr::set_names(
            x = list(~dplyr::first(rate),
                     ~dplyr::first(med.datetime),
                     ~dplyr::last(med.datetime),
                     ~difftime(dplyr::last(med.datetime),
                               dplyr::first(med.datetime),
                               units = units),
                     ~dplyr::last(time.next)
            ),
            nm = list("med.rate", "rate.start", "rate.stop", "rate.duration",
                      "time.next")
        )) %>%
        # group the data by pie.id and med
        dplyr::group_by_(.dots = list("pie.id", "med")) %>%
        # identify individual drips
        dplyr::mutate_(.dots = purrr::set_names(
            x = list(
                ~dplyr::if_else(time.next < drip.off & !is.na(time.next),
                                rate.duration + time.next,
                                rate.duration),
                ~dplyr::if_else(is.na(time.next) | time.next > no.doc |
                                    (med.rate == 0 & duration > drip.off),
                                TRUE,
                                FALSE),
                ~dplyr::if_else(change.num == 1 | dplyr::lag(drip.stop == TRUE),
                                TRUE,
                                FALSE),
                ~cumsum(drip.start)
            ),
            nm = list("duration", "drip.stop", "drip.start", "drip.count")
        )) %>%
        dplyr::group_by_("drip.count", add = TRUE) %>%
        # calculate run time
        dplyr::mutate_(.dots = purrr::set_names(
            x = list(~difftime(rate.start, first(rate.start), units = units)),
            nm = "run.time"
        )) %>%
        # remove unnecessary columns
        dplyr::select_(.dots = list(quote(-rate.duration),
                                    quote(-time.next),
                                    quote(-drip.stop),
                                    quote(-drip.start),
                                    quote(-change.num))
        )

    # update drip stop information if rate of last row isn't 0
    drip.end <- dplyr::filter_(cont, .dots = list(
        ~rate.stop == dplyr::last(rate.stop),
        ~med.rate > 0)
    ) %>%
        # calculate the run time for the last drip row
        dplyr::mutate_(.dots = purrr::set_names(
            x = list(~duration + run.time, "rate.stop", 0),
            nm = list("run.time", "rate.start", "duration")
        ))

    # bind the rows with drip end data and arrange by date/time
    cont <- dplyr::bind_rows(cont, drip.end) %>%
        dplyr::arrange_(.dots = list("pie.id", "med", "drip.count", "rate.start"))

    # keep original class
    class(cont) <- class(x)
    cont
}

#' @export
#' @rdname calc_runtime
#' @importFrom magrittr %>%
calc_runtime.meds_sched <- function(x, units = "hours", ...) {
    cont <- dplyr::group_by_(x, .dots = c("pie.id", "med")) %>%
        dplyr::arrange_(.dots = list("pie.id", "med", "med.datetime")) %>%
        dplyr::mutate_(.dots = purrr::set_names(
            x = list(~difftime(med.datetime, dplyr::lag(med.datetime),
                               units = units),
                     ~dplyr::coalesce(duration, 0),
                     ~difftime(med.datetime, dplyr::first(med.datetime),
                               units = units)
            ),
            nm = c("duration", "duration", "run.time")
        ))

    # keep original class
    class(cont) <- class(x)
    cont
}

#' @export
#' @rdname calc_runtime
#' @importFrom magrittr %>%
calc_runtime.labs <- function(x, units = "hours", ...) {
    cont <- dplyr::group_by_(x, .dots = c("pie.id", "lab")) %>%
        dplyr::arrange_(.dots = list("pie.id", "lab", "lab.datetime")) %>%
        dplyr::mutate_(.dots = purrr::set_names(
            x = list(~difftime(lab.datetime, dplyr::lag(lab.datetime),
                               units = units),
                     ~dplyr::coalesce(duration, 0),
                     ~difftime(lab.datetime, dplyr::first(lab.datetime),
                               units = units)
            ),
            nm = c("duration", "duration", "run.time")
        ))

    # keep original class
    class(cont) <- class(x)
    cont
}