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
#' @param x A data frame with serial measurement data
#' @param ... optional grouping variables
#' @param drip.off An optional numeric indicating the number of hours a
#'   medication infusion should be off to count as a new infusion, defaults to
#'   12 hours
#' @param no.doc An optional numeric indicating the number of hours without
#'   documentation which will be used to indicate a drip has ended, defaults to
#'   24 hours
#' @param units An optional character string specifying the time units to use in
#'   calculations, default is hours
#' @param cont A logical, if TRUE (default), treat the medications as continuous
#'   when summarizing
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
#' x <- tidy_data(meds_cont, meds_sched, ref)
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
    warning("No method available for objects of this class")
    x
}

#' @details For continuous medications, the data will be grouped into distinct
#'   sets of infusions, for patients who may have been restarted on the drip one
#'   or more times. Use the \code{drip.off} argument to modify the criteria for
#'   determining distinct infusions.
#'
#' @export
#' @rdname calc_runtime
calc_runtime.meds_cont <- function(x, ..., drip.off = 12, no.doc = 24,
                                   units = "hours") {
    id <- set_id_quo(x)

    group_var <- quos(...)

    med <- sym("med")
    med.datetime <- sym("med.datetime")
    med.rate <- sym("med.rate")
    med.rate.units <- sym("med.rate.units")
    change.num <- sym("change.num")
    rate.change <- sym("rate.change")
    rate <- sym("rate")
    rate.duration <- sym("rate.duration")
    rate.start <- sym("rate.start")
    rate.stop <- sym("rate.stop")
    time.next <- sym("time.next")
    drip.start <- sym("drip.start")
    drip.stop <- sym("drip.stop")
    drip.count <- sym("drip.count")
    duration <- sym("duration")

    cont <- x %>%
        arrange(!!id, !!!group_var, !!med, !!med.datetime) %>%

        # determine if it's a valid rate documentation
        group_by(!!id, !!!group_var, !!med) %>%
        mutate(
            !!"rate.change" := !is.na(!!med.rate.units),
            !!"change.num" := cumsum(!!rate.change)
        ) %>%

        # fill in missing rates
        group_by(!!id, !!!group_var, !!med, !!change.num) %>%
        mutate(
            !!"rate" := dplyr::if_else(
                is.na(!!med.rate.units),
                dplyr::first(!!med.rate),
                !!med.rate
            )
        ) %>%

        # calculate time between rows and order of rate changes
        group_by(!!id, !!!group_var, !!med) %>%
        mutate(
            !!"time.next" := difftime(
                dplyr::lead(!!med.datetime),
                !!med.datetime,
                units = units
            ),
            !!"rate.change" := is.na(dplyr::lag(!!rate)) |
                rate != dplyr::lag(!!rate),
            !!"change.num" := cumsum(!!rate.change)
        ) %>%

        # calculate how long the drip was at each rate
        group_by(!!id, !!!group_var, !!med, !!change.num) %>%
        summarize(
            !!"med.rate" := dplyr::first(!!rate),
            !!"rate.start" := dplyr::first(!!med.datetime),
            !!"rate.stop" := dplyr::last(!!med.datetime),
            !!"rate.duration" := difftime(
                dplyr::last(!!med.datetime),
                dplyr::first(!!med.datetime),
                units = units
            ),
            !!"time.next" := dplyr::last(!!time.next)
        ) %>%

        # identify individual drips
        group_by(!!id, !!!group_var, !!med) %>%
        mutate(
            !!"duration" := dplyr::if_else(
                !!time.next < drip.off & !is.na(!!time.next),
                !!rate.duration + !!time.next,
                !!rate.duration
            ),
            !!"drip.stop" := is.na(!!time.next) | !!time.next > no.doc |
                (!!med.rate == 0 & !!duration > drip.off),
            !!"drip.start" := !!change.num == 1 | dplyr::lag(!!drip.stop),
            !!"drip.count" := cumsum(!!drip.start)
        ) %>%
        dplyr::mutate_at("duration", as.numeric) %>%

        # calculate run time
        group_by(!!id, !!!group_var, !!med, !!drip.count) %>%
        mutate(
            !!"run.time" := difftime(
                !!rate.start,
                dplyr::first(!!rate.start),
                units = units
            )
        ) %>%

        # remove unnecessary columns
        select(
            -!!rate.duration,
            -!!time.next,
            -!!drip.start,
            -!!drip.stop,
            -!!change.num
        )

    # update drip stop information if rate of last row isn't 0
    drip.end <- cont %>%
        filter(
            !!rate.stop == dplyr::last(!!rate.stop),
            !!med.rate > 0
        ) %>%

        # calculate the run time for the last drip row
        mutate(
            !!"run.time" := !!duration + !!sym("run.time"),
            !!"rate.start" := !!rate.stop,
            !!"duration" := 0
        ) %>%
        ungroup()

    # bind the rows with drip end data and arrange by date/time; need to ungroup
    # first for bind_rows to keep edwr class assigment
    df <- cont %>%
        ungroup() %>%
        dplyr::bind_rows(drip.end) %>%
        arrange(!!id, !!!group_var, !!med, !!drip.count, !!rate.start)

    reclass(x, df)
}

#' @export
#' @rdname calc_runtime
calc_runtime.meds_inpt <- function(x, ..., drip.off = 12, no.doc = 24,
                                   units = "hours", cont = TRUE) {
    # calls method for continuous meds
    if (cont) {
        calc_runtime.meds_cont(
            x,
            ...,
            drip.off = drip.off,
            no.doc = no.doc,
            units = units
        )
    } else {
        calc_runtime.meds_sched(x, ..., units = units)
    }
}

#' Performs the runtime calculation
#'
#' @param x tibble
#' @param ... grouping quosures
#' @param val_col event column (lab, med, etc.)
#' @param dt_col datetime column
#' @param units An optional character string specifying the time units to use in
#'   calculations, default is hours
#'
#' @return tibble
#'
#' @importFrom rlang .data
#' @keywords internal
calc_runtime_fun <- function(x, ..., val_col, dt_col, units = "hours") {
    id <- set_id_quo(x)
    group_var <- quos(...)

    val_col <- enquo(val_col)
    dt_col <- enquo(dt_col)

    df <- x %>%
        arrange(!!id, !!!group_var, !!val_col, !!dt_col) %>%
        group_by(!!id, !!!group_var, !!val_col) %>%
        mutate(
            !!"duration" := difftime(
                !!dt_col,
                dplyr::lag(!!dt_col),
                units = units
            ),
            !!"run.time" := difftime(
                !!dt_col,
                dplyr::first(!!dt_col),
                units = units
            )
        ) %>%
        dplyr::mutate_at(
            "duration",
            dplyr::funs(
                dplyr::coalesce(.data, 0)
            )
        ) %>%
        ungroup()

    reclass(x, df)

}

#' @export
#' @rdname calc_runtime
calc_runtime.meds_sched <- function(x, ..., units = "hours") {
    calc_runtime_fun(
        x,
        ...,
        val_col = !!sym("med"),
        dt_col = !!sym("med.datetime"),
        units = units
    )
}

#' @export
#' @rdname calc_runtime
calc_runtime.labs <- function(x, ..., units = "hours") {
    calc_runtime_fun(
        x,
        ...,
        val_col = !!sym("lab"),
        dt_col = !!sym("lab.datetime"),
        units = units
    )
    # was difftime(lead(lab.datetime), lab.datetime)
}

#' @export
#' @rdname calc_runtime
calc_runtime.events <- function(x, ..., units = "hours") {
    x %>%
        dplyr::mutate_at("event.result", as.numeric) %>%
        calc_runtime_fun(
            ...,
            val_col = !!sym("event"),
            dt_col = !!sym("event.datetime"),
            units = units
        )
}

#' @export
#' @rdname calc_runtime
calc_runtime.vitals <- function(x, ..., units = "hours") {
    calc_runtime_fun(
        x,
        ...,
        val_col = !!sym("vital"),
        dt_col = !!sym("vital.datetime"),
        units = units
    )
    # was difftime(lead(vital.datetime), vital.datetime)
}
