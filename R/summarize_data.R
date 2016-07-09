# summarize_data.R

#' Summary calculations for serial measurement data
#'
#' \code{summarize_data} summarizes serial measurement  data
#'
#' This function takes a data frame with serial measurement  data (e.g.,
#' medications, lab results) and produces a data frame with summary data for
#' each patient.
#'
#' For continuous medications, the calculations include: first rate, last rate,
#' minimum rate, maximum rate, AUC, time-weighted average rate, total infusion
#' duration, total infusion running time, and cumulative dose.
#'
#' For labs and intermittent medications, the calculations include: first, last,
#' median, maximum, minimum, AUC, and time-weighted average result.
#'
#' @param x A data frame with continuous data
#' @param ... additional arguments passed on to individual methods
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
#' # tidy continuous medications and calculate runtime
#' x <- tidy_data(meds_cont, ref, meds_sched)
#' x <- calc_runtime(x)
#'
#' # pass runtime data to summarize
#' print(head(
#'   summarize_data(x)
#' ))
#'
#' @export
summarize_data <- function(x, ...) {
    UseMethod("summarize_data")
}

#' @export
#' @rdname summarize_data
summarize_data.default <- function(x, ...) {
    warning(paste("No summarize_data method available for class", class(x)))
    x
}

#' @export
#' @rdname summarize_data
summarize_data.meds_cont <- function(x, units = "hours", ...) {
    # turn off scientific notation
    options(scipen = 999)

    cont <- dplyr::group_by_(x, .dots = list("pie.id",
                                                  "med",
                                                  "drip.count"))

    # get last and min non-zero rate
    nz.rate <- dplyr::filter_(cont, .dots = ~(med.rate > 0)) %>%
        dplyr::summarize_(.dots = purrr::set_names(
            x = list(~dplyr::last(med.rate),
                     ~min(med.rate, na.rm = TRUE),
                     ~sum(duration, na.rm = TRUE)),
            nm = c("last.rate", "min.rate", "run.time")
        ))

    # get first and max rates and AUC
    cont <- dplyr::summarize_(cont, .dots = purrr::set_names(
        x = list(~dplyr::first(rate.start),
                 ~dplyr::last(rate.stop),
                 ~sum(med.rate * duration, na.rm = TRUE),
                 ~dplyr::first(med.rate),
                 ~max(med.rate, na.rm = TRUE),
                 ~MESS::auc(run.time, med.rate),
                 ~dplyr::last(run.time)),
        nm = c("start.datetime", "stop.datetime", "cum.dose", "first.rate",
               "max.rate", "auc", "duration")
    )) %>%
        # join the last and min data
        dplyr::inner_join(nz.rate, by = c("pie.id", "med", "drip.count")) %>%
        # calculate the time-weighted average and interval
        dplyr::mutate_(.dots = purrr::set_names(
            x = list(~auc/duration),
            nm = "time.wt.avg"
        ))

    # keep original class
    class(cont) <- class(x)
    cont
}

#' @export
#' @rdname summarize_data
summarize_data.meds_sched <- function(x, units = "hours", ...) {
    # turn off scientific notation
    options(scipen = 999)

    cont <- dplyr::group_by_(x, .dots = list("pie.id", "med")) %>%
        dplyr::summarize_(.dots = purrr::set_names(
            x = list(~dplyr::first(med.datetime),
                     ~dplyr::last(med.datetime),
                     ~dplyr::first(med.dose),
                     ~dplyr::last(med.dose),
                     ~median(med.dose, na.rm = TRUE),
                     ~max(med.dose, na.rm = TRUE),
                     ~min(med.dose, na.rm = TRUE),
                     ~MESS::auc(run.time, med.dose),
                     ~dplyr::last(run.time)),
            nm = c("first.datetime",
                   "last.datetime",
                   "first.result",
                   "last.result",
                   "median.result",
                   "max.result",
                   "min.result",
                   "auc",
                   "duration")
        )) %>%
        # calculate the time-weighted average
        dplyr::mutate_(.dots = purrr::set_names(
            x = list(~auc/duration),
            nm = "time.wt.avg"
        ))

    # keep original class
    class(cont) <- class(x)
    cont
}

#' @export
#' @rdname summarize_data
summarize_data.labs <- function(x, units = "hours", ...) {
    # turn off scientific notation
    options(scipen = 999)

    cont <- dplyr::group_by_(x, .dots = list("pie.id", "lab")) %>%
        dplyr::summarize_(.dots = purrr::set_names(
            x = list(~dplyr::first(lab.datetime),
                     ~dplyr::last(lab.datetime),
                     ~dplyr::first(lab.result),
                     ~dplyr::last(lab.result),
                     ~median(lab.result, na.rm = TRUE),
                     ~max(lab.result, na.rm = TRUE),
                     ~min(lab.result, na.rm = TRUE),
                     ~MESS::auc(run.time, lab.result),
                     ~dplyr::last(run.time)),
            nm = c("first.datetime",
                   "last.datetime",
                   "first.result",
                   "last.result",
                   "median.result",
                   "max.result",
                   "min.result",
                   "auc",
                   "duration")
        )) %>%
        # calculate the time-weighted average and interval
        dplyr::mutate_(.dots = purrr::set_names(
            x = list(~auc/duration),
            nm = "time.wt.avg"
        ))

    # keep original class
    class(cont) <- class(x)
    cont
}
