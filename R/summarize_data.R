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
#' @param ref A data frame with three columns: name, type, and group. See
#'   details below.
#' @param pts An optional data frame with a column pie.id including all patients
#'   in study
#' @param home A logical, if TRUE (default) look for home medications,
#'   otherwise look for discharge prescriptions
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
#' # make a reference data frame for tidying meds
#' ref <- tibble::tibble(
#'   name = c("heparin", "warfarin", "antiplatelet agents"),
#'   type = c("med", "med", "class"),
#'   group = c("cont", "sched", "sched")
#' )
#'
#' # tidy home medications
#' print(head(
#'   summarize_data(meds_home, ref)
#' ))
#'
#' # return all patients, even if they do not have any of the desired home meds
#' pts <- distinct(labs, pie.id)
#' print(head(
#'   summarize_data(meds_home, ref, pts = pts)
#' ))
#'
#' # return discharge prescriptions instead of home meds
#' print(head(
#'   summarize_data(meds_home, ref, pts = pts, home = FALSE)
#' ))
#'
#' @export
summarize_data <- function(x, ...) {
    UseMethod("summarize_data")
}

#' @export
#' @rdname summarize_data
summarize_data.default <- function(x, ...) {
    warning("No summarize_data methods available for objects of this class")
    x
}

#' @export
#' @rdname summarize_data
summarize_data.meds_cont <- function(x, units = "hours", ...) {
    # turn off scientific notation
    options(scipen = 999)

    cont <- group_by_(x, .dots = list("pie.id", "med", "drip.count"))

    # get last and min non-zero rate
    nz.rate <- filter_(cont, .dots = ~(med.rate > 0)) %>%
        summarize_(.dots = set_names(
            x = list(~dplyr::last(med.rate),
                     ~min(med.rate, na.rm = TRUE),
                     ~sum(duration, na.rm = TRUE)),
            nm = list("last.rate", "min.rate", "run.time")
        ))

    # get first and max rates and AUC
    summarize_(cont, .dots = set_names(
        x = list(~dplyr::first(rate.start),
                 ~dplyr::last(rate.stop),
                 ~sum(med.rate * duration, na.rm = TRUE),
                 ~dplyr::first(med.rate),
                 ~max(med.rate, na.rm = TRUE),
                 ~MESS::auc(run.time, med.rate),
                 ~dplyr::last(run.time)),
        nm = list("start.datetime", "stop.datetime", "cum.dose", "first.rate",
                  "max.rate", "auc", "duration")
    )) %>%

        # join the last and min data, then calculate the time-weighted average
        # and interval
        inner_join(nz.rate, by = c("pie.id", "med", "drip.count")) %>%
        group_by_(.dots = list("pie.id", "med", "drip.count")) %>%
        dplyr::mutate_(.dots = set_names(
            x = list(~auc/duration),
            nm = "time.wt.avg"
        )) %>%
        ungroup()
}

#' @details The data frame passed to \code{ref} should contain three character
#'   columns: name, type, and group. The name column should contain either
#'   generic medication names or medication classes. The type column should
#'   specify whether the value in name is a "class" or "med". The group column
#'   should specify whether the medication is a continous ("cont") or scheduled
#'   ("sched") medication.
#'
#' @export
#' @rdname summarize_data
summarize_data.meds_home <- function(x, ref, pts = NULL, home = TRUE, ...) {
    # for any med classes, lookup the meds included in the class
    y <- filter_(ref, .dots = list(~type == "class"))
    meds <- med_lookup(y$name)

    # join the list of meds with any indivdual meds included
    y <- filter_(ref, .dots = list(~type == "med"))
    lookup.meds <- c(y$name, meds$med.name)

    # filter to either home medications or discharge medications, then use the
    # medication name or class to group by, then remove any duplicate patient /
    # group combinations, then convert the data to wide format
    if (home == TRUE) {
        dots <- list(~med.type == "Recorded / Home Meds")
    } else {
        dots <- list(~med.type == "Prescription / Discharge Order")
    }

    tidy <- filter_(x, .dots = c(dots, list(~med %in% lookup.meds))) %>%
        left_join(meds, by = c("med" = "med.name")) %>%
        mutate_(.dots = set_names(
            x = list(~dplyr::if_else(is.na(med.class), med, med.class),
                     lazyeval::interp("y", y = TRUE)),
            nm = c("group", "value")
        )) %>%
        distinct_(.dots = list("pie.id", "group", "value")) %>%
        tidyr::spread_("group", "value", fill = FALSE, drop = FALSE)

    # keep original class (needed due to use of tidyr::spread function)
    class(tidy) <- class(x)

    # join with list of all patients, fill in values of FALSE for any patients
    # not in the data set
    if (!is.null(pts)) {
        tidy <- add_patients(tidy, pts)
    }

    tidy
}

#' @export
#' @rdname summarize_data
summarize_data.meds_sched <- function(x, units = "hours", ...) {
    # turn off scientific notation
    options(scipen = 999)

    group_by_(x, .dots = list("pie.id", "med")) %>%
        dplyr::summarize_(.dots = set_names(
            x = list(~dplyr::first(med.datetime),
                     ~dplyr::last(med.datetime),
                     ~dplyr::first(med.dose),
                     ~dplyr::last(med.dose),
                     ~median(med.dose, na.rm = TRUE),
                     ~max(med.dose, na.rm = TRUE),
                     ~min(med.dose, na.rm = TRUE),
                     ~MESS::auc(run.time, med.dose),
                     ~dplyr::last(run.time)),
            nm = list("first.datetime",
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
        group_by_(.dots = list("pie.id", "med")) %>%
        mutate_(.dots = set_names(
            x = list(~auc/duration),
            nm = "time.wt.avg"
        )) %>%
        ungroup()
}

#' @export
#' @rdname summarize_data
summarize_data.labs <- function(x, units = "hours", ...) {
    # turn off scientific notation
    options(scipen = 999)

    group_by_(x, .dots = list("pie.id", "lab")) %>%
        summarize_(.dots = set_names(
            x = list(~dplyr::first(lab.datetime),
                     ~dplyr::last(lab.datetime),
                     ~dplyr::first(lab.result),
                     ~dplyr::last(lab.result),
                     ~median(lab.result, na.rm = TRUE),
                     ~max(lab.result, na.rm = TRUE),
                     ~min(lab.result, na.rm = TRUE),
                     ~MESS::auc(run.time, lab.result),
                     ~dplyr::last(run.time)),
            nm = list("first.datetime",
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
        group_by_(.dots = list("pie.id", "lab")) %>%
        mutate_(.dots = set_names(
            x = list(~auc/duration),
            nm = "time.wt.avg"
        )) %>%
        ungroup()
}
