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
#' @param ... optional grouping variables
#' @param units An optional character string specifying the time units to use in
#'   calculations, default is hours
#' @param ref A data frame with three columns: name, type, and group. See
#'   details below.
#' @param pts An optional data frame with a column pie.id including all patients
#'   in study
#' @param home A logical, if TRUE (default) look for home medications, otherwise
#'   look for discharge prescriptions
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
#' # tidy continuous medications and calculate runtime
#' x <- tidy_data(meds_cont, meds_sched, ref)
#' x <- calc_runtime(x)
#'
#' # pass runtime data to summarize
#' print(head(
#'   summarize_data(x)
#' ))
#'
#' # tidy home medications
#' print(head(
#'   summarize_data(meds_home, ref = ref)
#' ))
#'
#' # return all patients, even if they do not have any of the desired home meds
#' pts <- dplyr::distinct(meds_home, pie.id)
#' print(head(
#'   summarize_data(meds_home, ref = ref, pts = pts)
#' ))
#'
#' # return discharge prescriptions instead of home meds
#' print(head(
#'   summarize_data(meds_home, ref = ref, pts = pts, home = FALSE)
#' ))
#'
#' @export
summarize_data <- function(x, ...) {
    UseMethod("summarize_data")
}

#' @export
#' @rdname summarize_data
summarize_data.default <- function(x, ...) {
    warning("No method available for objects of this class")
    x
}

#' @export
#' @rdname summarize_data
summarize_data.meds_cont <- function(x, ..., units = "hours") {
    # turn off scientific notation
    options(scipen = 999)

    id <- set_id_quo(x)
    group_var <- quos(...)

    grp_by <- quos(!!id, !!!group_var, !!sym("med"), !!sym("drip.count"))
    med.rate <- sym("med.rate")
    run.time <- sym("run.time")
    rate.start <- sym("rate.start")

    # cont <- x %>%
    #     group_by(!!!grp_by) %>%
    #     filter(!!run.time > 0)

    # get last and min non-zero rate
    nz.rate <- x %>%
        group_by(!!!grp_by) %>%
        filter(!!med.rate > 0) %>%
        summarize(
            !!"last.rate" := dplyr::last(!!med.rate),
            !!"min.rate" := min(!!med.rate, na.rm = TRUE),
            !!"run.time" := sum(!!sym("duration"), na.rm = TRUE)
        )

    # get first and max rates and AUC
    df <- x %>%
        group_by(!!!grp_by) %>%
        summarize(
            !!"start.datetime" := dplyr::first(!!rate.start),
            !!"stop.datetime" := dplyr::if_else(
                dplyr::last(!!med.rate) == 0,
                dplyr::last(!!rate.start),
                dplyr::last(!!sym("rate.stop"))
            ),
            !!"cum.dose" := sum(!!parse_expr("med.rate * duration"), na.rm = TRUE),
            !!"first.rate" := dplyr::first(!!med.rate),
            !!"max.rate" := max(!!med.rate, na.rm = TRUE),
            !!"auc" := MESS::auc(!!run.time, !!med.rate),
            !!"duration" := dplyr::last(!!run.time)
        ) %>%
        # join the last and min data, then calculate the time-weighted average
        # and interval
        inner_join(
            nz.rate,
            by = c(
                rlang::quo_text(id),
                purrr::map_chr(group_var, rlang::quo_text),
                "med",
                "drip.count"
            )
        ) %>%
        group_by(!!!grp_by) %>%
        dplyr::mutate_at("duration", as.numeric) %>%
        mutate(!!"time.wt.avg" := !!parse_expr("auc / duration")) %>%
        ungroup()

    reclass(x, df)
}

#' @export
#' @rdname summarize_data
summarize_data.meds_inpt <- function(x, ..., units = "hours", cont = TRUE) {
    if (cont) {
        summarize_data.meds_cont(x, ..., units = units)
    } else {
        summarize_data.meds_sched(x, ..., units = units)
    }
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
summarize_data.meds_home <- function(x, ..., ref, pts = NULL, home = TRUE) {
    id <- set_id_quo(x)
    type <- sym("type")
    # for any med classes, lookup the meds included in the class
    y <- filter(ref, !!type == "class")
    meds <- med_lookup(y$name)

    # join the list of meds with any indivdual meds included
    y <- filter(ref, !!type == "med")
    lookup.meds <- c(y$name, meds$med.name)

    # filter to either home medications or discharge medications, then use the
    # medication name or class to group by, then remove any duplicate patient /
    # group combinations, then convert the data to wide format
    if (home) {
        f <- parse_expr("med.type == 'Recorded / Home Meds'")
    } else {
        f <- parse_expr("med.type == 'Prescription / Discharge Order'")
    }

    df <- x %>%
        filter(
            !!f,
            !!parse_expr("med %in% lookup.meds")
        ) %>%
        left_join(meds, by = c("med" = "med.name")) %>%
        mutate(
            !!"group" := !!parse_expr("dplyr::if_else(is.na(med.class), med, med.class)"),
            !!"value" := TRUE
        ) %>%
        distinct(!!!quos(!!id, !!sym("group"), !!sym("value"))) %>%
        tidyr::spread(!!sym("group"), !!sym("value"), fill = FALSE, drop = FALSE)

    # join with list of all patients, fill in values of FALSE for any patients
    # not in the data set
    if (!is.null(pts)) {
        df <- reclass(x, df) %>%
            add_patients(pts)
    }

    reclass(x, df)
}

#' Performs the summarize operation
#'
#' @param x tibble
#' @param ... optional grouping variables
#' @param dt_col datetime column
#' @param val_col numeric value column
#'
#' @return tibble
#'
#' @keywords internal
summary_fun <- function(x, ..., dt_col, val_col) {
    # turn off scientific notation
    options(scipen = 999)

    id <- set_id_quo(x)
    group_var <- quos(...)

    dt_col <- enquo(dt_col)
    val_col <- enquo(val_col)

    df <- x %>%
        dplyr::add_count(!!id, !!!group_var) %>%
        group_by(!!id, !!!group_var, !!sym("n")) %>%
        summarize(
            !!"first.result" := dplyr::first(!!dt_col),
            !!"first.datetime" := dplyr::first(!!dt_col),
            !!"last.datetime" := dplyr::last(!!dt_col),
            !!"first.result" := dplyr::first(!!val_col),
            !!"last.result" := dplyr::last(!!val_col),
            !!"median.result" := stats::median(!!val_col, na.rm = TRUE),
            !!"max.result" := max(!!val_col, na.rm = TRUE),
            !!"min.result" := min(!!val_col, na.rm = TRUE),
            !!"auc" := MESS::auc(!!sym("run.time"), !!val_col),
            !!"duration" := dplyr::last(!!sym("run.time"))
        ) %>%
        group_by(!!id, !!!group_var) %>%
        dplyr::mutate_at("duration", as.numeric) %>%
        mutate(!!"time.wt.avg" := !!parse_expr("auc / duration")) %>%
        ungroup()

    reclass(x, df)
}

#' @export
#' @rdname summarize_data
summarize_data.meds_sched <- function(x, ..., units = "hours") {
    summary_fun(x,
                !!sym("med"),
                dt_col = !!sym("med.datetime"),
                val_col = !!sym("med.dose")
    )
}

#' @export
#' @rdname summarize_data
summarize_data.labs <- function(x, ..., units = "hours") {
    summary_fun(x,
                !!sym("lab"),
                dt_col = !!sym("lab.datetime"),
                val_col = !!sym("lab.result")
    )
}

#' @export
#' @rdname summarize_data
summarize_data.vitals <- function(x, ..., units = "hours") {
    summary_fun(x,
                !!sym("vital"),
                dt_col = !!sym("vital.datetime"),
                val_col = !!sym("vital.result")
    )
}

#' @export
#' @rdname summarize_data
summarize_data.events <- function(x, ..., units = "hours") {
    summary_fun(x,
                !!sym("event"),
                dt_col = !!sym("event.datetime"),
                val_col = !!sym("event.result")
    )
}
