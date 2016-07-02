# Tidy EDW data


#' Tidy data
#'
#' \code{tidy_data} transforms raw EDW data into a tidy format
#'
#' This is an S3 generic function for tidying EDW data read in using
#' \code{\link{read_data}}. The function invokes the appropriate method based on
#' the type of data being transformed (i.e., lab results, medication data,
#' etc.).
#'
#' The data frame passed to \code{ref} should contain three columns: name, type,
#' and group. The name column should contain either generic medication names or
#' medication classes. The type column should specify whether the value in name
#' is a "class" or "med". The group column should specify whether the medication
#' is a continous ("cont") or scheduled ("sched") medication.
#'
#' @param x A data frame with an edw class type
#' @param ... additional arguments passed on to individual methods
#' @param censor A logical, if TRUE will add a column indicating the data was
#'   censored (default)
#' @param ref A data frame with three columns: name, type, and group
#' @param sched A data frame with intermittent medications
#' @param pts An optional data frame with a column pie.id including all patients
#'   in study
#' @param home A logical, if TRUE (default) look for home medications,
#'   otherwise look for discharge medications
#'
#' @examples
#' x <- read_data(
#'   data.dir = paste0(system.file(package = "edwr", "extdata")),
#'   file.name = "test_labs.csv",
#'   type = "labs"
#' )
#'
#' suppressWarnings(
#'   y <- tidy_data(x)
#' )
#'
#' print(head(y))
#'
#' @export
tidy_data <- function(x, ...) {
    UseMethod("tidy_data")
}

#' @export
#' @rdname tidy_data
tidy_data.default <- function(x, ...) {
    warning(paste("No tidy_data method available for class", class(x)))
    x
}

#' @export
#' @rdname tidy_data
tidy_data.labs <- function(x, censor = TRUE, ...) {
    tidy <- x
    # create a column noting if data was censored
    if (censor == TRUE) {
        dots <- list(~stringr::str_detect(lab.result, ">|<"))
        tidy <- dplyr::mutate_(tidy, .dots = purrr::set_names(dots, "censored"))
    }

    # convert lab results to numeric values
    dots <- list(~as.numeric(lab.result))
    tidy <- dplyr::mutate_(tidy, .dots = purrr::set_names(dots, "lab.result"))

    # keep original class
    class(tidy) <- class(x)
    tidy
}

#' @export
#' @rdname tidy_data
#' @importFrom magrittr %>%
tidy_data.meds_cont <- function(x, ref, sched, ...) {
    # for any med classes, lookup the meds included in the class
    y <- dplyr::filter_(ref, .dots = list(~type == "class", ~group == "cont"))
    class.meds <- med_lookup(y$name)

    # join the list of meds with any indivdual meds included
    y <- dplyr::filter_(ref, .dots = list(~type == "med", ~group == "cont"))
    lookup.meds <- c(y$name, class.meds$med.name)

    # remove any rows in continuous data which are actually scheduled doses,
    # then filter to meds in lookup, then sort by pie.id, med, med.datetime
    tidy <- dplyr::anti_join(x, sched, by = "event.id") %>%
        dplyr::filter_(.dots = list(~med %in% lookup.meds)) %>%
        dplyr::arrange_(.dots = list("pie.id", "med", "med.datetime"))

    # keep original class
    class(tidy) <- class(x)
    tidy
}

#' @export
#' @rdname tidy_data
#' @importFrom magrittr %>%
tidy_data.meds_sched <- function(x, ref, ...) {
    # for any med classes, lookup the meds included in the class
    y <- dplyr::filter_(ref, .dots = list(~type == "class", ~group == "sched"))
    class.meds <- med_lookup(y$name)

    # join the list of meds with any indivdual meds included
    y <- dplyr::filter_(ref, .dots = list(~type == "med", ~group == "sched"))
    lookup.meds <- c(y$name, class.meds$med.name)

    # filter to keep only meds in lookup
    tidy <- dplyr::filter_(x, .dots = list(~med %in% lookup.meds)) %>%
        dplyr::arrange_(.dots = list("pie.id", "med", "med.datetime"))

    # keep original class
    class(tidy) <- class(x)
    tidy
}

#' @export
#' @rdname tidy_data
#' @importFrom magrittr %>%
tidy_data.home_meds <- function(x, ref, pts = NULL, home = TRUE, ...) {
    # for any med classes, lookup the meds included in the class
    y <- dplyr::filter_(ref, .dots = list(~type == "class"))
    meds <- med_lookup(y$name)

    # join the list of meds with any indivdual meds included
    y <- dplyr::filter_(ref, .dots = list(~type == "med"))
    lookup.meds <- c(y$name, meds$med.name)

    # filter to either home medications or discharge medications, then use the
    # medication name or class to group by, then remove any duplicate patient /
    # group combinations, then convert the data to wide format
    if (home == TRUE) {
        dots <- list(~med.type == "Recorded / Home Meds")
    } else {
        dots <- list(~med.type == "Prescription / Discharge Order")
    }

    dots2 <- list(~ifelse(is.na(med.class), med, med.class),
                  lazyeval::interp("y", y = TRUE))

    tidy <- dplyr::filter_(x, .dots = c(dots, list(~med %in% lookup.meds))) %>%
        dplyr::left_join(meds, by = c("med" = "med.name")) %>%
        dplyr::mutate_(.dots = purrr::set_names(dots2, c("group", "value"))) %>%
        dplyr::select_(.dots = list("pie.id", "group", "value")) %>%
        dplyr::distinct_(.dots = list("pie.id", "group"), .keep_all = TRUE) %>%
        tidyr::spread_("group", "value", fill = FALSE, drop = FALSE)

    # join with list of all patients, fill in values of FALSE for any patients
    # not in the data set
    if (!is.null(pts)) {
        tidy <- add_patients(tidy, pts)
    }

    # keep original class
    class(tidy) <- class(x)
    tidy
}
