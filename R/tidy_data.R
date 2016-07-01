# Tidy EDW data


#' Tidy data
#'
#' \code{tidy_data} transforms raw EDW data into a tidy format
#'
#' This is an S3 generic function for tidying EDW data read in using
#' \code{\link{read_edw_data}}. The function invokes the appropriate method
#' based on the type of data being transformed (i.e., lab results, medication
#' data, etc.).
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
#'
#' @examples
#' x <- read_edw_data(
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
