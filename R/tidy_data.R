# Tidy EDW data


#' Tidy data
#'
#' \code{tidy_data} transforms raw EDW data into a tidy format
#'
#' This is a generic function for tidying EDW data read in using
#' \code{\link{read_edw_data}}. The function invokes the appropriate method
#' based on the type of data being transformed (i.e., lab results, medication
#' data, etc.).
#'
#' @param x A data frame with an edw class type
#' @param ... additional arguments passed on to particular methods
#'
#' @seealso \code{\link{tidy_data.edw_labs}}
#'
#' @export
tidy_data <- function(x, ...) {
    UseMethod("tidy_data")
}

#' Tidy lab results
#'
#' \code{tidy_labs} transforms lab result data into a tidy format
#'
#' This function takes a data frame with lab results and returns a tidy data
#' frame. Results will be converted to numeric values and censored data will be
#' indicated.
#'
#' @param x A data frame with raw lab results
#' @param censor A logical, if TRUE will add a column indicating the data was
#'   censored (default)
#'
#' @return A data frame
#' @export
tidy_data.edw_labs <- function(x, censor = TRUE) {
    tidy <- x
    # create a column noting if data was censored
    if (censor == TRUE) {
        dots <- list(~stringr::str_detect(lab.result, ">|<"))
        tidy <- dplyr::mutate_(tidy, .dots = purrr::set_names(dots, "censored"))
    }

    # convert lab results to numeric values
    dots <- list(~as.numeric(lab.result))
    tidy <- dplyr::mutate_(tidy, .dots = purrr::set_names(dots, "lab.result"))

    class(tidy) <- class(x)
    tidy
}

#' @export
tidy_data.default <- function(x) {
    "default"
}