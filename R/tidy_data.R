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
#' @param x A data frame with an edw class type
#' @param ... additional arguments passed on to individual methods
#' @param censor A logical, if TRUE will add a column indicating the data was
#'   censored (default)
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
