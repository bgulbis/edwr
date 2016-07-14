# dplyr_verbs.R

# create generic functions for dplyr verbs which maintain edwr class types

#' @export
filter.edwr <- function(.data, ...) {
    y <- NextMethod()
    class(y) <- class(.data)
    y
}

#' @export
filter_.edwr <- function(.data, ..., .dots) {
    y <- NextMethod()
    class(y) <- class(.data)
    y
}
