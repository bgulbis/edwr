# class.R
#
# get and set class types used in edwr
#

#' Construct edwr data types
#'
#' Takes an R object and sets class to an edwr type.
#'
#' @param x object to set class \code{edwr}
#'
#' @name set_edwr_class
#' @keywords internal
edwr <- function(x) {
    cl <- class(x)
    if ("edwr" %in% cl) return (x)
    class(x) <- c("edwr", cl)
    x
}

#' @rdname set_edwr_class
#' @export
as.edwr <- function(x) {
    if (missing(x)) x <- character()
    if (is.edwr(x)) return(x)
    after <- match("edwr", class(x), nomatch = 0L)
    class(x) <- append(class(x), "edwr", after = after)
    x
}

#' Test edwr-related classes
#'
#' Takes an R object and checks for an edwr class type.
#'
#' @param x object which may have an edwr class type
#' @export
is.edwr <- function(x) inherits(x, "edwr")

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
