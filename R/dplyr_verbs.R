# dplyr_verbs.R

# create generic functions for dplyr verbs and joins which maintain edwr class
# types; for use in scripts and interactive programming only, use in functions
# should still call dplyr::_ (SE) functions

#' @export
arrange.edwr <- function(.data, ...) {
    y <- NextMethod()
    class(y) <- class(.data)
    y
}

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


#' @export
mutate.edwr <- function(.data, ...) {
    y <- NextMethod()
    class(y) <- class(.data)
    y
}

#' @export
select.edwr <- function(.data, ...) {
    y <- NextMethod()
    class(y) <- class(.data)
    y
}

#' @export
summarize.edwr <- function(.data, ...) {
    y <- NextMethod()
    class(y) <- class(.data)
    y
}

#' @export
anti_join.edwr <- function(x, y, by = NULL, copy = FALSE, ...) {
    y <- NextMethod()
    class(y) <- class(.data)
    y
}

#' @export
full_join.edwr <- function(x, y, by = NULL, copy = FALSE,
                           suffix = c(".x", ".y"), ...) {
    y <- NextMethod()
    class(y) <- class(.data)
    y
}

#' @export
inner_join.edwr <- function(x, y, by = NULL, copy = FALSE,
                            suffix = c(".x", ".y"), ...) {
    y <- NextMethod()
    class(y) <- class(.data)
    y
}

#' @export
semi_join.edwr <- function(x, y, by = NULL, copy = FALSE, ...) {
    y <- NextMethod()
    class(y) <- class(.data)
    y
}