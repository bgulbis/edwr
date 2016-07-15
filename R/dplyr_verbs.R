# dplyr_verbs.R

# create generic functions for dplyr verbs and joins which maintain edwr classes

#' @export
arrange_.edwr <- function(.data, ..., .dots) {
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
mutate_.edwr <- function(.data, ..., .dots) {
    y <- NextMethod()
    class(y) <- class(.data)
    y
}

#' @export
rename_.edwr <- function(.data, ..., .dots) {
    y <- NextMethod()
    class(y) <- class(.data)
    y
}


#' @export
select_.edwr <- function(.data, ..., .dots) {
    y <- NextMethod()
    class(y) <- class(.data)
    y
}

#' @export
summarise_.edwr <- function(.data, ..., .dots) {
    y <- NextMethod()
    class(y) <- class(.data)
    y
}

#' @export
anti_join.edwr <- function(x, y, by = NULL, copy = FALSE, ...) {
    z <- NextMethod()
    class(z) <- class(x)
    z
}

#' @export
full_join.edwr <- function(x, y, by = NULL, copy = FALSE,
                           suffix = c(".x", ".y"), ...) {
    z <- NextMethod()
    class(z) <- class(x)
    z
}

#' @export
inner_join.edwr <- function(x, y, by = NULL, copy = FALSE,
                            suffix = c(".x", ".y"), ...) {
    z <- NextMethod()
    class(z) <- class(x)
    z
}

#' @export
semi_join.edwr <- function(x, y, by = NULL, copy = FALSE, ...) {
    z <- NextMethod()
    class(z) <- class(x)
    z
}
