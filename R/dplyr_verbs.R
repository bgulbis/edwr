# dplyr_verbs.R

# create generic functions for dplyr verbs and joins which maintain edwr classes

#' @export
arrange_.edwr <- function(.data, ..., .dots) {
    y <- NextMethod()
    keep_class(.data, y)
}

#' @export
filter_.edwr <- function(.data, ..., .dots) {
    y <- NextMethod()
    keep_class(.data, y)
}

#' @export
mutate_.edwr <- function(.data, ..., .dots) {
    y <- NextMethod()
    keep_class(.data, y)
}

#' @export
rename_.edwr <- function(.data, ..., .dots) {
    y <- NextMethod()
    keep_class(.data, y)
}

#' @export
select_.edwr <- function(.data, ..., .dots) {
    y <- NextMethod()
    keep_class(.data, y)
}

#' @export
summarise_.edwr <- function(.data, ..., .dots) {
    y <- NextMethod()
    keep_class(.data, y)
}

#' @export
group_by_.edwr <- function(.data, ..., .dots, add = FALSE) {
    y <- NextMethod()
    keep_class(.data, y)
}

#' @export
ungroup.edwr <- function(x, ...) {
    y <- NextMethod()
    keep_class(x, y)
}

#' @export
distinct_.edwr <- function(.data, ..., .dots, .keep_all = FALSE) {
    y <- NextMethod()
    keep_class(.data, y)
}

#' @export
anti_join.edwr <- function(x, y, by = NULL, copy = FALSE, ...) {
    z <- NextMethod()
    keep_class(x, z)
}

#' @export
full_join.edwr <- function(x, y, by = NULL, copy = FALSE,
                           suffix = c(".x", ".y"), ...) {
    z <- NextMethod()
    keep_class(x, z)
}

#' @export
inner_join.edwr <- function(x, y, by = NULL, copy = FALSE,
                            suffix = c(".x", ".y"), ...) {
    z <- NextMethod()
    keep_class(x, z)
}

#' @export
left_join.edwr <- function(x, y, by = NULL, copy = FALSE,
                           suffix = c(".x", ".y"), ...) {
    z <- NextMethod()
    keep_class(x, z)
}

#' @export
right_join.edwr <- function(x, y, by = NULL, copy = FALSE,
                           suffix = c(".x", ".y"), ...) {
    z <- NextMethod()
    keep_class(x, z)
}

#' @export
semi_join.edwr <- function(x, y, by = NULL, copy = FALSE, ...) {
    z <- NextMethod()
    keep_class(x, z)
}
