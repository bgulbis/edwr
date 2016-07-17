# dplyr_verbs.R

# create generic functions for dplyr verbs and joins which maintain edwr classes

#' @export
arrange_.tbl_edwr <- function(.data, ..., .dots) {
    y <- NextMethod()
    keep_class(.data, y)
}

#' @export
filter_.tbl_edwr <- function(.data, ..., .dots) {
    y <- NextMethod()
    keep_class(.data, y)
}

#' @export
mutate_.tbl_edwr <- function(.data, ..., .dots) {
    y <- NextMethod()
    keep_class(.data, y)
}

#' @export
rename_.tbl_edwr <- function(.data, ..., .dots) {
    y <- NextMethod()
    keep_class(.data, y)
}

#' @export
select_.tbl_edwr <- function(.data, ..., .dots) {
    y <- NextMethod()
    keep_class(.data, y)
}

#' @export
summarise_.tbl_edwr <- function(.data, ..., .dots) {
    y <- NextMethod()
    keep_class(.data, y)
}

#' @export
distinct_.tbl_edwr <- function(.data, ..., .dots, .keep_all = FALSE) {
    y <- NextMethod()
    keep_class(.data, y)
}

#' @export
group_by_.tbl_edwr <- function(.data, ..., .dots, add = FALSE) {
    y <- NextMethod()
    keep_class(.data, y)
}

#' @export
ungroup.tbl_edwr <- function(x, ...) {
    y <- NextMethod()
    keep_class(x, y)
}

#' @export
anti_join.tbl_edwr <- function(x, y, by = NULL, copy = FALSE, ...) {
    z <- NextMethod()
    keep_class(x, z)
}

#' @export
full_join.tbl_edwr <- function(x, y, by = NULL, copy = FALSE,
                           suffix = c(".x", ".y"), ...) {
    z <- NextMethod()
    keep_class(x, z)
}

#' @export
inner_join.tbl_edwr <- function(x, y, by = NULL, copy = FALSE,
                            suffix = c(".x", ".y"), ...) {
    z <- NextMethod()
    keep_class(x, z)
}

#' @export
left_join.tbl_edwr <- function(x, y, by = NULL, copy = FALSE,
                           suffix = c(".x", ".y"), ...) {
    z <- NextMethod()
    keep_class(x, z)
}

#' @export
right_join.tbl_edwr <- function(x, y, by = NULL, copy = FALSE,
                           suffix = c(".x", ".y"), ...) {
    z <- NextMethod()
    keep_class(x, z)
}

#' @export
semi_join.tbl_edwr <- function(x, y, by = NULL, copy = FALSE, ...) {
    z <- NextMethod()
    keep_class(x, z)
}
