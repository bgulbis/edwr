# dplyr_verbs.R

# create generic functions for dplyr verbs and joins which maintain edwr classes

#' Keep edwr class assignments
#'
#' @param x tibble with tbl_edwr class(es)
#' @param result tibble as returned by dplyr manipulation function
#'
#' @return tibble
#'
#' @export
reclass <- function(x, result) {
    UseMethod('reclass')
}

#' @export
reclass.default <- function(x, result) {
    if (!is.tbl_edwr(result)) {
        cls <- match("tbl_edwr", class(x), nomatch = 0L)
        class(result) <- c(class(x)[1:cls], class(result))
        attr(result, "data") <- attr(x, "data")
    }
    result
}

#' @importFrom dplyr arrange
#' @export
arrange.tbl_edwr <- function(.data, ...) {
    result <- NextMethod()
    reclass(.data, result)
}

#' @importFrom dplyr arrange_
#' @export
arrange_.tbl_edwr <- function(.data, ..., .dots) {
    result <- NextMethod()
    reclass(.data, result)
}

#' @importFrom dplyr filter
#' @export
filter.tbl_edwr <- function(.data, ...) {
    result <- NextMethod()
    reclass(.data, result)
}

#' @importFrom dplyr filter_
#' @export
filter_.tbl_edwr <- function(.data, ..., .dots) {
    result <- NextMethod()
    reclass(.data, result)
}

#' @importFrom dplyr mutate
#' @export
mutate.tbl_edwr <- function(.data, ...) {
    result <- NextMethod()
    reclass(.data, result)
}

#' @importFrom dplyr mutate_
#' @export
mutate_.tbl_edwr <- function(.data, ..., .dots) {
    result <- NextMethod()
    reclass(.data, result)
}

#' @importFrom dplyr rename
#' @export
rename.tbl_edwr <- function(.data, ..., .dots) {
    result <- NextMethod()
    reclass(.data, result)
}

#' @importFrom dplyr rename_
#' @export
rename_.tbl_edwr <- function(.data, ...) {
    result <- NextMethod()
    reclass(.data, result)
}

#' @importFrom dplyr select
#' @export
select.tbl_edwr <- function(.data, ...) {
    result <- NextMethod()
    reclass(.data, result)
}

#' @importFrom dplyr select_
#' @export
select_.tbl_edwr <- function(.data, ..., .dots) {
    result <- NextMethod()
    reclass(.data, result)
}

#' @importFrom dplyr summarise
#' @export
summarise.tbl_edwr <- function(.data, ...) {
    result <- NextMethod()
    reclass(.data, result)
}

#' @importFrom dplyr summarise_
#' @export
summarise_.tbl_edwr <- function(.data, ..., .dots) {
    result <- NextMethod()
    reclass(.data, result)
}

#' @importFrom dplyr distinct
#' @export
distinct.tbl_edwr <- function(.data, ..., .keep_all = FALSE) {
    result <- NextMethod()
    reclass(.data, result)
}

#' @importFrom dplyr distinct_
#' @export
distinct_.tbl_edwr <- function(.data, ..., .dots, .keep_all = FALSE) {
    result <- NextMethod()
    reclass(.data, result)
}

#' @importFrom dplyr group_by
#' @export
group_by.tbl_edwr <- function(.data, ..., add = FALSE) {
    result <- NextMethod()
    reclass(.data, result)
}

#' @importFrom dplyr group_by_
#' @export
group_by_.tbl_edwr <- function(.data, ..., .dots = list(), add = FALSE) {
    result <- NextMethod()
    reclass(.data, result)
}

#' @importFrom dplyr ungroup
#' @export
ungroup.tbl_edwr <- function(x, ...) {
    result <- NextMethod()
    reclass(x, result)
}

#' @importFrom dplyr anti_join
#' @export
anti_join.tbl_edwr <- function(x, y, by = NULL, copy = FALSE, ...) {
    result <- NextMethod()
    reclass(x, result)
}

#' @importFrom dplyr full_join
#' @export
full_join.tbl_edwr <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
    result <- NextMethod()
    reclass(x, result)
}

#' @importFrom dplyr inner_join
#' @export
inner_join.tbl_edwr <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
    result <- NextMethod()
    reclass(x, result)
}

#' @importFrom dplyr left_join
#' @export
left_join.tbl_edwr <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
    result <- NextMethod()
    reclass(x, result)
}

#' @importFrom dplyr right_join
#' @export
right_join.tbl_edwr <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
    result <- NextMethod()
    reclass(x, result)
}

#' @importFrom dplyr semi_join
#' @export
semi_join.tbl_edwr <- function(x, y, by = NULL, copy = FALSE, ...) {
    result <- NextMethod()
    reclass(x, result)
}
