# helper functions

#' Add all included patients to data frame
#'
#' Makes sure all patients are included in the data frame and the values for
#' added patients are \code{FALSE}
#'
#' @param tidy data_frame with the data being returned
#' @param patients data_frame with a column \code{pie.id} that contains all
#'   included patients
#'
#' @return data_frame
#' @keywords internal
add_patients <- function(tidy, patients) {
    full_join(tidy, patients["pie.id"], by = "pie.id") %>%
        group_by_("pie.id") %>%
        dplyr::mutate_at(dplyr::vars(),
                  function(x) dplyr::coalesce(x, FALSE)) %>%
        ungroup()
}

#' Count the number of rows to go back in data frame
#'
#' Takes a vector of POSIXct and counts the number of rows which would fall
#' within the specified time frame. Typically called from
#' \code{\link[dplyr]{mutate}} and the results are passed on to
#' \code{\link[zoo]{rollapplyr}}.
#'
#' @param x = vector of type POSIXct
#' @param back = integer indicating the number of days back to include
#'
#' @return integer vector
#'
#' @keywords internal
count_rowsback <- function(x, back = 2) {
    purrr::map_int(x, function(y) sum(x >= y - lubridate::days(back) & x <= y))
}

#' Set the default format for reading date/time variables
#'
#' Set the default format for parsing date/time variables when creating
#' \code{edwr} classes.
#'
#' @param x character vector of date/time data
#'
#' @return A \code{\link[readr]{parse_datetime}} object
#'
#' @keywords internal
format_dates <- function(x) {
    readr::parse_datetime(
        x = x,
        format = "%Y/%m/%d %H:%M:%S",
        locale = readr::locale(tz = "US/Central")
    )
}

#' Keep edwr class assignments
#'
#' @param x tibble with tbl_edwr class(es)
#' @param y tibble as returned by dplyr manipulation function
#'
#' @return tibble
#'
#' @keywords internal
keep_class <- function(x, y) {
    if(is.tbl_edwr(y)) return(y)

    cls <- match("tbl_edwr", class(x), nomatch = 0L)
    class(y) <- c(class(x)[1:cls], class(y))
    y
}
