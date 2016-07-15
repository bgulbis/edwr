# helper functions

# make sure all patients are included in the data frame and the values for added
# patients are FALSE
add_patients <- function(tidy, patients) {
    dplyr::full_join(tidy, patients["pie.id"], by = "pie.id") %>%
        dplyr::group_by_("pie.id") %>%
        dplyr::mutate_at(dplyr::vars(),
                         function(x) dplyr::coalesce(x, FALSE)) %>%
        dplyr::ungroup()
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
    curr_val <- function(y) {
        sum(x >= y - lubridate::days(back) & x <= y)
    }

    purrr::map_int(x, curr_val)
}

#' Set the default format for reading date/time variables
#'
#' Set the default format for parsing date/time variables when creating
#' \code{edwr} classes.
#'
#' @param x character vector of date/time data
#'
#' @return A \code{\link[readr]{collector}} object
#'
#' @keywords internal
format_dates <- function(x) {
    readr::parse_datetime(
        x = x,
        format = "%Y/%m/%d %H:%M:%S",
        locale = readr::locale(tz = "US/Central")
    )
}
