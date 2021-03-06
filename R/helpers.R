# helper functions

#' Add all included patients to data frame
#'
#' Makes sure all patients are included in the data frame and the values for
#' added patients are \code{FALSE}
#'
#' @param tidy data_frame with the data being returned
#' @param patients data_frame which contains all included patients
#'
#' @return data_frame
#' @keywords internal
add_patients <- function(tidy, patients) {
    id <- set_id_name(tidy)

    tidy %>%
        full_join(patients[id], by = id) %>%
        ungroup()
}

#' Count the number of rows to go back in data frame
#'
#' Takes a vector of POSIXct and counts the number of rows which would fall
#' within the specified time frame. Typically called from
#' \code{mutate} and the results are passed on to
#' \code{rollapplyr}.
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

#' Convert date/time variables to POSIXct
#'
#' Converts date/time variables to POSIXct and adjusts to US/Central timezone.
#'
#' @param x character vector of date/time data
#' @param date_col character of column names to be converted
#' @param tz optional string with a properly formatted time zone; if given, will
#'   override the default time zone assignments for EDW and MBO data
#' @param date_only logical indicating if there is only a date without time
#'
#' @return A tibble
#'
#' @export
format_dates <- function(x, date_col, tz = NULL, date_only = FALSE) {
    if (is.null(tz)) {
        tzone <- "US/Central"
        if (attr(x, "data") == "mbo") tzone <- "UTC"
    } else {
        tzone <- tz
    }

    if ("archive" %in% names(attributes(x))) {
        archive <- attr(x, "archive")
    } else {
        archive <- FALSE
    }

    if (archive) {
        if (date_only) {
            orders <- "%m/%d/%y"
        } else {
            orders <- "%m/%d/%Y %I:%M:%S %p"
        }

        x %>%
            dplyr::mutate_at(
                date_col,
                lubridate::parse_date_time,
                orders = orders,
                tz = tzone
            ) %>%
            dplyr::mutate_at(date_col, lubridate::with_tz, tzone = "US/Central")
    } else {
        x %>%
            dplyr::mutate_at(date_col, lubridate::ymd_hms, tz = tzone) %>%
            dplyr::mutate_at(date_col, lubridate::with_tz, tzone = "US/Central")
    }
}

#' Set timezone based on data source
#'
#' Sets the timezone to US/Central for EDW data, and UTC for MBO data
#'
#' @param x A tibble with an attribute of "data"
#'
#' @return character vector indicating the timezone
#'
#' @keywords internal
set_timezone <- function(x) {
    tzone <- "US/Central"
    if (attr(x, "data") == "mbo") tzone <- "UTC"
    tzone
}

#' Set the name of the id field based on data source
#'
#' Sets the id name to pie.id for EDW data and millennium.id for MBO data
#'
#' @param x A tibble with an attribute of "data"
#'
#' @return character vector with the id name
#'
#' @keywords internal
set_id_name <- function(x) {
    if (attr(x, "data") == "edw") {
        "pie.id"
    } else {
        "millennium.id"
    }
}

#' Set the name of the id field based on data source as a quosure
#'
#' Sets the id name to pie.id for EDW data and millennium.id for MBO data
#'
#' @param x A tibble with an attribute of "data"
#'
#' @return character vector with the id name
#'
#' @keywords internal
set_id_quo <- function(x) {
    if (attr(x, "data") == "edw") {
        quo(!!sym("pie.id"))
    } else {
        quo(!!sym("millennium.id"))
    }
}

