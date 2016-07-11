# helper functions

# make sure all patients are included in the data frame and the values for added
# patients are FALSE
add_patients <- function(tidy, patients) {
    tidy <- dplyr::full_join(tidy, patients["pie.id"], by = "pie.id") %>%
        dplyr::group_by_("pie.id") %>%
        dplyr::mutate_at(dplyr::vars(), function(x) dplyr::coalesce(x, FALSE)) %>%
        dplyr::ungroup()
}


# Count the number of rows to go back in data frame. Takes a vector of POSIXct
# and counts the number of rows which would fall within the specified time
# frame. Typically called from dplyr::mutate and the results are passed on to
# zoo::rollapplyr.
count_rowsback <- function(x, back = 2) {
    curr_val <- function(y) {
        sum(x >= y - lubridate::days(back) & x <= y)
    }

    purrr::map_int(x, curr_val)
}
