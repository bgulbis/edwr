# lab_change.R

#' Determine if a lab changed by a set amount within a specific time frame
#'
#' \code{lab_change} checks for changes in lab values
#'
#' This function takes a data frame with lab data for a single lab and checks
#' whether the lab changes by a certain amount within a given period of time.
#'
#' For FUN, use max when looking for a decrease in lab value, and min when
#' looking for an increase in lab value.
#'
#' @param x A data frame with lab data
#' @param .lab A character string indicating the name of the lab to evaluate
#' @param change.by A numeric indicating the threshold for lab changes
#' @param FUN A function for \code{rollapplyr}, most commonly max or
#'   min
#' @param back An optional numeric specifying the number of days back to go.
#'   Defaults to 2 days.
#'
#' @return A data frame
#'
#' @examples
#' # checks for a >= 2 decrease in the hemoglobin value within the past 2 days
#' x <- tidy_data(labs)
#'
#' print(head(
#'   lab_change(x, "hgb", -2, max, back = 2)
#' ))
#'
#' @export
lab_change <- function(x, .lab, change.by, FUN, back = 2) {
    if ("lab.result" %in% colnames(x)) {
        lab_col <- "lab"
        lab_datetime <- sym("lab.datetime")
        lab_result <- sym("lab.result")
    } else if ("event.result" %in% colnames(x)) {
        lab_col <- "event"
        lab_datetime <- sym("event.datetime")
        lab_result <- sym("event.result")
    } else {
        warning("No valid result column found, need lab.result or event.result")
        return(x)
    }

    # calculate the number of rows that are included within the window, then
    # calculate the running min/max during the time window, then calculate the
    # change from the running min/max to current value, then filter values which
    # exceed the change.by value
    id <- set_id_quo(x)

    lab <- sym(lab_col)
    rowsback <- sym("rowsback")

    df <- x %>%
        filter(
            !!parse_expr(
                paste(lab_col, "%in% .lab")
            )
        ) %>%
        arrange(!!id, !!lab, !!lab_datetime) %>%
        group_by(!!id, !!lab) %>%
        mutate(!!"rowsback" := count_rowsback(!!lab_datetime, back)) %>%
        filter(!is.na(!!rowsback)) %>%
        mutate(
            !!"running" := zoo::rollapplyr(
                !!lab.result,
                !!rowsback,
                FUN,
                fill = NA,
                partial = TRUE
            ),
            !!"change" := !!lab_result - !!sym("running")
        ) %>%
        filter(!!parse_expr("abs(change) >= abs(change.by)")) %>%
        ungroup

    reclass(x, df)
}
