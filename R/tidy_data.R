#' Tidy EDW data
#'
#' \code{tidy_data} tidy data from standard EDW queries
#'
#' This function calls the underlying tidy function based on the value passed to
#' the type parameter and returns the tidy data frame. Valid options for type
#' are: icd9, icd10, labs, locations, meds_cont, meds_outpt, meds_sched,
#' services, visit_times.
#'
#' @param raw.data A data frame with the data to be tidied
#' @param type A character indicating what type of data is being tidied
#' @param ... parameters to pass on to the underlying tidy function
#'
#' @return A data frame
#'
#' @export
tidy_data <- function(raw.data, type, ...) {
    home <- TRUE
    patients <- NULL
    censor <- TRUE
    ref.data <- NULL
    sched.data <- NULL
    visit.times <- NULL

    # get list of parameters from ellipsis
    x <- list(...)
    list2env(x, environment())

    # call the desired tidy function based on type
    switch(type,
           icd9 = tidy_icd(raw.data, ref.data, FALSE, patients),
           icd10 = tidy_icd(raw.data, ref.data, TRUE, patients),
           labs = tidy_labs(raw.data, censor),
           locations = tidy_locations(raw.data),
           meds_cont = tidy_meds_cont(raw.data, ref.data, sched.data),
           meds_outpt = tidy_meds_outpt(raw.data, ref.data, patients, home),
           meds_sched = tidy_meds_sched(raw.data, ref.data),
           services = tidy_services(raw.data),
           vent_times = tidy_vent_times(raw.data, visit.times),
           stop("Invalid type")
    )
}

# make sure all patients are included in the table
add_patients <- function(tidy, patients) {
    # Change NA to FALSE
    fill_false <- function(x) {
        x[is.na(x)] <- FALSE
        x
    }

    tidy <- dplyr::full_join(tidy, patients["pie.id"], by = "pie.id")
    tidy <- dplyr::mutate_each_(tidy, dplyr::funs("fill_false"),
                                list(quote(-pie.id)))
}

