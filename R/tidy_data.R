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
#' @seealso \code{\link{tidy_icd}}
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
    # switch(type,
    #        icd9 = tidy_icd(raw.data, ref.data, FALSE, patients),
    #        icd10 = tidy_icd(raw.data, ref.data, TRUE, patients),
    #        labs = tidy_labs(raw.data, censor),
    #        locations = tidy_locations(raw.data),
    #        meds_cont = tidy_meds_cont(raw.data, ref.data, sched.data),
    #        meds_outpt = tidy_meds_outpt(raw.data, ref.data, patients, home),
    #        meds_sched = tidy_meds_sched(raw.data, ref.data),
    #        services = tidy_services(raw.data),
    #        vent_times = tidy_vent_times(raw.data, visit.times),
    #        stop("Invalid type")
    # )
}

# make sure all patients are included in the table
add_patients <- function(tidy, patients) {
    # Change NA to FALSE
    fill_false <- function(x) {
        x[is.na(x)] <- FALSE
        x
    }

    tidy <- dplyr::full_join(tidy, patients["pie.id"], by = "pie.id") %>%
        dplyr::mutate_each_(dplyr::funs("fill_false"), list(quote(-pie.id)))
}

#' Tidy ICD9/10 diagnosis codes
#'
#' \code{tidy_icd} determines which patients have the desired ICD9/10 diagnosis
#'
#' This function is called by \code{\link{tidy_data}} with a \code{type}
#' argument of \code{icd9} or \code{icd10}.
#'
#' The function takes a data frame with reference diagnosis codes and a data
#' frame with all patient diagnosis codes, and returns a data frame with a
#' column for each disease state for each patient.
#'
#' @param raw.data A data frame with all patient diagnosis codes
#' @param ref.data A data frame with the desired diagnosis codes
#' @param icd10 A logical indicating whether to use ICD-10 codes or ICD-9 codes
#'   (default)
#' @param patients An optional data frame with a column pie.id including all
#'   patients in study
#'
#' @return A data frame
#'
#' @seealso \code{\link{tidy_data}}
#'
#' @importFrom magrittr "%>%"
tidy_icd <- function(raw.data, ref.data, icd10 = FALSE, patients = NULL) {
    # # convert any CCS codes to ICD
    # lookup.codes <- icd_lookup(ref.data, icd10) %>%
    #     dplyr::ungroup() %>%
    #     dplyr::mutate_(.dots = setNames(list(~factor(disease.state)), "disease.state"))
    #
    # # only use finalized diagnosis codes
    # dots <- list(~diag.type != "Admitting", ~diag.type != "Working")
    # tidy <- dplyr::filter_(raw.data, .dots = dots)
    #
    # # join with the lookup codes
    # tidy <- dplyr::inner_join(tidy, lookup.codes, by = c("diag.code" = "icd.code"))
    #
    # # add a column called value and assign as TRUE, to be used with spread
    # tidy$value <- TRUE
    #
    # # drop all columns except pie.id, disease state, and value
    # tidy <- dplyr::select_(tidy, .dots = list("pie.id", "disease.state", "value"))
    #
    # # remove all duplicate pie.id / disease state combinations
    # dots <- list("pie.id", "disease.state")
    # tidy <- dplyr::distinct_(tidy, .dots = dots)
    #
    # # convert the data to wide format
    # tidy <- tidyr::spread_(tidy, "disease.state", "value", fill = FALSE, drop = FALSE)
    #
    # # join with list of all patients, fill in values of FALSE for any patients
    # # not in the data set
    # if (!is.null(patients)) {
    #     tidy <- add_patients(tidy, patients)
    # }
    #
    # tidy
}
