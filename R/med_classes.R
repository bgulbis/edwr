# med_classes.R

#' Lookup medications by medication class
#'
#' \code{med_lookup} takes a vector of medication classes and returns all
#' medications in those classes
#'
#' This function takes a character vector of medication classes and returns a
#' data frame with all of the medications (by generic name) contained in the
#' medication class. The data frame will contain two columns: med.class and
#' med.name.
#'
#' Medication class data comes from the Enterprise Data Warehouse.
#'
#' @param med_class A character vector of medication classes
#'
#' @return A data frame with columns: med.class and med.name
#'
#' @export
med_lookup <- function(med_class) {
    med.classes %>%
        filter(!!parse_expr("med.class %in% med_class"))
}

#' Lookup medication class by medication generic name
#'
#' \code{med_class_lookup} takes a vector of medication generic names and
#' returns the classes which contain those medications
#'
#' This function takes a character vector of medication generic names and
#' returns a data frame with all of the medication classes that contain those
#' medications. The data frame will contain two columns: med.class and med.name.
#'
#' @param meds A character vector of medication names
#'
#' @return A data frame with columns: med.class and med.name
#'
#' @export
med_class_lookup <- function(meds) {
    # make a regular expression string of desired meds, then check each
    # medication to see if it is in the list of desired meds, then filter only
    # medications which match, then remove contains from data frame

    contains <- sym("contains")

    med.classes %>%
        mutate(
            !!"contains" := stringr::str_detect(
                !!sym("med.name"),
                stringr::regex(
                    paste(meds, collapse = "|"),
                    ignore_case = TRUE
                )
            )
        ) %>%
        filter(!!contains) %>%
        select(-!!contains)
}
