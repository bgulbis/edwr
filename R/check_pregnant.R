# check_pregnant.R

#' Identify pregnant patients
#'
#' \code{check_pregnant} takes data frames with either diagnosis codes or labs
#' and returns a data frame with identifiers of pregnant patients
#'
#' This function takes a data frame of either class diagnosis or labs and
#' returns a data frame with a list of all patients who are pregnant. This can
#' be used to exclude patients from research studies.
#'
#' @param x A data frame of either class diagnosis or labs
#' @param ... additional arguments passed on to individual methods
#'
#' @return A data frame
#'
#' @export
check_pregnant <- function(x, ...) {
    UseMethod("check_pregnant")
}

#' @export
#' @rdname check_pregnant
check_pregnant.default <- function(x, ...) {
    warning("No method available for objects of this class")
    x
}

#' @export
#' @rdname check_pregnant
check_pregnant.diagnosis <- function(x, ...) {
    preg9 <- check_icd(x, "icd9") %>%
        icd::icd9_comorbid(map = preg.icd9,
                           icd_name = "diag.code",
                           return_df = TRUE) %>%
        filter_(.dots = list(~pregnant == TRUE))

    preg10 <- check_icd(x, "icd10") %>%
        icd::icd10_comorbid(map = preg.icd10,
                           icd_name = "diag.code",
                           return_df = TRUE) %>%
        filter_(.dots = list(~pregnant == TRUE))

    full_join(preg9["pie.id"], preg10["pie.id"], by = "pie.id") %>%
        distinct_()
}

#' @export
#' @rdname check_pregnant
check_pregnant.labs <- function(x, ...) {
    # consider pregnant if they have a positve urine or serum pregnancy test
    preg.test <- filter_(x, .dots = list(
        ~lab %in% c("u preg", "s preg"),
        ~lab.result == "Positive"))

    # or if beta hcg is > 5
    bhcg <- filter_(x, .dots = list(~lab %in% c("hcg tot", "hcg total"))) %>%
        tidy_data() %>%
        filter_(.dots = list(~lab.result > 5))

    full_join(preg.test["pie.id"], bhcg["pie.id"], by = "pie.id") %>%
        distinct_()
}
