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
    icd9 <- sym("icd9")
    pregnant <- sym("pregnant")

    preg9 <- x %>%
        filter(!!icd9) %>%
        icd::icd9_comorbid(
            map = preg.icd9,
            icd_name = "diag.code",
            return_df = TRUE
        ) %>%
        fitler(!!pregnant)

    preg10 <- x %>%
        filter(!!icd9 == FALSE) %>%
        icd::icd10_comorbid(
            map = preg.icd10,
            icd_name = "diag.code",
            return_df = TRUE
        ) %>%
        fitler(!!pregnant)

    id <- set_id_name(x)

    df <- preg9[id] %>%
        full_join(preg10[id], by = id) %>%
        distinct()

    reclass(x, df)
}

#' @export
#' @rdname check_pregnant
check_pregnant.labs <- function(x, ...) {
    # consider pregnant if they have a positve urine or serum pregnancy test
    preg.test <- x %>%
        filter(
            !!parse_expr("lab %in% c('u preg', 's preg')"),
            !!parse_expr("lab.result == 'Positive'")
        )

    # or if beta hcg is > 5
    bhcg <- x %>%
        filter(!!parse_expr("lab %in% c('hcg tot', 'hcg total')")) %>%
        tidy_data() %>%
        filter(!!sym("lab.result") > 5)

    id <- set_id_name(x)

    df <- preg.test[id] %>%
        full_join(bhcg[id], by = id) %>%
        distinct()

    reclass(x, df)
}
