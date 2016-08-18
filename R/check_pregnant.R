# check_pregnant.R

#' Check for pregnant patients
#'
#' \code{check_pregnant} takes data frames with either diagnosis codes or labs
#' and returns a data frame with identifiers of pregnant patients
#'
#' This function takes a data frame of either class diagnosis or labs and
#' returns a data frame with a list of all patients who are pregnant. This can
#' be used to exclude patients from research studies.
#'
#' @param x A data frame of either class diagnosis or labs
#'
#' @return A data frame
#'
#' @export
check_pregnant <- function(x) {
    if (is.diagnosis(x)) {
        icd9 <- stringr::str_detect(names(icd::icd9_chapters), "Pregnancy")
        codes.icd9 <- list(pregnant = icd::icd_expand_range(
            start = icd::icd9_chapters[icd9][[1]][[1]],
            end = icd::icd9_chapters[icd9][[1]][[2]]))

        preg.icd9 <- filter_(x, .dots = list(~code.source == "ICD-9-CM")) %>%
            icd::icd9_comorbid(map = codes.icd9,
                               icd_name = "diag.code",
                               return_df = TRUE) %>%
            filter_(.dots = list(~pregnant == TRUE))

        # get list of ICD-10 codes related to pregnancy / complications
        icd10 <- stringr::str_detect(names(icd::icd10_chapters), "Pregnancy")
        codes.icd10 <- list(pregnant = icd::icd_expand_range(
            start = icd::icd10_chapters[icd10][[1]][[1]],
            end = icd::icd10_chapters[icd10][[1]][[2]]))

        preg.icd10 <- filter_(x, .dots = list(~code.source == "ICD-10-CM")) %>%
            icd::icd9_comorbid(map = codes.icd10,
                               icd_name = "diag.code",
                               return_df = TRUE) %>%
            filter_(.dots = list(~pregnant == TRUE))

        preg <- tibble::data_frame_(list(
            pie.id = ~c(preg.icd9$pie.id, preg.icd10$pie.id)
        ))

    } else if (is.labs(x)) {
        # consider pregnant if they have a positve urine or serum pregnancy test
        preg.test <- filter_(x, .dots = list(
            ~lab %in% c("u preg", "s preg"),
            ~lab.result == "Positive"))

        # or if beta hcg is > 5
        bhcg <- filter_(x, .dots = list(~lab %in% c("hcg tot", "hcg total"))) %>%
            tidy_data() %>%
            filter_(.dots = list(~lab.result > 5))

        preg <- tibble::data_frame_(list(
            pie.id = ~c(preg.test$pie.id, bhcg$pie.id)
        ))
    } else {
        stop("Data must be of class diagnosis or labs")
    }

    distinct_(preg)
}
