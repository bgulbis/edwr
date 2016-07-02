# helper functions

#' @importFrom magrittr %>%
add_patients <- function(tidy, patients) {
    # make sure all patients are included in the data frame and the values for
    # added patients are FALSE
    tidy <- dplyr::full_join(tidy, patients["pie.id"], by = "pie.id") %>%
        dplyr::mutate_at(
            .cols = dplyr::vars(quote(-pie.id)),
            .funs = function(x) dplyr::coalesce(x, FALSE)
        )
}
