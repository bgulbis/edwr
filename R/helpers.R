# helper functions

# make sure all patients are included in the data frame and the values for added
# patients are FALSE
add_patients <- function(tidy, patients) {
    tidy <- dplyr::full_join(tidy, patients["pie.id"], by = "pie.id") %>%
        dplyr::group_by_("pie.id") %>%
        dplyr::mutate_at(dplyr::vars(), function(x) dplyr::coalesce(x, FALSE)) %>%
        dplyr::ungroup()
}
