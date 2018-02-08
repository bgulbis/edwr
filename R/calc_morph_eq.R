
#' Calculate morphine equivalents
#'
#' This takes a data frame with columns: med, med.dose, med.dose.units, and
#' route and will convert the med.dose to the morphine equivalent.
#'
#' @param x data_frame
#'
#' @return data_frame
#' @export
calc_morph_eq <- function(x) {
    # med.dose <- enquo(med.dose)

    df <- x %>%
        mutate(!!"morph_eq" := !!parse_expr('dplyr::case_when(
            med == "hydromorphone" & (route == "IV" | route == "IVP") ~ med.dose * 0.4,
            med == "hydromorphone" & (route == "PO" | route == "NG") ~ med.dose / 0.8,
            med == "fentanyl" & (route == "IV" | route == "IVP") ~ med.dose * 0.1,
            med == "oxycodone" & (route == "PO" | route == "NG") ~ med.dose / 2.5,
            med == "acetaminophen-hydrocodone" & med.dose.units == "mL" ~ med.dose * 10 / 15 / 2.5,
            med == "acetaminophen-hydrocodone" & med.dose.units == "mg" ~ med.dose / 2.5,
            med == "acetaminophen-codeine" & med.dose.units == "mL" ~ med.dose * 12 / 5 / 6,
            med == "acetaminophen-codeine" & med.dose.units == "mg" ~ med.dose / 6,
            med == "tramadol" & (route == "PO" | route == "NG") ~ med.dose / 25,
            TRUE ~ med.dose)'))

    reclass(x, df)
}

