
#' Calculate morphine equivalents
#'
#' This takes a data frame with columns: med, med.dose, med.dose.units, and
#' route and will convert the med.dose to the morphine equivalent.
#'
#' @param x data_frame
#'
#' @return data_frame
#'
#' @references Lexicomp - Opiod Agonist Conversion (from drug, to Morphine IM);
#' https://www.cms.gov/Medicare/Prescription-Drug-Coverage/PrescriptionDrugCovContra/Downloads/Opioid-Morphine-EQ-Conversion-Factors-March-2015.pdf (note, doses listed in oral mme, converted to iv mme by * 0.3);
#' http://clincalc.com/opioids/
#'
#' @export
calc_morph_eq <- function(x) {
    # med.dose <- enquo(med.dose)

    med <- sym("med")
    route.group <- sym("route.group")
    dose.mg <- sym("dose.mg")
    med.dose.units <- sym("med.dose.units")

    convert <- quos(med == "buprenorphine" & route.group == "TOP" ~ 3.8,
                    med == "buprenorphine" & route.group == "PO" ~ 3,
                    med == "butorphenol" ~ dose.mg * 5,
                    stringr::str_detect(med, "codeine") & route.group == "PO" ~ dose.mg * 0.1,
                    med == "fentanyl" & route.group == "IV" ~ dose.mg * 0.1,
                    med == "fentanyl" & route.group == "NASAL" ~ dose.mg * 0.16 * 0.3,
                    med == "fentanyl" & route.group == "TOP" ~ dose.mg * 7.2 * 0.3,
                    stringr::str_detect(med, "hydrocodone") ~ dose.mg * 1 * 0.3,
                    med == "hydromorphone" & route.group == "PO" ~ dose.mg * 1.3,
                    med == "hydromorphone" & route.group == "IV" ~ dose.mg * 6.7,
                    med == "levorphanol" ~ dose.mg * 5,
                    med == "mepridine" ~ dose.mg * 0.1,
                    med == "methadone" & route.group == "PO" ~ dose.mg * 3 * 0.3,
                    med == "morphine" & route.group == "PO" ~ dose.mg * 0.3,
                    med == "nalbuphine" ~ dose.mg * 1,
                    med == "opium" ~ dose.mg * 1 * 0.3,
                    stringr::str_detect(med, "oxycodone") & route.group == "PO" ~ dose.mg * 0.5,
                    med == "oxymorphone" & route.group == "PO" ~ dose.mg * 1,
                    med == "oxymorphone" & route.group == "IV" ~ dose.mg * 10,
                    med == "pentazocine" ~ dose.mg * 0.37 * 0.3,
                    med == "tapentadol" ~ dose.mg * 0.1,
                    med == "remifentanil" & med.dose.units == "mg" ~ dose.mg * 100, # dose recorded in mg; assume equiv with fentanyl
                    med == "sufentanil" ~ dose.mg * 0.5, # 100mcg = 50mg
                    med == "tramadol" ~ dose.mg * 0.1 * 0.3,
                    TRUE ~ dose.mg)

    df <- x %>%
        mutate(!!"mme.iv" := dplyr::case_when(!!!convert))

    reclass(x, df)
}

