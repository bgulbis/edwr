library(tidyverse)
library(edwr)
library(rlang)

attr(meds_sched, "data") <- "edw"
x <- meds_sched %>%
    mutate_at("med.dose", as.numeric) %>%
    calc_runtime() %>%
    summarize_data()

attr(labs, "data") <- "edw"
z <- labs %>%
    tidy_data() %>%
    calc_runtime() %>%
    summarize_data()

attr(meds_cont, "data") <- "edw"
y <- meds_cont %>%
    mutate_at("med.rate", as.numeric) %>%
    calc_runtime() %>%
    summarize_data()

id <- quo(!!sym("id"))
grp_by <- quos(!!id, !!sym("med"), !!sym("drip.count"))

quo_text(id)
c(quo_text(id), "med", "drip.count")

ref <- tibble::tibble(
    name = c("heparin", "warfarin", "antiplatelet agents"),
    type = c("med", "med", "class"),
    group = c("cont", "sched", "sched")
)
x <- summarize_data(meds_home, ref)
    # summarize_data(ref = ref)

filter(ref, !!parse_expr("type == 'class'"))

fx <- function(x, ref) {
    y <- filter(ref, !!parse_expr("type == 'class'"))
    print(y)
    meds <- med_lookup(y$name)
    print(meds)

    # join the list of meds with any indivdual meds included
    y <- filter(ref, !!parse_expr("type == 'med'"))
    lookup.meds <- c(y$name, meds$med.name)

    print(lookup.meds)
    # if (home) {
    #     f <- rlang::parse_expr("med.type == 'Recorded / Home Meds'")
    # } else {
    #     f <- rlang::parse_expr("med.type == 'Prescription / Discharge Order'")
    # }
    #
    df <- x %>%
        filter(
            !!rlang::parse_expr("med.type == 'Recorded / Home Meds'"),
            !!rlang::parse_expr("med %in% lookup.meds")
        ) %>%
        left_join(meds, by = c("med" = "med.name")) %>%
        mutate(
            !!"group" := !!rlang::parse_expr("dplyr::if_else(is.na(med.class), med, med.class)"),
            !!"value" := TRUE
        ) %>%
        distinct(!!!quos(!!id, !!sym("group"), !!sym("value"))) %>%
        tidyr::spread(!!sym("group"), !!sym("value"), fill = FALSE, drop = FALSE)

}

x <- fx(meds_home, ref)
