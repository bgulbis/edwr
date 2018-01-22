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
