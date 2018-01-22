library(tidyverse)
library(edwr)

attr(meds_sched, "data") <- "edw"
x <- meds_sched %>%
    mutate_at("med.dose", as.numeric) %>%
    calc_runtime()
y <- summarize_data(x)

attr(labs, "data") <- "edw"
z <- labs %>%
    tidy_data() %>%
    calc_runtime() %>%
    summarize_data()
