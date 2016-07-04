# sample data

library(edwr)

diagnosis <- readRDS("data-raw/test_diag.Rds")
labs <- readRDS("data-raw/test_labs.Rds")
meds_cont <- readRDS("data-raw/test_meds_cont.Rds")
meds_home <- readRDS("data-raw/test_meds_home.Rds")
meds_sched <- readRDS("data-raw/test_meds_sched.Rds")
warfarin <- readRDS("data-raw/test_warf.Rds")

devtools::use_data(diagnosis, labs, meds_cont, meds_home, meds_sched, warfarin,
                   overwrite = TRUE)
