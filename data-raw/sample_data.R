# sample data

library(dplyr)
library(stringr)
library(lubridate)
library(readr)
library(edwr)

dir.sample <- "data-raw/sample"
dir.tests <- "tests/testthat"
rnum <- sample.int(100000, 1)
rdays <- sample.int(15, 1)

# store sample demographics data as csv file to use for read_data tests
demographics <- read_data(dir.sample, "demographics") %>%
    # filter(`PowerInsight Encounter Id` %in% pts.sample$`PowerInsight Encounter Id`) %>%
    mutate(
        `PowerInsight Encounter Id` = as.character(
            as.numeric(`PowerInsight Encounter Id`) + rnum),
        `Person ID` = as.character(as.numeric(`Person ID`) + rnum),
        `Person Location- Facility (Curr)` = "Hospital"
    )

write_csv(demographics, "inst/extdata/demographics.csv")

# get sample data for examples and tests
labs <- read_data(dir.sample, "labs") %>%
    as.labs() %>%
    filter(lab %in% c("hgb", "platelet", "wbc", "inr", "ptt")) %>%
    mutate(pie.id = as.character(as.numeric(pie.id) + rnum),
           lab.datetime = lab.datetime + ddays(rdays))

meds_home <- read_data(dir.sample, "meds_home") %>%
    as.meds_home() %>%
    mutate(pie.id = as.character(as.numeric(pie.id) + rnum))

med.sample <- read_data(dir.sample, "meds_cont") %>%
    as.meds_cont() %>%
    filter(med == "heparin") %>%
    distinct(pie.id) %>%
    sample_n(3)

meds_cont <- read_data(dir.sample, "meds_cont") %>%
    as.meds_cont() %>%
    filter(pie.id %in% med.sample$pie.id) %>%
    mutate(pie.id = as.character(as.numeric(pie.id) + rnum),
           order.id = as.character(as.numeric(order.id) + rnum),
           event.id = as.character(as.numeric(event.id) + rnum),
           med.datetime = med.datetime + ddays(rdays))

meds_sched <- read_data(dir.sample, "meds_sched") %>%
    as.meds_sched() %>%
    filter(pie.id %in% med.sample$pie.id) %>%
    mutate(pie.id = as.character(as.numeric(pie.id) + rnum),
           order.id = as.character(as.numeric(order.id) + rnum),
           event.id = as.character(as.numeric(event.id) + rnum),
           med.datetime = med.datetime + ddays(rdays))

warfarin <- read_data(dir.sample, "warfarin") %>%
    as.warfarin() %>%
    mutate(pie.id = as.character(as.numeric(pie.id) + rnum),
           warfarin.datetime = warfarin.datetime + ddays(rdays))

hosp <- c("Jones" = "Smith", "Hermann" = "George", "HVI" = "HeartHosp",
          "Cullen" = "Roy", "PAHH" = "PACU")
locations <- read_data(dir.sample, "locations") %>%
    as.locations() %>%
    mutate(pie.id = as.character(as.numeric(pie.id) + rnum),
           arrive.datetime = arrive.datetime + ddays(rdays),
           depart.datetime = depart.datetime + ddays(rdays),
           unit.from = str_replace_all(unit.from, hosp),
           unit.to = str_replace_all(unit.to, hosp))

services <- read_data(dir.sample, "services") %>%
    as.services() %>%
    mutate(pie.id = as.character(as.numeric(pie.id) + rnum),
           start.datetime = start.datetime + ddays(rdays),
           end.datetime = end.datetime + ddays(rdays))

vent_times <- read_data(dir.sample, "vent_times") %>%
    as.vent_times() %>%
    mutate(pie.id = as.character(as.numeric(pie.id) + rnum),
           vent.datetime = vent.datetime + ddays(rdays))

visits <- read_data(dir.sample, "visits") %>%
    as.visits() %>%
    mutate(pie.id = as.character(as.numeric(pie.id) + rnum),
           arrival.datetime = arrival.datetime + ddays(rdays),
           admit.datetime = admit.datetime + ddays(rdays),
           discharge.datetime = discharge.datetime + ddays(rdays),
           facility = "Hospital",
           nurse.unit.admit = str_replace_all(nurse.unit.admit, hosp))

diagnosis <- read_data(dir.sample, "diagnosis") %>%
    as.diagnosis() %>%
    mutate(pie.id = as.character(as.numeric(pie.id) + rnum))

# save data for use in package
devtools::use_data(labs, meds_cont, meds_home, meds_sched, warfarin,
                   overwrite = TRUE)

# save data for tests
saveRDS(locations, paste(dir.tests, "locations.Rds", sep = "/"))
saveRDS(services, paste(dir.tests, "services.Rds", sep = "/"))
saveRDS(vent_times, paste(dir.tests, "vent_times.Rds", sep = "/"))
saveRDS(visits, paste(dir.tests, "visits.Rds", sep = "/"))
saveRDS(diagnosis, paste(dir.tests, "diagnosis.Rds", sep = "/"))

rm(rnum, rdays)
