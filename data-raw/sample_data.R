# sample data

library(dplyr)
library(stringr)
library(lubridate)
library(readr)
library(edwr)

dir.sample <- "data-raw/sample"
rnum <- sample.int(100000, 1)
rdays <- sample.int(15, 1)

# store sample demographics data as csv file to use for read_data tests
demographics <- read_data(dir.sample, "demographics", "skip") %>%
    # filter(`PowerInsight Encounter Id` %in% pts.sample$`PowerInsight Encounter Id`) %>%
    mutate(
        `PowerInsight Encounter Id` = as.character(
            as.numeric(`PowerInsight Encounter Id`) + rnum),
        `Person ID` = as.character(as.numeric(`Person ID`) + rnum),
        `Person Location- Facility (Curr)` = "Hospital"
    )

write_csv(demographics, "inst/extdata/demographics.csv")

# get sample data for examples / tests
x <- read_data(dir.sample, "diagnosis")
diagnosis <- x %>%
    mutate(pie.id = as.character(as.numeric(pie.id) + rnum))
class(diagnosis) <- class(x)

x <- read_data(dir.sample, "labs")
labs <- x %>%
    filter(lab %in% c("hgb", "platelet", "wbc", "inr", "ptt")) %>%
    mutate(pie.id = as.character(as.numeric(pie.id) + rnum),
           lab.datetime = lab.datetime + days(rdays))
class(labs) <- class(x)

x <- read_data(dir.sample, "meds_home")
meds_home <- mutate(x, pie.id = as.character(as.numeric(pie.id) + rnum))
class(meds_home) <- class(x)

med.sample <- read_data(dir.sample, "meds_cont") %>%
    filter(med == "heparin") %>%
    distinct(pie.id) %>%
    sample_n(3)

x <- read_data(dir.sample, "meds_cont")
meds_cont <- x %>%
    filter(pie.id %in% med.sample$pie.id) %>%
    mutate(pie.id = as.character(as.numeric(pie.id) + rnum),
           order.id = as.character(as.numeric(order.id) + rnum),
           event.id = as.character(as.numeric(event.id) + rnum),
           med.datetime = med.datetime + days(rdays))
class(meds_cont) <- class(x)

x <- read_data(dir.sample, "meds_sched")
meds_sched <- x %>%
    filter(pie.id %in% med.sample$pie.id) %>%
    mutate(pie.id = as.character(as.numeric(pie.id) + rnum),
           order.id = as.character(as.numeric(order.id) + rnum),
           event.id = as.character(as.numeric(event.id) + rnum),
           med.datetime = med.datetime + days(rdays))
class(meds_sched) <- class(x)

x <- read_data(dir.sample, "warfarin")
warfarin <- x %>%
    mutate(pie.id = as.character(as.numeric(pie.id) + rnum),
           warfarin.datetime = warfarin.datetime + days(rdays))
class(warfarin) <- class(x)

hosp <- c("Jones" = "Smith", "Hermann" = "George", "HVI" = "HeartHosp",
          "Cullen" = "Roy", "PAHH" = "PACU")
x <- read_data(dir.sample, "locations")
locations <- x %>%
    mutate(pie.id = as.character(as.numeric(pie.id) + rnum),
           arrive.datetime = arrive.datetime + days(rdays),
           depart.datetime = depart.datetime + days(rdays),
           unit.from = str_replace_all(unit.from, hosp),
           unit.to = str_replace_all(unit.to, hosp))
class(locations) <- class(x)

# save data for use in package
devtools::use_data(diagnosis, labs, meds_cont, meds_home, meds_sched, warfarin,
                   locations, overwrite = TRUE)

rm(rnum, rdays)
