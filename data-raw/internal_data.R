# internal_data.R
#
# data to be used internally in package

# medication classes downloaded from EDW
# last updated: 6/30/2016

library(dplyr)

dirr::gzip_files("data-raw", pattern = "medication_classes.csv")

med.classes <- readr::read_csv("data-raw/medication_classes.csv.gz") %>%
    rename(med.class = `Drug Catalog`,
           med.name = `Generic Drug Name`) %>%
    distinct

devtools::use_data(med.classes, internal = TRUE, overwrite = TRUE)
