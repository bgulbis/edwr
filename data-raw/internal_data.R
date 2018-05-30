# internal_data.R
#
# data to be used internally in package

# medication classes downloaded from EDW
# last updated: 9/27/2017

library(dplyr)
library(icd)

dirr::gzip_files("data-raw", pattern = "medication_classes.csv")

med.classes <- readr::read_csv("data-raw/medication_classes.csv.gz") %>%
    rename(med.class = `Drug Catalog`,
           med.name = `Generic Drug Name`) %>%
    distinct

icd9 <- stringr::str_detect(names(icd::icd9_chapters), "Pregnancy")
preg.icd9 <- list(pregnant = icd::icd_expand_range(
    start = icd::icd9_chapters[icd9][[1]][[1]],
    end = icd::icd9_chapters[icd9][[1]][[2]]))

icd10 <- stringr::str_detect(names(icd::icd10_chapters), "Pregnancy")
preg.icd10 <- list(pregnant = icd::icd_expand_range(
    start = icd::icd10_chapters[icd10][[1]][[1]],
    end = icd::icd10_chapters[icd10][[1]][[2]]))

devtools::use_data(med.classes, preg.icd9, preg.icd10,
                   internal = TRUE,
                   overwrite = TRUE)
