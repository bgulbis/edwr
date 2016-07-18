
library(edwr)
library(icd)
library(dplyr)

x <- icd9_map_ahrq
diagnosis <- readRDS("tests/testthat/diagnosis.Rds")
.icd9 <- filter(diagnosis, code.source == "ICD-9-CM",
               icd_is_valid(as.icd9(diag.code))) #%>%
    # mutate(icd = icd_decimal_to_short(diag.code))

y <- icd_comorbid(.icd9, icd9_map_ahrq, visit_name = "pie.id",
                  icd_name = "diag.code") %>%
    icd_comorbid_mat_to_df()

my.map <- list(test = icd_decimal_to_short(as.icd9cm(c("250.00", "272.4")))) %>%
    as.icd_comorbidity_map()

my.map <- list(test = c("250.00", "272.4"))

z <- icd_comorbid(.icd9, my.map) %>%
    icd_comorbid_mat_to_df()

xx <- icd_long_data(.icd9)
