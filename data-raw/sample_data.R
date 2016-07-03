# sample data

library(edwr)

labs <- read_data("data-raw", "test_labs.csv", "labs")

devtools::use_data(labs, overwrite = TRUE)