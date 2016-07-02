# sample data

library(edwr)

dir <- paste0(system.file(package = "edwr", "extdata"))

labs <- read_data(dir, "test_labs.csv", "labs")

devtools::use_data(labs)