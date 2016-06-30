# tests for read_edw_data

dir <- paste0(system.file(package = "edwr", "extdata"))

library(edwr)
context("Check tidy_data")

test_that("returns proper class", {
    tmp <- read_edw_data(dir, "test_labs.csv", "labs")
    expect_s3_class(tidy_data(tmp), "edw_labs")
})
