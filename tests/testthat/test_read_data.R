# tests for read_data

dir <- system.file(package = "edwr", "extdata")

library(edwr)
context("Check read_data")

test_that("returns proper class", {
    expect_s3_class(
        read_data(dir, "demographics.csv"), "tbl_edwr"
    )
})
