# tests for read_edw_data

dir <- paste0(system.file(package = "edwr", "extdata"))

library(edwr)
context("Check read_edw_data")

test_that("invalid type returns error", {
    expect_error(read_edw_data(dir, "test_demographics.csv", "demograph"), "Invalid type")
})

test_that("returns proper class", {
    expect_s3_class(read_edw_data(dir, "test_demographics.csv", "demographics"), "tbl_df")
    expect_s3_class(read_edw_data(dir, "test_demographics.csv", "demographics"), "edw_demographics")
})

test_examples()