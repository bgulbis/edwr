# tests for read_edw_data

dir <- "../../inst/extdata"

library(edwr)
context("Invalid arguments")

test_that("invalid type returns error", {
    expect_error(read_edw_data(dir, "test_demographics.csv", "demograph"), "Invalid type")
})

context("Returns tbl_df")

test_that("returns tbl_df object", {
    expect_s3_class(read_edw_data(dir, "test_demographics.csv", "demographics"), "tbl_df")
})