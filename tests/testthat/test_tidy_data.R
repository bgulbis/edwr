# tests for tidy_data

dir <- paste0(system.file(package = "edwr", "extdata"))

library(edwr)
context("Check tidy_data")

test_that("returns proper class", {
    expect_s3_class(tidy_data(labs), "labs")

})

test_that("returns default method warning", {
    tmp <- read_data(dir, "test_demographics.csv", "demographics")
    expect_warning(tidy_data(tmp), "No tidy_data method available for class")
})
