# tests for tidy_data

dir <- paste0(system.file(package = "edwr", "extdata"))

library(edwr)
context("Check tidy_data")

test_that("returns proper class", {
    expect_s3_class(tidy_data(labs), "labs")

})

test_that("returns default method warning", {
    tmp <- read_data(dir, "demographics.csv", "demographics")
    expect_warning(tidy_data(tmp), "No tidy_data method available for class")
})

test_that("location stays appropriate", {
    tmp <- tidy_data(locations)
    expect_gt(nrow(tmp), 0)
    expect_equal(length(names(tmp)), 6)
})