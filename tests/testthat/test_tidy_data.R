# tests for tidy_data

dir <- paste0(system.file(package = "edwr", "extdata"))
dir.sample <- "../../data-raw/sample"

library(edwr)
context("Check tidy_data")

test_that("returns proper class", {
    expect_s3_class(tidy_data(labs), "labs")

})

test_that("returns default method warning", {
    tmp <- read_data(dir, "demographics.csv", "demographics")
    expect_warning(tidy_data(tmp), "No tidy_data method available for class")
})

test_that("location tidying", {
    x <- readRDS("locations.Rds")
    tmp <- tidy_data(x)
    expect_gt(nrow(tmp), 0)
    expect_equal(length(names(tmp)), 6)
})

test_that("medical services tidying", {
    x <- readRDS("services.Rds")
    tmp <- tidy_data(x)
    expect_gt(nrow(tmp), 0)
    expect_equal(length(names(tmp)), 5)
})

test_that("vent times tidying", {
    visits <- readRDS("visits.Rds")
    x <- readRDS("vent_times.Rds")
    tmp <- tidy_data(x, dc = visits)
    expect_gt(nrow(tmp), 0)
    expect_equal(length(names(tmp)), 4)
})