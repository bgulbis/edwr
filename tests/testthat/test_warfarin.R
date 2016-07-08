# tests for transforming warfarin details data

dir.sample <- "../../data-raw/sample"

library(edwr)
context("Check warfarin details data")

warfarin <- read_data(dir.sample, "warfarin")

test_that("make INR ranges", {
    tmp <- make_inr_ranges(warfarin)
    expect_gt(nrow(tmp), 0)
    expect_equal(length(names(tmp)), 4)
})

test_that("make indications", {
    tmp <- make_indications(warfarin)
    expect_gt(nrow(tmp), 0)
    expect_equal(length(names(tmp)), 12)
})

