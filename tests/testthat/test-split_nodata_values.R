message("test-split_nodata_values.R")

test_that("split_nodata_values works as expected", {
  expect_equal(split_nodata_values(c("250:255"))[[1]], 250:255)
  expect_equal(split_nodata_values(c("250,255"))[[1]], c(250,255))
})

test_that("create_nodata_rcl works as expected", {
  expect_equal(
    as.data.frame(create_nodata_rcl(c("250,254:255"),
                                               c("255"))[[1]]),
    data.frame(from = c(250,254), to = c(250,255), becomes = c(255,255)))

  expect_equal(split_nodata_values(c("250,255"))[[1]], c(250,255))
})
