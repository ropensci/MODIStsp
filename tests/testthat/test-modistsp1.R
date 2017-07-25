context("MODIStsp_Processing")
testthat::test_that("Test On MODIStsp", {
  # skip_on_cran()
  skip_on_travis()
  expect_warning(MODIStsp(gui = FALSE, options_file = "D:/Temp/buttami/test_modis/test_MOD13A2.json"))
  expect_warning(MODIStsp(gui = FALSE, options_file = "D:/Temp/buttami/test_modis/test_MOD13A2_ftp.json"))
  expect_warning(MODIStsp(gui = FALSE, options_file = "D:/Temp/buttami/test_modis/test_MOD13A2_resized.json"))
  }
)
