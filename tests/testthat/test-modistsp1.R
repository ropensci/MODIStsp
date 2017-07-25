context("MODIStsp_Processing")
testthat::test_that("Test On MODIStsp", {
  # skip_on_cran()
  skip_on_travis()
  MODIStsp(gui = FALSE, options_file = "/home/lb/Temp/buttami/test_modistsp/test_MOD13A2_http_full.json")
  MODIStsp(gui = FALSE, options_file = "/home/lb/Temp/buttami/test_modistsp/test_MOD13A2_ftp_full.json")
  MODIStsp(gui = FALSE, options_file = "/home/lb/Temp/buttami/test_modistsp/test_MOD13A2_http_resized.json")
  }
)
