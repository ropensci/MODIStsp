context("MODIStsp_extract")


test_that("MODIStsp_extract works as expected", {
  context("MODIStsp Test 1: Basic processing on bands and quality 
        indicators")
  testthat::test_that(
    "Tests on MODIStsp", {
      
      library(testthat)
      
      # skip("Skip tests - since they rely on download they are only run locally")
      # skip_on_cran()
      # skip_on_travis()
      
      ### Test 1: test of the basic operations of MODIStsp.                   ####
      #   The test process two bands and extracts one quality indicator from a
      #   single local hdf file for MOD11A2 product  without any
      #   additional preprocessing operations. Output files are in GeoTiff format.
      
      # build a short times series of MODIS 16-days 1km LAI
      MODIStsp(test = 7)
      stack_file  <- list.files(file.path(
        tempdir(),
        "VI_16Days_1Km_v6/Time_Series/RData/Mixed/NDVI/"),
        recursive = TRUE, full.names = TRUE)
      ts_data <- get(load(stack_file))
      file_sizes <- file.info(out_files)$size
      
      # check that size of files file resulting from test run are equal to those
      #  obtained with a "working" MODIStsp version
      expect_equal(file_sizes, c(80670, 80670, 40916, 40916))
      
      # check that median value of files file resulting from test run are
      # equal to those obtained with a "working" MODIStsp version
      
      means <- unlist(
        lapply(out_files,
               FUN = function(x) {
                 mean(raster::getValues(raster::raster(x)), na.rm = T)
               })
      )
      expect_equal(means, c(13341.450786, 13266.374624, 2.843336, 2.824311))
    })

})
