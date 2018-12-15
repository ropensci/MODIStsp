context("MODIStsp Test 1: Basic processing on bands and quality
        indicators")
testthat::test_that(
  "Tests on MODIStsp", {

    # skip("Skip tests - since they rely on download they are only run locally")
    skip_on_cran()
    # skip_on_travis()

    ### Test 1: test of the basic operations of MODIStsp.                   ####
    #   The test process two bands and extracts one quality indicator from a
    #   single local hdf file for MOD11A2 product  without any
    #   additional preprocessing operations. Output files are in GeoTiff format.

    MODIStsp(test = 1)
    out_files  <- list.files(
      file.path(tempdir(), "MODIStsp/Surf_Temp_8Days_GridSin_v6"),
      pattern = "\\.tif$", recursive = TRUE, full.names = TRUE)
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
    expect_equal(means, c(13341.450786, 13266.374624, 2.843336, 2.824311),
                 tolerance = 0.001, scale = 1)

    # NodataValue not changed
    r <- suppressWarnings(rgdal::GDALinfo(out_files[1]))
    expect_equal(attr(r, "df")$NoDataValue, 0)

    unlink(out_files)

    ### Test 1: Nodata values are properly changed on full tiles           ####
    context("Nodata values are properly changed on full tiles ")
    MODIStsp(test = "01a")
    r <- suppressWarnings(rgdal::GDALinfo(out_files[1]))
    expect_equal(attr(r, "df")$NoDataValue, 65535)

  })
