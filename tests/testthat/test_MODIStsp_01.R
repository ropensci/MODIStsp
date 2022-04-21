message("MODIStsp Test 1: Basic processing on bands and quality
        indicators")
test_that(
  "Tests on MODIStsp", {

    # skip("Skip tests - since they rely on download they are only run locally")
    skip_on_cran()
    # skip_on_travis()
    skip_if(!"HDF4" %in% sf::st_drivers("raster")$name)
    
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

    # momentarly disable - TODO reenable when TRAVIS gets to GDAL 2.3
    # expect_equal(file_sizes, c(80662, 80662, 40908, 40908))

    # check that median value of files file resulting from test run are
    # equal to those obtained with a "working" MODIStsp version

    means <- unlist(
      lapply(out_files,
             FUN = function(x) {
               mean(raster::getValues(suppressWarnings(raster::raster(x))), na.rm = T)
             })
    )
    expect_equal(means, c(13341.450786, 13266.374624, 2.843336, 2.824311),
                 tolerance = 0.001, scale = 1)

    # NodataValue not changed
    r <- sf::gdal_utils("info",out_files[1], quiet = TRUE)
    r <- unlist(strsplit(r, "\n"))
    r <- r[grep("NoData", r)]
    r <- as.numeric(strsplit(r, "NoData Value=")[[1]][2])

    expect_equal(r, 0)

    unlink(out_files)

    ### Test 1: Nodata values are properly changed on full tiles           ####
    message("Nodata values are properly changed on full tiles ")
    MODIStsp(test = "01a")
    r <- sf::gdal_utils("info", out_files[1], quiet = TRUE)
    expect_equal(substring(strsplit(r, "NoData Value=")[[1]][2], 1, 5),
                 "65535")
  })
