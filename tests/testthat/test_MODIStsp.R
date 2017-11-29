context("Test 1: basic processing of bands and quality indicators")
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

    MODIStsp(test = 1)
    out_files  <- list.files(file.path(tempdir(),
                                       "Surf_Temp_8Days_GridSin_v6"),
                             recursive = TRUE, full.names = TRUE)
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

### Test 2: test of geometric routines.                                 ####
#   The test works on the same local product of test 1, performing geometric
#   operations (clipping on the extent of Avalon peninsula and resampling
#   resolution to 1000m). Output files are in ENVI format.
context("Test 2: geometric operations")
testthat::test_that(
  "Tests on MODIStsp", {

    MODIStsp(test = 2)
    out_files_dat  <- list.files(file.path(
      tempdir(),"Surf_Temp_8Days_GridSin_v6"),
      pattern = ".dat$", recursive = TRUE, full.names = TRUE)

    # same checks as before on file size and raster stats
    file_sizes_dat <- file.info(out_files_dat)$size
    expect_equal(file_sizes_dat, c(52000, 52000,  26000,  26000))
    means <- unlist(
      lapply(out_files_dat,
             FUN = function(x) {
               mean(raster::getValues(raster::raster(x)), na.rm = T)
             })
    )
    expect_equal(means, c(13515.308303, 13447.650685, 1.709568, 1.757238))

    # additional checks on output projection and resolution
    r <- raster::raster(out_files_dat[1])
    expect_equal(
      sp::proj4string(r),
      "+proj=utm +zone=22 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" #nolint
    )
    expect_equal(raster::res(r), c(1000,1000))
  })

### Test 3: test of the creation of spectral indices and time series. ####
#   The test works on a SR local product (MOD09A1) clipped on a small region
#   (Barbellino, Orobie Alps) and computes two standard spectral indices
#   (NDVI and SAVI) and one custom index (GVMI). Geometric operations are
#   performed like in test 2 (with a resampling to 250m resolution using
#   mode), and processing options for time series creation are applied.
#   Output files are in GeoTiff compressed format, with vrt and ENVI
#   virtual time series.
context("Test 3: Computation of spectral indices and creation of time
            series")
testthat::test_that(
  "Tests on MODIStsp", {

    MODIStsp(test = 3)
    out_files_tif <- list.files(
      file.path(tempdir(), "Surf_Ref_8Days_500m_v6"),
      pattern = ".tif$", recursive = TRUE, full.names = TRUE)

    file_sizes_tif <- file.info(out_files_tif)$size
    expect_equal(file_sizes_tif, c(10583, 10642, 752, 10706, 1409))
    means <- unlist(lapply(out_files_tif,
                           FUN = function(x) {
                             mean(raster::getValues(raster::raster(x)),
                                  na.rm = T)
                           }))
    expect_equal(means, c(0.5400184, 0.6436071, 0.0000000, 0.3753549,
                          197.0045406))

    out_files_vrt <- list.files(
      file.path(tempdir(),"Surf_Ref_8Days_500m_v6"),
      pattern = ".vrt$", recursive = TRUE, full.names = TRUE)
    file_sizes_vrt <- file.info(out_files_vrt)$size
    expect_equal(length(out_files_vrt), 5)

    vrt_1 <- raster::raster(out_files_vrt[1])
    expect_is(vrt_1, "RasterLayer")
    mean <- mean(raster::getValues(vrt_1), na.rm = T)
    expect_equal(mean,  0.5400184, tolerance = .00001)
  })

### Test 4: test of HTTP download (from NSIDC) with seasonal period. ####
#   This test downloads two snow cover products (MYD10CM) of 1st August of
#   years 2015 and 2016, applying geometric operations (clipping on Svalbard
#   islands and reprojecting in Arctic Polar Stereographic projection).
#   Output files and time series are in ENVI format.
#   The test downloads and processes both TERRA and AQUA datasets and
#   creates vrt time series and rts files.
#   NOTE! the test requires to enter USGS credentials, which are asked in
#   interactive mode. For this reason the test is excluded when running
#   `R CMD check` and must be run manually or using `devtools::test()`

# if (interactive()) {

  context("Test 4: HTTP download from NSIDC and seasonal download")
  testthat::test_that(
    "Tests on MODIStsp", {
      MODIStsp(test = 4)
      out_files_dat <- list.files(
        file.path(tempdir(),"Snow_cov_mnt_005dg_v6"),
        pattern = "[0-9].dat$", recursive = TRUE, full.names = TRUE)
      file_sizes_dat <- file.info(out_files_dat)$size
      # checking only the .dat because depending on file system the
      # ENVI hdr files may have slightly different file sizes !
      expect_equal(file_sizes_dat, c(91872, 91872, 91872, 91872))
      means <- unlist(
        lapply(out_files_dat,
               FUN = function(x) {
                 mean(raster::getValues(raster::raster(x)), na.rm = T)
               })
      )
      expect_equal(means, c(203.7691, 202.8263, 206.5492, 205.2043),
                   tolerance = 0.000001)

      out_files_rts <- list.files(
        file.path(tempdir(),"Surf_Ref_8Days_500m_v6"),
        pattern = ".RData$", recursive = TRUE, full.names = TRUE)

      #check that rts files are properly created
      expect_equal(length(out_files_rts), 5)

      # loading an rdata utput yields a RasterStack
      r <- get(load(out_files_rts[1]))
      expect_is(r, "RasterStack")
      expect_equal(names(r), "MOD09A1_GVMI_2017_193")

      # check correct resampling and reprojection
      expect_equal(raster::res(r), c(250,250))
      expect_equal(
        sp::proj4string(r),
        "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"#nolint
      )
    })
# } else {
#   message("(skipped)\n")
# }

### Test 5: test of HTTP download (from USGS).  ####
#   This test downloads an albedo product (MCD43A3), clipping (Cape Verde)
#   and reprojecting it (Cape Verde National CRS).
#   Output files and time series are in ENVI format.
#   NOTE! the test requires to enter USGS credentials, which are asked in
#   interactive mode. For this reason the test is excluded when running
#   `R CMD check` and must be run manually or using `devtools::test()`

# if (interactive()) {
  context("Test 5: HTTP download from USGS, resize and reproject")
  testthat::test_that(
    "Tests on MODIStsp", {
      MODIStsp(test = 5)
      out_files_tif <- list.files(file.path(tempdir(),"Albedo_Daily_500m_v6"),
                                  pattern = "tif$", recursive = TRUE,
                                  full.names = TRUE)
      file_sizes_tif <- file.info(out_files_tif)$size
      expect_equal(file_sizes_tif, c(7534, 8196))
      means <- unlist(
        lapply(out_files_tif,
               FUN = function(x) {
                 mean(raster::getValues(raster::raster(x)), na.rm = T)
               })
      )
      expect_equal(means, c(0.9504958, 0.8962911))
    })
# } else {
#   message("(skipped)\n")
# }

### Test 6: test of FTP download and union of MODIS tiles               ####
#   This test downloads four MCD LAI products (MCD15A2H) from FTP and mosaic
#   them and crop to the ouput extent (Minorca island).
#   After reprojection in geographic coordinates, output files are exported
#   as GeoTiff (scaling output values) and vrt time series are created.

context("Test 6: FTP download and union of MODIS tiles")
testthat::test_that(
  "Tests on MODIStsp", {
    expect_warning(MODIStsp(test = 6))
    out_files_dat <- list.files(file.path(tempdir(),"LAI_8Days_500m_v6"),
                                pattern = ".tif$", recursive = TRUE,
                                full.names = TRUE)
    file_sizes_dat <- file.info(out_files_dat)$size
    expect_equal(file_sizes_dat, c(1911, 1894, 840, 840))
    means <- unlist(
      lapply(out_files_dat,
             FUN = function(x) {
               mean(raster::getValues(raster::raster(x)), na.rm = T)
             })
    )
    expect_equal(means, c(14.9886548, 14.9802337, 0.5719298, 0.5719298))
    r <- raster::raster(out_files_dat[1])
    expect_equal(
      sp::proj4string(r),
      "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    )
    expect_equal(raster::res(r), c(0.01, 0.01))
  }
)
