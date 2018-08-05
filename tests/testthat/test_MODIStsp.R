context("MODIStsp Test 0: Gracefully fail on input problems")
testthat::test_that(
  "Tests on MODIStsp", {
    skip_on_cran()
    # no options file
    expect_error(MODIStsp(gui = FALSE),
                 "Please provide a valid \"options_file\"")
    # wrong path or non-existing options_file
    expect_error(expect_warning(MODIStsp(options_file = "", gui = FALSE),
                                "Processing Options file not found"))

    expect_error(expect_warning(MODIStsp(options_file = "", gui = TRUE),
                                "The specified `.json` options file was not
                                found"))
    # provided options file is not a MODIStsp json options file
    expect_error(MODIStsp(
      options_file = system.file("ExtData", "MODIStsp_ProdOpts.xml",
                                 package = "MODIStsp"),
      gui = FALSE), "Unable to read the provided JSON")

    # Credentials for earthdata login for http download are wrong
    expect_error(MODIStsp(
      options_file = system.file("testdata/test05_wrong_pwd.json",
                                 package = "MODIStsp"),
      gui = FALSE, n_retries = 2), "Username and/or password are not valid")
  })


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
    unlink(out_files)
  })

### Test 2: test of geometric routines.                                 ####
#   The test works on the same local product of test 1, performing geometric
#   operations (clipping on the extent of Avalon peninsula and resampling
#   resolution to 1000m). Output files are in ENVI format.
context("MODIStsp Test 2: Geometric operations")
testthat::test_that(
  "Tests on MODIStsp", {
    skip_on_cran()
    # skip_on_travis()

    MODIStsp(test = 2)
    out_files_dat  <- list.files(
      file.path(tempdir(), "MODIStsp/Surf_Temp_8Days_GridSin_v6"),
      pattern = "\\.dat$", recursive = TRUE, full.names = TRUE)

    # same checks as before on file size and raster stats
    file_sizes_dat <- file.info(out_files_dat)$size
    expect_equal(file_sizes_dat, c(52000, 26000))
    means <- unlist(
      lapply(out_files_dat,
             FUN = function(x) {
               mean(raster::getValues(raster::raster(x)), na.rm = T)
             })
    )
    expect_equal(means, c(13447.650685, 1.757238),
                 tolerance = 0.001, scale = 1)

    # additional checks on output projection and resolution
    r <- raster::raster(out_files_dat[1])
    expect_equal(
      sp::proj4string(r),
      "+proj=utm +zone=22 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" #nolint
    )
    expect_equal(raster::res(r), c(1000, 1000))
    unlink(out_files_dat)
  })

### Test 3: test of the creation of spectral indices and time series. ####
#   The test works on a SR local product (MOD09A1) clipped on a small region
#   (Barbellino, Orobie Alps) and computes two standard spectral indices
#   (NDVI and SAVI) and one custom index (GVMI). Geometric operations are
#   performed like in test 2 (with a resampling to 250m resolution using
#   mode), and processing options for time series creation are applied.
#   Output files are in GeoTiff compressed format, with vrt and ENVI
#   virtual time series.
context("MODIStsp Test 3: Computation of spectral indices and
            creation of time series")
testthat::test_that(
  "Tests on MODIStsp", {
    skip_on_cran()
    # skip_on_travis()

    MODIStsp(test = 3)
    out_files_tif <- list.files(
      file.path(tempdir(), "MODIStsp/Surf_Ref_8Days_500m_v6"),
      pattern = "\\.tif$", recursive = TRUE, full.names = TRUE)

    file_sizes_tif <- file.info(out_files_tif)$size
    expect_equal(file_sizes_tif, c(10583, 10642, 752, 10706, 1409),
                 tolerance = 0.001, scale = 1)
    means <- unlist(lapply(out_files_tif,
                           FUN = function(x) {
                             mean(raster::getValues(raster::raster(x)),
                                  na.rm = T)
                           }))
    expect_equal(means, c(0.5400184, 0.6436071, 0.0000000, 0.3753549,
                          197.0045406), tolerance = 0.001, scale = 1)

    out_files_vrt <- list.files(
      file.path(tempdir(), "MODIStsp/Surf_Ref_8Days_500m_v6"),
      pattern = "\\.vrt$", recursive = TRUE, full.names = TRUE)
    file_sizes_vrt <- file.info(out_files_vrt)$size
    expect_equal(length(out_files_vrt), 5)

    vrt_1 <- raster::raster(out_files_vrt[1])
    expect_is(vrt_1, "RasterLayer")
    mean <- mean(raster::getValues(vrt_1), na.rm = T)
    expect_equal(mean,  0.5400184, tolerance = .00001, scale = 1)
    unlink(out_files_tif)

    # same execution with ENVI output and no scaling on indexes

    MODIStsp(test = 8)
    out_files_dat <- list.files(
      file.path(tempdir(), "MODIStsp/Surf_Ref_8Days_500m_v6"),
      pattern = "\\.dat$", recursive = TRUE, full.names = TRUE)
    file_sizes_dat <- file.info(out_files_dat)$size
    expect_equal(length(out_files_dat), 5)
    expect_equal(file_sizes_dat[1:5], c(7488, 7488, 3744, 7488, 7488),
                 tolerance = 0.001, scale = 1)
    unlink(out_files_dat)
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


context("MODIStsp Test 4: HTTP download from NSIDC (seasonal)")
testthat::test_that("Tests on MODIStsp", {

  # skip("Skip tests - since they rely on download they are only run locally")
  skip_on_cran()
  # skip_on_travis()

  MODIStsp(test = 4)
  out_files_dat <- list.files(
    file.path(tempdir(), "MODIStsp/Snow_cov_mnt_005dg_v6"),
    pattern = "[0-9]\\.dat$", recursive = TRUE, full.names = TRUE)
  file_sizes_dat <- file.info(out_files_dat)$size
  # checking only the .dat because depending on file system the
  # ENVI hdr files may have slightly different file sizes !
  expect_equal(file_sizes_dat, c(91872, 91872, 91872, 91872),
               tolerance = 0.001, scale = 1)
  means <- unlist(
    lapply(out_files_dat,
           FUN = function(x) {
             mean(raster::getValues(raster::raster(x)), na.rm = T)
           })
  )
  expect_equal(means, c(68.31894, 65.17218, 70.06092, 67.14750),
               tolerance = 0.1, scale = 1)
  # check that all nodata_in values were coerced to nodata_out
  na_values <- lapply(
    out_files_dat,
    FUN = function(x) {
      tab <- table(raster::getValues(raster::raster(x)), useNA = "ifany")
      tab[as.numeric(names(tab)) > 100]
    }
  )
  expect_equal(unlist(lapply(na_values,names)), rep(as.character(NA), 4))

  out_files_rts <- list.files(
    file.path(tempdir(), "MODIStsp/Snow_cov_mnt_005dg_v6"),
    pattern = "\\.RData$", recursive = TRUE, full.names = TRUE)

  #check that rts files are properly created
  expect_equal(length(out_files_rts), 3)

  # loading an rdata utput yields a RasterStack
  r <- get(load(out_files_rts[1]))
  expect_is(r, "RasterStack")
  expect_equal(names(r)[1], "MYD10CM_SN_COV_MNT_2015_213")

  # check correct resampling and reprojection
  expect_equal(raster::res(r), c(1553.030, 1551.724),
               tolerance = 0.01, scale = 1)
  expect_equal(
    sp::proj4string(r),
    "+proj=stere +lat_0=90 +lat_ts=71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"#nolint
  )
  unlink(out_files_dat)
  unlink(out_files_rts)

  # test proper behaviour if only Aqua selected
  MODIStsp(
    options_file = system.file("testdata/test04_aqua.json",
                               package = "MODIStsp"),
    gui = FALSE, n_retries = 2, verbose = FALSE)

  out_files_rts <- list.files(
    file.path(tempdir(), "MODIStsp/Snow_cov_mnt_005dg_v6/Time_Series/RData/Aqua/SN_COV_MNT"),
    pattern = "\\.RData$", recursive = TRUE, full.names = TRUE)

  #check that rts files are properly created
  expect_equal(length(out_files_rts), 1)

})

### Test 5: test of HTTP download (from USGS).  ####
#   This test downloads an albedo product (MCD43A3), clipping (Cape Verde)
#   and reprojecting it (Cape Verde National CRS).
#   Output files and time series are in ENVI format.
#   NOTE! the test requires to enter USGS credentials, which are asked in
#   interactive mode. For this reason the test is excluded when running
#   `R CMD check` and must be run manually or using `devtools::test()`


context("MODIStsp Test 5: HTTP download from USGS, resize and reproject")
testthat::test_that(
  "Tests on MODIStsp", {
    skip_on_cran()
    # skip_on_travis()

    MODIStsp(test = 5)
    out_files_tif <- list.files(
      file.path(tempdir(), "MODIStsp/Albedo_Daily_500m_v6"),
      pattern = "\\.tif$", recursive = TRUE, full.names = TRUE)
    file_sizes_tif <- file.info(out_files_tif)$size
    expect_equal(file_sizes_tif, c(8196))
    means <- unlist(
      lapply(out_files_tif,
             FUN = function(x) {
               mean(raster::getValues(raster::raster(x)), na.rm = T)
             })
    )
    expect_equal(means, c(0.8962911), tolerance = 0.001, scale = 1)
    unlink(out_files_tif)
  })

#
### Test 6: test of http download and mosaicing of MODIS tiles               ####
#   This test downloads four MCD LAI products (MCD15A2H) from http and mosaics
#   them and crop to the output extent (Minorca island).
#   After reprojection in geographic coordinates, output files are exported
#   as GeoTiff (scaling output values) and vrt time series are created.

context("MODIStsp Test 6: http download and mosaicing of MODIS tiles")
testthat::test_that(
  "Tests on MODIStsp", {
    skip_on_cran()
    # skip_on_travis()

    MODIStsp(test = 6)
    out_files_dat <- list.files(
      file.path(tempdir(), "MODIStsp/LAI_8Days_500m_v6"),
      pattern = "\\.tif$", recursive = TRUE, full.names = TRUE)
    file_sizes_dat <- file.info(out_files_dat)$size
    expect_equal(file_sizes_dat, c(1633, 1611))
    means <- unlist(
      lapply(out_files_dat,
             FUN = function(x) {
               mean(raster::getValues(raster::raster(x)), na.rm = T)
             })
    )
    expect_equal(means, c(14.9886548, 14.9802337), tolerance = 0.001, scale = 1)
    r <- raster::raster(out_files_dat[1])
    expect_equal(
      sp::proj4string(r),
      "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    )
    expect_equal(raster::res(r), c(0.01, 0.01), tolerance = 0.001, scale = 1)
    # re-run with same parameterization. Since Reprocess = "No", the
    # auto-skipping of already processed dates kicks-in in this case, leading
    # the process to be very quick (Only MODIStso_vrt_create needs to run. )
    MODIStsp(test = 6)
    unlink(file_sizes_dat)
  }
)

### Test 7: Test proper working when passing a spatial file to set the      ####
### extent

context("MODIStsp Test 7: Passing the extent with a spatial file")
testthat::test_that(
  "Tests on MODIStsp", {
    skip_on_cran()
    # skip_on_travis()

    MODIStsp(
      test = 7,
      spatial_file_path = system.file("testdata/spatial_file.shp",
                                      package = "MODIStsp")
    )
    outpath <- file.path(
      tempdir(), "MODIStsp/spatial_file/",
      "/Surf_Temp_8Days_GridSin_v6/LST_Day_6km/MOD11B2_LST_Day_6km_2017_001.tif"
    )
    outrast     <- raster::raster(outpath)
    ext_mstpout <- sp::bbox(outrast)

    ext_spin <- sp::bbox(rgdal::readOGR(
      system.file("testdata/spatial_file.shp", package = "MODIStsp"),
      verbose = FALSE))
    # Is input and output extent equal (allow for difference equal to raster
    # resolution to account for the fact that to include boundaries of the
    # polygon a padding of one pixel is always made)
    expect_equal(as.numeric(ext_mstpout), as.numeric(ext_spin),
                 tolerance = raster::res(outrast), scale = 1)
    unlink(outpath)

  }
)


### Test 8: Fail gracefully on no connection               ####
#   If internet connection is down, retry n_retries times. After n_retries,
#   abort gracefully.
context("MODIStsp Test 8: Fail gracefully on missing connection")
testthat::test_that(
  "Tests on MODIStsp", {
    skip_on_cran()
    expect_error(httptest::without_internet(MODIStsp(test = 5, n_retries = 1)),
                 "Error: http server seems to be down!")
  }
)
