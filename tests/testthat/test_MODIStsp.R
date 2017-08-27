context("MODIStsp_Processing")
testthat::test_that(
  "Tests on MODIStsp", {
    
    library(testthat)
    
    # skip("Skip tests - since they rely on download they are only run locally !")
    skip_on_cran()
    skip_on_travis()
    
    ### Test 1: test of the basic operations of MODIStsp.                   ####
    #   The test downloads two bands and extract one quality indicator from a
    #   single locale LST product (MOD11A2, Newfoundland), without any
    #   additional preprocessing operation. Output files are in GeoTiff format.
    context("Test 1: basic extraction of bands and quality indicators")
    MODIStsp(test = 1)
    file_sizes <- file.info(
      list.files(file.path(tempdir(),"/Surf_Temp_8Days_GridSin_v6"), 
                 recursive = TRUE, full.names = TRUE)
    )$size
    expect_equal(file_sizes, c(80670, 80670, 40916, 40916))
    #MODIStest_check_md5(test = 1)

    ### Test 2: test of geometric routines.                                 ####
    #   The test works on the same local product of test 1, performing geometric
    #   operations (clipping on the extent of Avalon peninsula and resampling
    #   resolution to 1000m. Output files are in ENVI format.
    context("Test 2: geometric operations")
    MODIStsp(test = 2)
    file_sizes_dat <- file.info(list.files(
      file.path(tempdir(),"/Surf_Temp_8Days_GridSin_v6"), 
      pattern = ".dat$", recursive = TRUE, full.names = TRUE)
    )$size
    expect_equal(file_sizes_dat, c(52000, 52000,  26000,  26000))

    # MODIStest_check_md5(test = 2)

    ### Test 3: test of the creation of spectral indices and time series. ####
    #   The test works on a SR local product (MOD09A1) clipped on a small region
    #   (Barbellino, Orobie Alps) and creates two defined spectral indices
    #   (NDVI and SAVI) and one custom index (GVMI). Geometric operations are
    #   performed like in test 2 (with a resampling to 250m resolution using
    #   mode), and processing options for time series creation are applied.
    #   Output files are in GeoTiff compressed format, with vrt time series.
    context("Test 3: spectral indices and creation of time series")
    expect_warning(MODIStsp(test = 3))
    # MODIStest_check_md5(test = 3)
    file_sizes_tif <- file.info(
      list.files(file.path(tempdir(),"/Surf_Ref_8Days_500m_v6"), 
                 pattern = ".tif$", recursive = TRUE, full.names = TRUE)
    )$size
    expect_equal(file_sizes_tif, c(10583, 10642, 752, 10706 , 1409))

    ### Test 4: test of HTTP download (from NSIDC) with seasonal period. ####
    #   This test downloads two snow cover products (MYD10CM) of 1st August of
    #   years 2015 and 2016, applying geometric operations (clipping on Svalbard
    #   islands and reprojecting in Arctic Polar Stereographic projection).
    #   Output files and time series are in ENVI format.
    #   FIXME the test requires valid USGS credentials, which are asked in
    #   interactive mode. For this reason the test is excluded from testthat
    #   routines, and must be run manually.
    context("Test 4: HTTP download from NSIDC and seasonal download")
    cat("(skipped)\n")
    # MODIStsp(test = 4)
    # MODIStest_check_md5(test = 4)
    # file_sizes_dat <- file.info(
    #   list.files(file.path(tempdir(),"/Snow_cov_mnt_005dg_v6"),
    #              pattern = ".dat$", recursive = TRUE, full.names = TRUE)
    # )$size
    # expect_equal(file_sizes_dat, c(91872, 91872, 328))

    ### Test 5: test of HTTP download (from USGS).  ####
    #   This test downloads an albedo product (MCD43A3), clipping (Cape Verde)
    #   and reprojecting it (Cape Verde National CRS).
    #   Output files and time series are in ENVI format.
    #   FIXME the test requires valid USGS credentials, which are asked in
    #   interactive mode. For this reason the test is excluded from testthat
    #   routines, and must be runned manually.
    context("Test 5: HTTP download from NSIDC and seasonal download")
    cat("(skipped)\n")
    # MODIStsp(test = 5)
    # MODIStest_check_md5(test = 5)
    # file_sizes_tif <- file.info(
    #   list.files(file.path(tempdir(),"/Albedo_Daily_500m_v6"),
    #              pattern = "tif$", recursive = TRUE, full.names = TRUE)
    # )$size
    # expect_equal(file_sizes_tif, c(8278, 8741))

    ### Test 6: test of FTP download and union of MODIS tiles               ####
    #   This test download four LAI products (MCD15A2H) from FTP and merge them
    #   in a clipped overlapping area (Minorca island).
    #   After reprojection in geographic coordinates, output files are exported
    #   as GeoTiff (scaling output values) and vrt time series are created.
    context("Test 6: FTP download and union of MODIS tiles")
    MODIStsp(test = 6)
    # MODIStest_check_md5(test = 6)
    file_sizes_dat <- file.info(
      list.files(file.path(tempdir(),"/LAI_8Days_500m_v6"),
                 pattern = ".tif$", recursive = TRUE, full.names = TRUE)
    )$size
    expect_equal(file_sizes_dat, c(1911, 1894, 840, 840))
    
  }
)
