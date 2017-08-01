context("MODIStsp_Processing")
testthat::test_that(
  "Tests on MODIStsp", {
    
    library(testthat)
    # skip_on_cran()
    skip_on_travis()
    
    # MODIStsp()
    # 
    # print("test-http")
    # MODIStsp(gui = FALSE, options_file = system.file("ExtData/test_http.json", package = "MODIStsp")) 
    
    ### Test 1: test of the basic operations of MODIStsp. ###
    #   The test downloads two bands and extract one quality indicator from a single locale LST product
    #   (MOD11A2), without any additional preprocessing operation. Output files are in GeoTiff format.
    cat("Test 1: basic extraction of bands and quality indicators\n")
    MODIStsp(test = 1)
    
    ### Test 2: test of geometric routines. ###
    #   The test works on the same local product of test 1, performing geometric operations (clipping,
    #   resampling resolution to 1000m. Output files are in ENVI format.
    cat("Test 2: geometric operations\n")
    MODIStsp(test = 2)
    
    ### Test 3: test of the creation of spectral indices and time series. ###
    #   The test work on a clipped SR local product (MOD09A1), acreating two defined spectral indices
    #   (NDVI and SAVI) and one custom index (GVMI). Geometric operations are performed like in
    #   test 2 (with a resampling to 250m resolution using mode), and processing options for time 
    #   series creation are applied.
    #   Output files are in GeoTiff compressed format, with vrt time series.
    cat("Test 3: spectral indices and creation of time series\n")
    expect_warning(MODIStsp(test = 3))
    
    ### Test 4: test of HTTP download (from NSIDC) with seasonal period. ###
    #   This test downloads two snow cover products (MOD10CM) of 1st August of years 2015 and 2016,
    #   applying geometric operations (clipping and reprojecting).
    #   Output files and time series are in ENVI format.
    #   FIXME the test requires valid USGS credentials, which are asked in 
    #   interactive mode. For this reason the test is excluded from testthat routines,
    #   and must be runned manually.
    cat("Test 4: HTTP download from NSIDC and seasonal download\n")
    cat("(skipped)\n")
    # MODIStsp(test = 4)
    
    ### Test 5: test of HTTP download (from USGS). ###
    #   This test downloads an albedo product (MCD43A3), clipping and reprojecting it.
    #   Output files and time series are in ENVI format.
    #   FIXME the test requires valid USGS credentials, which are asked in 
    #   interactive mode. For this reason the test is excluded from testthat routines,
    #   and must be runned manually.
    cat("Test 5: HTTP download from NSIDC and seasonal download\n")
    cat("(skipped)\n")
    # MODIStsp(test = 5)

    ### Test 6: test of FTP download and union of MODIS tiles ###
    #   This test download X LAI products (MCD15A2H) from FTP and merge them in a clipped
    #   overlapping area. 
    #   After reprojection in geographic coordinates, output files are exported as GeoTiff
    #   (scaling output values) and vrt time series are created.
    cat("Test 6: FTP download and union of MODIS tiles\n")
    expect_warning(MODIStsp(test = 6))
    
    
  }
)
