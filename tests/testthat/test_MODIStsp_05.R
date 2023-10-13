### Test 5: test of HTTP download (from USGS).  ####
#   This test downloads an albedo product (MCD43A3), clipping (Cape Verde)
#   and reprojecting it (Cape Verde National CRS).
#   Output files and time series are in ENVI format.
#   NOTE! the test requires to enter USGS credentials, which are asked in
#   interactive mode. For this reason the test is excluded when running
#   `R CMD check` and must be run manually or using `devtools::test()`


message("MODIStsp Test 5: HTTP download from USGS, resize and reproject")
test_that(
  "Tests on MODIStsp", {
    skip_on_cran()
    skip_on_travis()
    skip_if(!"HDF4" %in% sf::st_drivers("raster")$name)
    
    MODIStsp(test = "05")
    out_files_tif <- list.files(
      file.path(tempdir(), "MODIStsp/Albedo_Daily_500m_v61"),
      pattern = "\\.tif$", recursive = TRUE, full.names = TRUE)
    file_sizes_tif <- file.info(out_files_tif)$size
    expect_equal(file_sizes_tif, c(8566))
    means <- unlist(
      lapply(out_files_tif,
             FUN = function(x) {
               mean(raster::values(raster::raster(x)), na.rm = T)
             })
    )
    expect_equal(means, c(0.809), tolerance = 0.01, scale = 1)
    unlink(out_files_tif)
  })
