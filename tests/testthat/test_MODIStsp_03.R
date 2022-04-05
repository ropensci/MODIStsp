### Test 3: test of the creation of spectral indices and time series. ####
#   The test works on a SR local product (MOD09A1) clipped on a small region
#   (Barbellino, Orobie Alps) and computes two standard spectral indices
#   (NDVI and SAVI) and one custom index (GVMI). Geometric operations are
#   performed like in test 2 (with a resampling to 250m resolution using
#   mode), and processing options for time series creation are applied.
#   Output files are in GeoTiff compressed format, with vrt and ENVI
#   virtual time series.
message("MODIStsp Test 3: Computation of spectral indices and
            creation of time series")
test_that(
  "Tests on MODIStsp", {
    skip_on_cran()
    # skip_on_travis()
    try(MODIStsp_addindex("GVMI",
                      "Global Vegetation Moisture Index  (((NIR+0.1)−(SWIR+0.02))/((NIR+0.1)+(SWIR+0.02)))",
                      "((b2_NIR+0.1)-(b7_SWIR+0.02))/((b2_NIR+0.1)+(b7_SWIR+0.02))",
                      "32767"))

    MODIStsp(test = 3)
    out_files_tif <- list.files(
      file.path(tempdir(), "MODIStsp/Surf_Ref_8Days_500m_v61"),
      pattern = "\\.tif$", recursive = TRUE, full.names = TRUE)

    file_sizes_tif <- file.info(out_files_tif)$size
    expect_equal(file_sizes_tif, c(12410, 12000, 741, 11952, 1219),
                 tolerance = 0.01)
    means <- unlist(lapply(out_files_tif,
                           FUN = function(x) {
                             mean(raster::values(raster::raster(x)),
                                  na.rm = T)
                           }))
    expect_equal(means, c(-0.06390684, 0.45772493, 0, 0.27926952, 199.22489316),
                 tolerance = 0.001, scale = 1)

    # nodatavalue properly changed
    r <- sf::gdal_utils("info", out_files_tif[1], quiet = TRUE)
    expect_equal(substring(strsplit(r, "NoData Value=")[[1]][2], 1, 5),
                 "32767")

    out_files_vrt <- list.files(
      file.path(tempdir(), "MODIStsp/Surf_Ref_8Days_500m_v61"),
      pattern = "\\.vrt$", recursive = TRUE, full.names = TRUE)
    file_sizes_vrt <- file.info(out_files_vrt)$size
    expect_equal(length(out_files_vrt), 5)

    vrt_1 <- raster::raster(out_files_vrt[1])
    expect_is(vrt_1, "RasterLayer")
    mean_scaled <- mean(raster::getValues(vrt_1), na.rm = T)
    expect_equal(mean_scaled,  -0.06390684, tolerance = .00001, scale = 1)

    unlink(out_files_tif)

    # same execution with ENVI output and no scaling on indexes
    message("MODIStsp Test 3: Save in ENVI format")
    MODIStsp(test = "03a")
    out_files_dat <- list.files(
      file.path(tempdir(), "MODIStsp/Surf_Ref_8Days_500m_v61"),
      pattern = "\\.dat$", recursive = TRUE, full.names = TRUE)
    file_sizes_dat <- file.info(out_files_dat)$size
    expect_equal(length(out_files_dat), 5)
    expect_equal(file_sizes_dat, c(7488, 7488, 3744, 7488, 7488),
                 tolerance = 0.001, scale = 1)
    dat_1 <- raster::raster(out_files_dat[1])
    mean_noscaled <- mean(raster::getValues(dat_1), na.rm = T)

    # Average index value is the same whether it is comnputed from scaled ####
    # or noscaled  reflectances also when additive factors are present
    message("MODIStsp Test 3: Indexes with additive components are properly
            computed")
    expect_equal((mean_noscaled / 10000), mean_scaled, tolerance = 1e-5)


    unlink(out_files_dat)
  })
