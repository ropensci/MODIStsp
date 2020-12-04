### Test 2: test of geometric routines.                                 ####
#   The test works on the same local product of test 1, performing geometric
#   operations (clipping on the extent of Avalon peninsula and resampling
#   resolution to 1000m). Output files are in ENVI format.

test_that(
  "Tests on MODIStsp", {
    skip_on_cran()
    # skip_on_travis()

    MODIStsp(test = 2)
    context("MODIStsp Test 2: Processing works when changing projection and
            resolution")
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
    context("MODIStsp Test 2: Output projection and resolution are properly
            set")
    r <- raster::raster(out_files_dat[1])
    expect_true(grepl(
      "+proj=utm +zone=22 +datum=WGS84 +units=m +no_defs", #nolint,
      sf::st_crs(r)$input,
      fixed=TRUE
    ))
    expect_equal(raster::res(r), c(1000, 1000))
    unlink(out_files_dat)
  })
