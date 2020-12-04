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
test_that("Tests on MODIStsp", {
  #
  # skip("Skip tests - since they rely on download they are only run locally")
  skip_on_cran()
  # skip_on_travis()

  MODIStsp(test = 4)
  out_files_dat <- list.files(
    file.path(tempdir(), "MODIStsp/Snow_cov_mnt_005dg_v6"),
    pattern = "[0-9]\\.dat$", recursive = TRUE, full.names = TRUE)
  out_files_hdr <- list.files(
    file.path(tempdir(), "MODIStsp/Snow_cov_mnt_005dg_v6"),
    pattern = "[0-9]\\.hdr$", recursive = TRUE, full.names = TRUE)
  file_sizes_dat <- file.info(out_files_dat)$size
  # # checking only the .dat because depending on file system the
  # # ENVI hdr files may have slightly different file sizes !
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
      tab[which(as.numeric(names(tab)) > 100)]
    }
  )
  expect_equal(unlist(lapply(na_values,names)), character(0))

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
  expect_true(grepl(
    "+proj=stere +lat_0=90 +lat_ts=71 +lon_0=0",
    sf::st_crs(r)$input,
    fixed = TRUE
  ))
  unlink(out_files_dat)
  unlink(out_files_hdr)
  unlink(out_files_rts)
  #
  #   # test proper behaviour if only Aqua selected
  MODIStsp(test = "04a")
  context("MODIStsp Test 4: Proper functioning when only AQUA is selected")
  out_files_rts <- list.files(
    file.path(
      tempdir(),
      "MODIStsp/Snow_cov_mnt_005dg_v6/Time_Series/RData/Aqua/SN_COV_MNT"),
    pattern = "\\.RData$", recursive = TRUE, full.names = TRUE)

  #check that rts files are properly created
  expect_equal(length(out_files_rts), 1)
  unlink(out_files_rts)

  out_files_dat <- list.files(
    file.path(tempdir(), "MODIStsp/Snow_cov_mnt_005dg_v6"),
    pattern = "[0-9]\\.dat$", recursive = TRUE, full.names = TRUE)
  out_files_hdr <- list.files(
    file.path(tempdir(), "MODIStsp/Snow_cov_mnt_005dg_v6"),
    pattern = "[0-9]\\.hdr$", recursive = TRUE, full.names = TRUE)
  unlink(out_files_hdr)
  unlink(out_files_dat)

  MODIStsp(test = "04c")

  context("MODIStsp Test 4: Reassign multiple nodata on notiled processing")
  out_files_tif <- list.files(
    file.path(
      tempdir(),
      "MODIStsp/Snow_cov_mnt_005dg_v6/SN_COV_MNT/"),
    pattern = "\\.tif$", recursive = TRUE, full.names = TRUE)
  r <- sf::gdal_utils("info", out_files_tif[1], quiet = TRUE)
  expect_equal(substring(strsplit(r, "NoData Value=")[[1]][2], 1, 3),
               "255")
  unlink(out_files_tif)

  out_files_rts <- list.files(
    file.path(tempdir(), "MODIStsp/Snow_cov_mnt_005dg_v6"),
    pattern = "\\.RData$", recursive = TRUE, full.names = TRUE)
  unlink(out_files_rts)
})
