context("check_proj4string tests")

testthat::test_that("check_proj4string works as expected", {
  # skip_on_travis()

  # valid character
  expect_equal(
    check_proj4string("32N"),
    "+init=epsg:32632 +proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") #nolint

  expect_equal(
    check_proj4string(sp::CRS("+init=epsg:32632")),
    "+init=epsg:32632 +proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") #nolint

  # valid numeric

  expect_true(
    check_proj4string(3857) %in% c(
      "+init=epsg:3857 +proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs", #nolint
      "+init=epsg:3857 +proj=merc +a=6378137 +b=6378137 +lat_ts=0 +lon_0=0 +x_0=0 +y_0=0 +k=1 +units=m +nadgrids=@null +no_defs") #nolint
    )


  # same using CRS
  expect_equal(
    check_proj4string("+init=epsg:32632"),
    "+init=epsg:32632 +proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") #nolint


  # invalid inputs
  expect_error(check_proj4string("+init=epsg:montemario", abort = TRUE))
  expect_error(check_proj4string("123554644", abort = TRUE))
  expect_error(check_proj4string(123554644, abort = TRUE))
  expect_warning(check_proj4string("+init=epsg:montemario"))


})
