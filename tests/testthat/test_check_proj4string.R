context("check_projection tests")

testthat::test_that("check_projection works as expected", {
  # skip_on_travis()

  # valid character
  expect_equal(
    check_projection("32N"),
    32632) #nolint

  # expect_equal(
  #   check_proj4string(sp::CRS("+init=epsg:32632")),
  #   "+init=epsg:32632 +proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") #nolint

  # temporary - until  WKT can be retrieved from CRS
  expect_equal(expect_warning(
    check_projection(sp::CRS("+init=epsg:32632"))),
    NA)

  # valid numeric

  # expect_true(
  #   check_projection(3857) %in% c(
  #     "+init=epsg:3857 +proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs", #nolint
  #     "+init=epsg:3857 +proj=merc +a=6378137 +b=6378137 +lat_ts=0 +lon_0=0 +x_0=0 +y_0=0 +k=1 +units=m +nadgrids=@null +no_defs") #nolint
  #   )
  #
  expect_true(
    check_projection(3857) %in% c(
      3857))

  expect_true(
    check_projection("3857") %in% c(
      3857))

  expect_equal(
    check_projection(sf::st_as_text(sf::st_crs("+init=epsg:32632"))),
    sf::st_as_text(sf::st_crs("+init=epsg:32632"))) #nolint

  # invalid inputs
  expect_error(expect_warning(check_projection("+init=epsg:montemario",
                                               abort = TRUE)))
  expect_error(expect_warning(check_projection("123554644", abort = TRUE)))
  expect_error(expect_warning(check_projection(123554644, abort = TRUE)))
  expect_error(expect_warning(check_projection("+init=epsg:montemario",
                                               abort = TRUE)))


})
