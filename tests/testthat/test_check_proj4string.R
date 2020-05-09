context("check_projection tests")

test_that("check_projection works as expected", {
  skip_on_travis()
  skip_on_cran()

  # valid character
  expect_equal(check_projection("32N"), 32632)

  # WKT can be retrieved from CRS
  expect_equal(
    check_projection(sf::st_crs(32632))$input,
    "EPSG:32632") #nolint

  # valid numeric or epsg string

  expect_true(
    check_projection(3857) %in% c(
      3857))

  expect_true(
    check_projection("3857") %in% c(
      3857))

  expect_equal(
    check_projection(sf::st_as_text(sf::st_crs(32632))),
    sf::st_as_text(sf::st_crs(32632))) #nolint

  # invalid inputs
  expect_error(expect_warning(check_projection("+init=epsg:montemario",
                                               abort = TRUE)))
  expect_error(expect_warning(check_projection("123554644", abort = TRUE)))
  expect_error(expect_warning(check_projection(123554644, abort = TRUE)))
  expect_error(expect_warning(check_projection("+init=epsg:montemario",
                                               abort = TRUE)))


})
