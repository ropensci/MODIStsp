context("check_projection tests")

test_that("check_projection works as expected", {
  # skip_on_travis()

  # valid character
  expect_equal(check_projection("32N"), 32632)

  # WKT can be retrieved from CRS
  expect_equal(
    check_projection(sf::st_crs(32632))$wkt,
    "PROJCS[\"WGS 84 / UTM zone 32N\",\n    GEOGCS[\"WGS 84\",\n        DATUM[\"WGS_1984\",\n            SPHEROID[\"WGS 84\",6378137,298.257223563,\n                AUTHORITY[\"EPSG\",\"7030\"]],\n            AUTHORITY[\"EPSG\",\"6326\"]],\n        PRIMEM[\"Greenwich\",0,\n            AUTHORITY[\"EPSG\",\"8901\"]],\n        UNIT[\"degree\",0.0174532925199433,\n            AUTHORITY[\"EPSG\",\"9122\"]],\n        AUTHORITY[\"EPSG\",\"4326\"]],\n    PROJECTION[\"Transverse_Mercator\"],\n    PARAMETER[\"latitude_of_origin\",0],\n    PARAMETER[\"central_meridian\",9],\n    PARAMETER[\"scale_factor\",0.9996],\n    PARAMETER[\"false_easting\",500000],\n    PARAMETER[\"false_northing\",0],\n    UNIT[\"metre\",1,\n        AUTHORITY[\"EPSG\",\"9001\"]],\n    AXIS[\"Easting\",EAST],\n    AXIS[\"Northing\",NORTH],\n    AUTHORITY[\"EPSG\",\"32632\"]]") #nolint

  # valid numeric or epsg string

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
