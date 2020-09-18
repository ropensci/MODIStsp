context("Check proper functioning of bbox_from_file")


test_that("bbox_from_file works as expected", {
  skip_on_cran()
  skip_on_travis()
  # bbox from a raster file
  r <- raster::raster(xmn = -10, xmx = 10, ymn = -10, ymx = 10)
  r <- raster::init(r, "cell")

  tmpfile  <- tempfile(fileext = ".tif")
  raster::writeRaster(r, tmpfile)
  rep_bbox <- bbox_from_file(tmpfile,
                             crs_out = 3035)
  testthat::expect_equal(as.numeric(rep_bbox), c(1786548, -3333786,  4321000, -1072611),
               tolerance = 0.000001)

  # bbox fromn a a shapefile
  # try wityh 3035 projection
  rep_bbox <- bbox_from_file(system.file("shape/nc.shp", package="sf"),
                             crs_out = 3035)
  testthat::expect_equal(as.numeric(rep_bbox), c(-1997537 , 5368147,  -1565600,  6332622),
               tolerance = 0.000001)

  # try with a different projection
  rep_bbox <- bbox_from_file(system.file("shape/nc.shp", package="sf"),
                             crs_out = "OGC:CRS84")
  expect_equal(as.numeric(rep_bbox), c( -84.32379, 33.88209, -75.45656, 36.58981),
               tolerance = 0.000001)

  # try passing crs_out as WKT
  rep_bbox <- bbox_from_file(system.file("shape/nc.shp", package="sf"),
                             crs_out = st_as_text(st_crs(3035)))
  expect_equal(as.numeric(rep_bbox), c(-1997537 , 5368147,  -1565600 ,  6332622),
               tolerance = 0.000001)

  rep_bbox <- bbox_from_file(system.file("shape/nc.shp", package="sf"),
                             crs_out = st_crs(3035)$wkt)
  expect_equal(as.numeric(rep_bbox), c(-1997537 , 5368147,  -1565600 ,  6332622),
               tolerance = 0.000001)

  rep_bbox <- bbox_from_file(system.file("shape/nc.shp", package="sf"),
                             crs_out = st_crs(3035)$epsg)
  expect_equal(as.numeric(rep_bbox), c(-1997537 , 5368147,  -1565600 ,  6332622),
               tolerance = 0.000001)

  # expect error on badly specified crs_out
  expect_error(expect_warning(bbox_from_file(system.file("shape/nc.shp", package="sf")),
                              crs_out = st_as_text(st_crs(43265))))

  expect_error(expect_warning(bbox_from_file(system.file("shape/nc.shp", package="sf")),
                              crs_out = "+init=epssg:4326"))

  # expect error on a non-spatial file name
  in_file  <- system.file("ExtData", "MODIStsp_ProdOpts.xml",
                          package = "MODIStsp")
  expect_error(bbox_from_file(in_file))

})
