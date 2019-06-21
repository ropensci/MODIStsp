context("Check proper functioning of bbox_from_file")


test_that("bbox_from_file works as expected", {
  skip_on_cran()
  # bbox from a raster file
  r        <- raster::raster(xmn = -10, xmx = 10, ymn = -10, ymx = 10)
  r        <- raster::init(r, "cell")
  proj4string(r) <-
    tmpfile  <- tempfile(fileext = ".tif")
  raster::writeRaster(r, tmpfile)
  rep_bbox <- bbox_from_file(tmpfile,
                             crs_out = "+init=epsg:3035")
  expect_equal(as.numeric(rep_bbox), c(1786548, -3333786,  4321000, -1072611),
               tolerance = 0.000001)

  # bbox fromn a a shapefile
  # try wityh 3035 projection
  rep_bbox <- bbox_from_file(system.file("vectors", "cities.shp",
                                         package = "rgdal"),
                             crs_out = "+init=epsg:3035")
  expect_equal(as.numeric(rep_bbox), c(-8419359, -6888003,  17037146, 15489222),
               tolerance = 0.000001)

  # try with a different projection
  rep_bbox <- bbox_from_file(system.file("vectors", "cities.shp",
                                         package = "rgdal"),
                             crs_out = "+init=epsg:4326")
  expect_equal(as.numeric(rep_bbox), c(-165.2700, -53.1500,  177.1302, 78.2000),
               tolerance = 0.000001)

  # expect error on a non-spatial file name
  in_file  <- system.file("ExtData", "MODIStsp_ProdOpts.xml",
                          package = "MODIStsp")
  expect_error(bbox_from_file(in_file))

})
