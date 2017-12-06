context("MODIStsp_extract")


test_that("MODIStsp_extract works as expected", {
      library(testthat)

      # skip("Skip tests - since they rely on download they are only run
      # locally")
      # skip_on_cran()
      # skip_on_travis()

      ### Test 1: test of extraction on polygons                ####
      #   The test uses tif files in Test_files/VI_16Days_500m_v6 to build
      #   a MODIStsp rasterStack, end extract data on polygons saved in
      #   Test_files/extract_polys.shp

      # update the output folder in test_extract.json
      options <- jsonlite::read_json(system.file("Test_files/test_extract.json",
                                    package = "MODIStsp"))
      options$out_folder <- system.file("Test_files/",
                                        package = "MODIStsp")
      options <- jsonlite::write_json(
        options,
        "/home/lb/Source/git/MODIStsp/inst/Test_files/test_extract.json",
        auto_unbox = TRUE, pretty = TRUE)

      # build and load the MODIStsp stack
      MODIStsp(options_file = system.file("Test_files/test_extract.json",
                                          package = "MODIStsp"), gui = FALSE)
      stack_file  <- list.files(
        system.file("Test_files/VI_16Days_500m_v6/Time_Series/RData/Terra/NDVI/",
                                            package = "MODIStsp"), full.names = TRUE)
      ts_data <- get(load(stack_file))

      # extract data - average

      polygons <- rgdal::readOGR(system.file("Test_files/extract_polys.shp",
                                             package = "MODIStsp"),
                                 verbose = FALSE)
      out_data <- MODIStsp_extract(ts_data, polygons, id_field = "lc_type")
      expect_is(out_data, "xts")
      expect_equal(mean(out_data), 0.5239479, tolerance = 0.001)

      # extract data - number of pixels

      out_data <- MODIStsp_extract(ts_data, polygons, id_field = "lc_type",
                                   FUN = function(x,...)length(x))
      expect_is(out_data, "xts")
      expect_equal(mean(out_data), 7.857143, tolerance = 0.001)
      expect_equal(as.numeric(out_data[1,]), c(25,2,1,9,3,6,9))

      polygons <- rgdal::readOGR(system.file("Test_files/extract_polys.shp",
                                             package = "MODIStsp"))

      ### Test 1: test of extraction on polygons                ####
      #   The test uses the same time series as before, and extgract data on
      #   centroids of the polygons

      centroids <- rgeos::gCentroid(polygons, byid = TRUE)
      points <- sp::SpatialPointsDataFrame(centroids,
        polygons@data, match.ID = FALSE)

      out_data <- MODIStsp_extract(ts_data, points, id_field = "lc_type")
      expect_equal(mean(out_data, na.rm = T), 0.5348604, tolerance = 0.001)


})
