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
        system.file("Test_files/test_extract.json",
                    package = "MODIStsp"),
        auto_unbox = TRUE, pretty = TRUE)

      # build and load the MODIStsp stack
      MODIStsp(options_file = system.file("Test_files/test_extract.json",
                                          package = "MODIStsp"), gui = FALSE)
      stack_file  <- list.files(
        system.file("Test_files/VI_16Days_500m_v6/Time_Series/RData/Terra/NDVI/", #nolint
                                            package = "MODIStsp"),
        full.names = TRUE)
      ts_data <- get(load(stack_file))

      # extract data - average

      polygons <- rgdal::readOGR(system.file("Test_files/extract_polys.shp",
                                             package = "MODIStsp"),
                                 verbose = FALSE)
      expect_warning(out_data <- MODIStsp_extract(ts_data, polygons, 
                                                  id_field = "lc_type", 
                     small = TRUE))
      expect_is(out_data, "xts")
      expect_equal(mean(out_data, na.rm = TRUE), 0.4727554, tolerance = 0.001)
      expect_equal(mean(out_data[, 9], na.rm = TRUE), NaN)
      
      # extract data - number of pixels, using small = TRUE --> one additional
      # column
      out_data <- MODIStsp_extract(ts_data, polygons, id_field = "lc_type",
                                   FUN = function(x,...) length(x), 
                                   small = TRUE)
      expect_is(out_data, "xts")
      expect_equal(mean(out_data, na.rm = TRUE), 11.88889, tolerance = 0.001)
      expect_equal(as.numeric(out_data[1,]), c(25, 25, 3, 1, 9 ,6 , ))
      
      # With small == T we have an additional column
      out_data <- MODIStsp_extract(ts_data, polygons, id_field = "lc_type",
                                   FUN = function(x,...) length(x), 
                                   small = FALSE)
      ### Test 2: test of extraction on polygons                ####
      #   The test uses the same time series as before, and extgract data on
      #   centroids of the polygons

      centroids <- rgeos::gCentroid(polygons, byid = TRUE)
      points <- sp::SpatialPointsDataFrame(centroids,
        polygons@data, match.ID = FALSE)

      out_data <- MODIStsp_extract(ts_data, points, id_field = "lc_type")
      expect_equal(mean(out_data, na.rm = T), 0.5348604, tolerance = 0.001)
      
      #redo without specifying a column for extraction
      out_data <- MODIStsp_extract(ts_data, points)
      expect_equal(mean(out_data, na.rm = T), 0.5348604, tolerance = 0.001)
     
      out_data <- MODIStsp_extract(ts_data, points, start_date = "2016-07-01",
                                   end_date = "2016-08-01")
      expect_equal(length(out_data[,1]), 2)       
      expect_equal(mean(out_data[,1], na.rm = T), 0.0858)
      
      expect_equal(mean(out_data, na.rm = T), 0.5348604, tolerance = 0.001)
      
})
