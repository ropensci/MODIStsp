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
      tempjson <- tempfile(fileext = ".json")
      options <- jsonlite::read_json(system.file("Test_files/test_extract.json",
                                    package = "MODIStsp"))
      options$out_folder <- system.file("Test_files/",
                                        package = "MODIStsp")
      jsonlite::write_json(options, tempjson, auto_unbox = TRUE, pretty = TRUE)

      # build and load the MODIStsp stack
      MODIStsp(options_file = tempjson, gui = FALSE)
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
                                                  small = FALSE))
      expect_is(out_data, "xts")
      expect_equal(mean(out_data, na.rm = TRUE), 0.4727554, tolerance = 0.001)
      
      #all data for polys outside extent of raster are NaN
      expect_equal(mean(out_data[, 9], na.rm = TRUE), NaN)

      #chack that results are equal to raster::extract
      expect_warning(out_rastextract <- raster::extract(ts_data,
                                                        polygons, fun = mean,
                                                        na.rm = TRUE, df = TRUE, 
                                                        small = TRUE))
      expect_equal(as.numeric(out_rastextract[2,2:24]),
                   as.numeric(out_data[,2]))

      expect_equal(as.numeric(out_rastextract[6,2:24]),
                   as.numeric(out_data[,6]))

      # check proper working with small == TRUE
      expect_warning(out_data <- MODIStsp_extract(ts_data, polygons,
                                                  id_field = "lc_type",
                                                  small = TRUE,
                                                  small_method = "full"))
      #extracted data for the small polygon equal to raster::extract if
      #small_method = "full"
      expect_equal(as.numeric(out_rastextract[10,2:24]),
                   as.numeric(out_data[,9]))

      # extract data - number of pixels, using a different function
      expect_warning(out_data <- MODIStsp_extract(ts_data, polygons, id_field = "lc_type",
                                   FUN = function(x,...) length(x), 
                                   small = FALSE))
      expect_is(out_data, "xts")
      expect_equal(mean(out_data, na.rm = TRUE), 10.25, tolerance = 0.001)
      expect_equal(as.numeric(out_data[1,]), c(25, 3, 1, 9, 6 , 13, 9, 16, NA ))
      
      #with small == FALSE, one less column in out
      expect_equal(dim(out_data)[2], 9)
      
      
      ### Test 2: test of extraction on polygons                ####
      #   The test uses the same time series as before, and extgract data on
      #   centroids of the polygons

      centroids <- rgeos::gCentroid(polygons, byid = TRUE)
      points <- sp::SpatialPointsDataFrame(centroids,
        polygons@data, match.ID = FALSE)

      expect_warning(out_data <- MODIStsp_extract(ts_data, 
                                                  points,
                                                  id_field = "lc_type"))
      expect_equal(mean(out_data, na.rm = T), 0.4991827, tolerance = 0.001)

      # redo without specifying a column for extraction, and compare wrt
      # raster::extract
      expect_equal(mean(out_data, na.rm = T), 0.4991827, tolerance = 0.001)

      #subset on dates
      expect_warning(out_data <- MODIStsp_extract(ts_data, points, 
                                   start_date = "2016-07-01",
                                   end_date = "2016-08-01", 
                                   out_format = "dframe"))
      expect_equal(length(out_data[,1]), 2)
      expect_equal(mean(out_data[,2], na.rm = T), 0.0858)
      #compare again with raster::extract
      expect_warning(out_rastextract <- raster::extract(ts_data[[13:14]],
                                                        points, fun = mean,
                                                        na.rm = TRUE, 
                                                        df = TRUE))
      expect_equal(as.numeric(out_data[,2]),
                   as.numeric(out_rastextract[1, 2:3]))
      
})
