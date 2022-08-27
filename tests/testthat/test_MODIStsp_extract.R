message("MODIStsp_extract works as expected")

test_that("MODIStsp_extract works as expected", {

      # skip("Skip tests - since they rely on download they are only run
      # locally")
      skip_on_cran()
      # skip_on_travis()

      ### Test 1: test of extraction on polygons                ####
      #   The test uses tif files in testdata/VI_16Days_500m_v6 to build
      #   a MODIStsp rasterStack, end extract data on polygons saved in
      #   testdata/extract_polys.shp

      # copy files in "inst/testdata/VI_16Days_500m_v6" to tempdir to avoid
      # creating files outside tempdir while running the test
      #
      test_zip <-  system.file("testdata/VI_16Days_500m_v6/NDVI.zip",
                                  package = "MODIStsp")
      dir.create(file.path(tempdir(), "MODIStsp/VI_16Days_500m_v6"),
                 showWarnings = FALSE, recursive = TRUE)
      utils::unzip(test_zip, 
                   exdir = file.path(tempdir(), "MODIStsp/VI_16Days_500m_v6"))

      # build and load the MODIStsp stack

      opts_file <- system.file("testdata/test_extract.json",
                               package = "MODIStsp")
      MODIStsp(opts_file = opts_file, gui = FALSE)
      stack_file  <- list.files(
        file.path(tempdir(),
                  "MODIStsp/VI_16Days_500m_v6/Time_Series/RData/Terra/NDVI/"
                  ),
        full.names = TRUE)
      ts_data <- get(load(stack_file))

      # extract data - average values over polygons

      polygons <- sf::st_read(system.file("testdata/extract_polys.shp",
                                             package = "MODIStsp"),
                                 quiet = TRUE)
      expect_warning(out_data <- MODIStsp_extract(ts_data, polygons,
                                                  id_field = "lc_type",
                                                  small = FALSE))
      expect_is(out_data, "xts")
      expect_equal(mean(out_data, na.rm = TRUE), 4431.833, tolerance = 0.001,
                   scale = 1)

      #all data for polys outside extent of raster are NaN
      expect_equal(mean(out_data[, 9], na.rm = TRUE), NaN)

      #check that results are equal to raster::extract
      expect_warning(out_rastextract <- raster::extract(ts_data,
                                                        polygons,
                                                        fun = mean,
                                                        na.rm = TRUE,
                                                        df = TRUE,
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
      expect_warning(out_data <- MODIStsp_extract(ts_data, polygons,
                                                  id_field = "lc_type",
                                   FUN = function(x,...) length(x),
                                   small = FALSE))
      expect_is(out_data, "xts")
      expect_equal(mean(out_data, na.rm = TRUE), 10.25, tolerance = 0.001,
                   scale = 1)
      expect_equal(as.numeric(out_data[1,]), c(25, 3, 1, 9, 6 , 13, 9, 16, NA))

      #with small == FALSE, one less column in out
      expect_equal(dim(out_data)[2], 9)

      ### Test 2: test of extraction on points                ####
      #   The test uses the same time series as before, and extract data on
      #   centroids of the polygons

      points <- expect_warning(sf::st_centroid(polygons))
      expect_warning(out_data <- MODIStsp_extract(ts_data,
                                                  points,
                                                  id_field = "lc_type"))
      expect_equal(mean(out_data, na.rm = T), 4757.042, tolerance = 0.001,
                   scale = 1)

      # redo without specifying a column for extraction, and compare wrt
      # raster::extract

      #subset on dates
      expect_warning(out_data <- MODIStsp_extract(ts_data, points,
                                   start_date = "2016-07-01",
                                   end_date = "2016-08-01",
                                   out_format = "dframe"))
      expect_equal(length(out_data[,1]), 2)
      expect_equal(mean(out_data[,2], na.rm = T), 858, tolerance = 0.001,
                   scale = 1)
      #compare again with raster::extract
      expect_warning(out_rastextract <- raster::extract(ts_data[[13:14]],
                                                        points, fun = mean,
                                                        na.rm = TRUE,
                                                        df = TRUE))
      expect_equal(as.numeric(out_data[,2]),
                   as.numeric(out_rastextract[1, 2:3]))

})
