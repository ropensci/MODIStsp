### Test 7: Test proper working when passing a spatial file to set the      ####
### extent

message("MODIStsp Test 7: Passing the extent with a spatial file")
test_that(
  "Tests on MODIStsp", {
    skip_on_cran()
    skip_on_travis()
    skip_if(!"HDF4" %in% sf::st_drivers("raster")$name)
    
    MODIStsp(
      test = 7,
      spatmeth = "file",
      spafile = system.file("testdata/spatial_file.shp",
                                      package = "MODIStsp")
    )
    outpath <- file.path(
      tempdir(), "MODIStsp/spatial_file/",
      "/Surf_Temp_8Days_GridSin_v6/LST_Day_6km/MOD11B2_LST_Day_6km_2017_001.tif"
    )
    outrast     <- suppressWarnings(raster::raster(outpath))
    ext_mstpout <- sf::st_bbox(outrast)

    ext_spin <-  sf::st_bbox(sf::st_read(
      system.file("testdata/spatial_file.shp", package = "MODIStsp"), quiet = TRUE))
    # Is input and output extent equal (allow for difference equal to raster
    # resolution to account for the fact that to include boundaries of the
    # polygon a padding of one pixel is always made)
    expect_equal(as.numeric(ext_mstpout), as.numeric(ext_spin),
                 tolerance = raster::res(outrast), scale = 1)
    unlink(outpath)

    MODIStsp(test = "07b")
    message("MODIStsp Test 7: Reassign multiple nodata on tiled processing and
            Full Tiles")
    out_files_tif <- list.files(
      file.path(
        tempdir(),
        "MODIStsp/Surf_Temp_8Days_GridSin_v6/LST_Day_6km/"),
      pattern = "\\.tif$", recursive = TRUE, full.names = TRUE)

    r <- sf::gdal_utils("info", out_files_tif[1], quiet = TRUE)
    expect_equal(substring(strsplit(r, "NoData Value=")[[1]][2], 1, 5),
                 "65535")
  }
)
