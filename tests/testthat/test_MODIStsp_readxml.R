message("Check proper functioning of MODIStsp_readxml")


test_that("MODIStsp_readxml works as expected", {

  tmpfile <- tempfile(fileext = ".RData")
  # read the xml file of products options
  MODIStsp_read_xml(tmpfile, system.file("ExtData", "MODIStsp_ProdOpts.xml.zip",
                                        package = "MODIStsp"))
  # load the saved opts file
  opts <- get(load(tmpfile))

  # check that reading from the file retrieves a list with MODIS products
  # characteristics
  testthat::expect_is(opts, "list")
  testthat::expect_equal(names(opts)[[1]], "Surf_Ref_8Days_500m (M*D09A1)")
  testthat::expect_equal(opts[[1]][["061"]][["file_prefix"]][["Terra"]],
                         "MOD09A1")
  testthat::expect_equal(opts[[101]][["061"]][["file_prefix"]][["Aqua"]],
                         "MYD13C1")

})
