context("MODIStsp_resetindexes")


test_that("MODIStsp_resetindexes works as expected", {
  skip_on_cran()
  # add a custom index
  expect_message(
    MODIStsp_addindex(
      opts_jsfile = system.file("testdata/test_addindex.json",
                                  package = "MODIStsp"),
      gui = FALSE,
      new_indexbandname = paste0("Index_", as.character(sample(10000, 1))),
      new_indexformula = "b1_Red - b2_NIR",
      new_indexfullname = paste0("Index_", as.character(sample(10000, 1))))
  )
  # remove it and check if custom indexes were removed
  MODIStsp_resetindexes(
    opts_jsfile = system.file("testdata/test_addindex.json",
                                package = "MODIStsp")
  )
  opts <- load_opts(system.file("testdata/test_addindex.json",
                                package = "MODIStsp"))
  expect_equal(length(opts$custom_indexes), 0)

})
