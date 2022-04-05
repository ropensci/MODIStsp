message("MODIStsp_resetindexes")


test_that("MODIStsp_resetindexes works as expected", {
  skip_on_cran()
  skip_on_travis()
  # add a custom index
  expect_message(
    MODIStsp_addindex(
      new_indexbandname = paste0("Index_", as.character(sample(10000, 1))),
      new_indexformula = "b1_Red - b2_NIR",
      new_indexfullname = paste0("Index_", as.character(sample(10000, 1))))
  )
  # remove it and check if custom indexes were removed
  MODIStsp_resetindexes()
  opts <- jsonlite::read_json(system.file("ExtData" , "MODIStsp_indexes.json",
                                package = "MODIStsp"))
  expect_equal(opts, "")

})
