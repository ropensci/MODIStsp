context("Check proper functioning of MODIStsp_addindex")

test_that("MODIStsp_addindex works as expected", {
  # skip on cran, because the test needs to write on file system.
  # (Note that an authorization to do this is asked to the user).
  #
  skip_on_cran()
  # error in formula leads to aborting
  expect_error(
    MODIStsp_addindex(
      opts_jsfile = system.file("testdata/test_addindex.json",
                                  package = "MODIStsp"),
      gui = FALSE, new_indexbandname = "myindex",
      new_indexformula = "b2_dsadsa",
      new_indexfullname = "Test Index"
    )
  )

  expect_error(
    MODIStsp_addindex(
      opts_jsfile = system.file("testdata/test_addindex.json",
                                  package = "MODIStsp"),
      gui = FALSE, new_indexbandname = "myindex",
      new_indexformula = "b1_Red - bg",
      new_indexfullname = "Test Index")
  )

  # Duplicated index name leads to aborting
  expect_error(
    MODIStsp_addindex(
      opts_jsfile = system.file("testdata/test_addindex.json",
                                  package = "MODIStsp"),
      gui = FALSE, new_indexbandname = "SR",
      new_indexformula = "b1_Red - b2_NIR",
      new_indexfullname = "Test Index")
  )
  # Duplicated index full name leads to aborting
  expect_error(
    MODIStsp_addindex(gui = FALSE,
                      opts_jsfile = system.file("testdata/test_addindex.json",
                                                package = "MODIStsp"),
                      new_indexbandname = "SR2",
                      new_indexformula = "b1_Red - b2_NIR",
                      new_indexfullname = "Simple Ratio (NIR/Red)")
  )

  # error in formula leads to aborting
  expect_error(
    MODIStsp_addindex(
      opts_jsfile = system.file("testdata/test_addindex.json",
                                  package = "MODIStsp"),
      gui = FALSE,
      new_indexformula = "b1_Red",
      new_indexfullname = "Test Index")
  )


  # All parameters ok: just a message is given
  expect_message(
    MODIStsp_addindex(
      opts_jsfile = system.file("testdata/test_addindex.json",
                                  package = "MODIStsp"),
      gui = FALSE,
      new_indexbandname = paste0("Index_", as.character(sample(10000, 1))),
      new_indexformula = "b1_Red - b2_NIR",
      new_indexfullname = paste0("Index_", as.character(sample(10000, 1))))
  )
  # check that the index was added
  opts <- load_opts(system.file("testdata/test_addindex.json",
                                package = "MODIStsp"))
  expect_equal(length(opts$custom_indexes), 11)
  # remove the index not to clutter the test file
  MODIStsp_resetindexes(system.file("testdata/test_addindex.json",
                                    package = "MODIStsp"))
})
