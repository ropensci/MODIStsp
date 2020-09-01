context("Check proper functioning of MODIStsp_addindex")

test_that("MODIStsp_addindex works as expected", {
  # skip on cran, because the test needs to write on file system.
  # (Note that an authorization to do this is asked to the user).
  #
  skip_on_cran()
  skip_on_travis()
  # error in formula leads to aborting
  expect_error(
    MODIStsp_addindex(
      new_indexbandname = "myindex",
      new_indexformula = "b2_dsadsa",
      new_indexfullname = "Test Index"
    )
  )

  expect_error(
    MODIStsp_addindex(
      new_indexbandname = "myindex",
      new_indexformula = "b1_Red - bg",
      new_indexfullname = "Test Index")
  )

  # Duplicated index name leads to aborting
  expect_error(
    MODIStsp_addindex(
      new_indexbandname = "SR",
      new_indexformula = "b1_Red - b2_NIR",
      new_indexfullname = "Test Index")
  )
  # Duplicated index full name leads to aborting
  expect_error(
    MODIStsp_addindex(new_indexbandname = "SR2",
                      new_indexformula = "b1_Red - b2_NIR",
                      new_indexfullname = "Simple Ratio (NIR/Red)")
  )

  # error in formula leads to aborting
  expect_error(
    MODIStsp_addindex(
      new_indexformula = "b1_Red",
      new_indexfullname = "Test Index")
  )


  # All parameters ok: just a message is given
  expect_message(
    MODIStsp_addindex(
      new_indexbandname = paste0("Index_", as.character(sample(10000, 1))),
      new_indexformula = "b1_Red - b2_NIR",
      new_indexfullname = paste0("Index_", as.character(sample(10000, 1))))
  )
  # check that the index was added
  # remove the index not to clutter the test file
  MODIStsp_resetindexes()
})
