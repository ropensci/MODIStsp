context("Check proper functioning of MODIStsp_addindex")

test_that("MODIStsp_addindex works as expected", {
  # error in formula leads to aborting
  expect_error(
    MODIStsp_addindex(gui = FALSE, new_indexbandname = "myindex",
                      new_indexformula = "b2_dsadsa",
                      new_indexfullname = "Test Index")
  )
  
  expect_error(
    MODIStsp_addindex(gui = FALSE, new_indexbandname = "myindex",
                      new_indexformula = "b1_Red - bg",
                      new_indexfullname = "Test Index")
  )
  
  # Duplicated index name leads to aborting
  expect_error(
    MODIStsp_addindex(gui = FALSE, new_indexbandname = "SR",
                      new_indexformula = "b1_Red - b2_NIR",
                      new_indexfullname = "Test Index")
  )
  # Duplicated index full name leads to aborting
  expect_error(
    MODIStsp_addindex(gui = FALSE, new_indexbandname = "SR2",
                      new_indexformula = "b1_Red - b2_NIR",
                      new_indexfullname = "Simple Ratio (b2_NIR/b1_Red)")
  )

  # error in formula leads to aborting
  expect_error(
    MODIStsp_addindex(gui = FALSE, 
                      new_indexformula = "b1_Red",
                      new_indexfullname = "Test Index")
  )
  
  
  # All parameters ok: just a message is given
  expect_message(
    MODIStsp_addindex(gui = FALSE, new_indexbandname = sample(10000, 1),
                      new_indexformula = "b1_Red - b2_NIR",
                      new_indexfullname = sample(10000, 1)),
    option_jsfile = tempfile(fileext = ".json")
  )
})
