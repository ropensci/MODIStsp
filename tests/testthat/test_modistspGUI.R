context("MODIStsp_GUI")
testthat::test_that(
  "Tests on MODIStsp_GUI", {

    library(testthat)
    skip_on_cran()
    skip_on_travis()

    ### Test 1: worka bit on the GUI to guarante it's not crashing. ###
    ### Quit when finished so that processing is not started (well behabiour of
    ### processing checked in other tests !)
    MODIStsp()
    }
)
