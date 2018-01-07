context("Check proper functioning of the main MODIStsp GUI")
testthat::test_that(
  "Tests on MODIStsp_GUI", {
    skip_on_cran()
    skip_on_travis()
    ### Test mainGUI: work a bit on the GUI to guarante it's not crashing. ###
    ### Quit when finished so that processing is not started (well behaviour of
    ### processing checked in other tests !)
    if (interactive()) {
      MODIStsp()
    }
  }
)
