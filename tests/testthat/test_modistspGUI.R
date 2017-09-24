context("Check proper functioning of the main MODIStsp GUI")
testthat::test_that(
  "Tests on MODIStsp_GUI", {
    
    ### Test mainGUI: work a bit on the GUI to guarante it's not crashing. ###
    ### Quit when finished so that processing is not started (well behabiour
    ### processing checked in other tests !)
    if (interactive()) {
      MODIStsp()
    }
  }
)
