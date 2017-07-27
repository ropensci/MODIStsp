context("MODIStsp_Processing")
testthat::test_that(
  "Tests on MODIStsp", {
    # skip_on_cran()
    skip_on_travis()
    for (n in c(1:3,5)) { # test 4 (http) can not be runned in non-interactive mode
      cat("\nTest ",n,"...\n", sep="")
      MODIStsp(test=n)
    }
  }
)
