context("MODIStsp_Processing")
testthat::test_that(
  "Tests on MODIStsp", {
    
    library(testthat)
    # skip_on_cran()
    skip_on_travis()
    
    MODIStsp()
    
    print("test-http")
    MODIStsp(gui = FALSE, options_file = system.file("ExtData/test_http.json", package = "MODIStsp")) 
    
    # What is this test ? 
    print("test1")
    MODIStsp(test = 1)   
    
    # What is this test ? 
    print("test2")
    MODIStsp(test = 2)
    
    # What is this test ? 
    print("test3")
    expect_warning(MODIStsp(test = 3))
    
    # What is this test ? 
    # MODIStsp(test = 4)
    
    # What is this test ? 
    print("test5")
    expect_warning(MODIStsp(test = 5))
    
    # for (n in c(1:3,5)) { # test 4 (http) can not be runned in non-interactive mode
    #   cat("\nTest ",n,"...\n", sep = "")
    #   MODIStsp(test = n)
    # }
  }
)
