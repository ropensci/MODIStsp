context("MODIStsp Test 0: Gracefully fail on input problems")
test_that(
  "Tests on MODIStsp", {
    skip_on_cran()
    skip_on_travis()

    # wrong path or non-existing opts_file
    expect_error(expect_warning(MODIStsp(opts_file = "", gui = FALSE),
                                "Processing Options file not found"))

    expect_error(expect_warning(MODIStsp(opts_file = "", gui = TRUE),
                                "The specified `.json` options file was not
                                found"))
    # provided options file is not a MODIStsp json options file
    expect_error(MODIStsp(
      opts_file = system.file("ExtData", "MODIStsp_ProdOpts.xml",
                                 package = "MODIStsp"),
      gui = FALSE), "Unable to read the provided options")

    # Credentials for earthdata login for http download are wrong
    expect_error(MODIStsp(
      opts_file = system.file("testdata/test05a.json",
                                 package = "MODIStsp"), over,
      gui = FALSE, n_retries = 2), "Username and/or password are not valid")
  })
