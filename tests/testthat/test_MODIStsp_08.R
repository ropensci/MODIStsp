### Test 8: Fail gracefully on no connection               ####
#   If internet connection is down, retry n_retries times. After n_retries,
#   abort gracefully.
context("MODIStsp Test 8: Fail gracefully on missing internet connection")
test_that(
  "Tests on MODIStsp", {
    skip_on_cran()
    expect_error(httptest::without_internet(MODIStsp(test = 5, n_retries = 1)),
                 "Error: http server seems to be down!")
  }
)
