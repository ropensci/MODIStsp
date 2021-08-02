### Test 8: Fail gracefully on no connection               ####
#   If internet connection is down, retry n_retries times. After n_retries,
#   exit gracefully returning NULL with a message.
context("MODIStsp Test 8: Exit gracefully on missing internet connection")
test_that(
  "Tests on MODIStsp", {
    skip_on_cran()
    expect_message(
      httptest::without_internet(MODIStsp(test = 5, n_retries = 1)),
      "Error: http server seems to be down!"
    )
  }
)
