context("create_help_messages")


test_that("create_help_messages works as expected", {
        mess <- create_help_messages()
        testthat::expect_is(mess, "data.frame")
})
