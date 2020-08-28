test_that("get_yeardates works as expected", {
  context("get_yeardates works as expected when download_range == \"Full\"")
  # download range == "full"

  # year within start and end year: all year processed
  expect_equal(
    get_yeardates("Full",
                  yy = 2010,
                  start_year = 2005, end_year = 2012,
                  start_date = "2005.02.12", end_date = "2012.02.12"),
    c("2010.1.1", "2010.12.31")
  )
  # year equal to  end year: only up to end_date
  expect_equal(
    get_yeardates("Full",
                  yy = 2012,
                  start_year = 2005, end_year = 2012,
                  start_date = "2005.02.12", end_date = "2012.02.12"),
    c("2012.1.1", "2012.02.12")
  )

  # year equal to  start year: only from start_date
  expect_equal(
    get_yeardates("Full",
                  yy = 2005,
                  start_year = 2005, end_year = 2012,
                  start_date = "2005.02.12", end_date = "2012.02.12"),
    c("2005.02.12", "2005.12.31")
  )

  # year within start and end year: all year processed
  expect_equal(
    get_yeardates("Full",
                  yy = 2012,
                  start_year = 2012, end_year = 2012,
                  start_date = "2012.01.12", end_date = "2012.02.12"),
    c("2012.01.12", "2012.02.12")
  )

})

test_that("get_yeardates works as expected", {
  context("get_yeardates works as expected when download_range == \"Seasonal\"")

  # If the period spans nye:

  # year within start and end year: the dates range is made by 4 entries, two
  # for the dates at the beginning and two for the dates at the end of the year
  expect_equal(
    get_yeardates("Seasonal",
                  yy = 2010,
                  start_year = 2004, end_year = 2012,
                  start_date = "2004.11.12", end_date = "2012.02.12"),
    c("2010.1.1", "2010.02.12", "2010.11.12", "2010.12.31")
  )

  # year equal to  end year: only from start of year to end date
  expect_equal(
    get_yeardates("Seasonal",
                  yy = 2012,
                  start_year = 2004, end_year = 2012,
                  start_date = "2004.11.12", end_date = "2012.02.12"),
    c("2012.1.1", "2012.02.12")
  )

  # year equal to  start year: only from start_date to end of year
  expect_equal(
    get_yeardates("Seasonal",
                  yy = 2004,
                  start_year = 2004, end_year = 2012,
                  start_date = "2004.11.12", end_date = "2012.02.12"),
    c("2004.11.12", "2004.12.31")
  )

  # If the period does not include nye: from start to end date in all years
  expect_equal(
    get_yeardates("Seasonal",
                  yy = 2010,
                  start_year = 2004, end_year = 2012,
                  start_date = "2004.08.01", end_date = "2012.10.01"),
    c("2010.08.01", "2010.10.01")
  )

  # If the period does not include nye:
  expect_equal(
    get_yeardates("Seasonal",
                  yy = 2004,
                  start_year = 2004, end_year = 2012,
                  start_date = "2004.08.01", end_date = "2012.10.01"),
    c("2004.08.01", "2004.10.01")
  )

  expect_equal(
    get_yeardates("Seasonal",
                  yy = 2012,
                  start_year = 2004, end_year = 2012,
                  start_date = "2004.08.01", end_date = "2012.10.01"),
    c("2012.08.01", "2012.10.01")
  )


})
