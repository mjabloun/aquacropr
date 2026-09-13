test_that("FAO day-number example 24 August 1982 is 29821", {
  expect_equal(aquacrop_day_number(as.Date("1982-08-24")), 29821L)
})

test_that("1 January 1901 is day 1", {
  expect_equal(aquacrop_day_number(as.Date("1901-01-01")), 1L)
})

test_that("aquacrop_date inverts aquacrop_day_number", {
  dates <- as.Date(c("1901-01-01", "1982-08-24", "2000-02-29", "2099-12-31"))
  expect_equal(aquacrop_date(aquacrop_day_number(dates)), dates)
})
