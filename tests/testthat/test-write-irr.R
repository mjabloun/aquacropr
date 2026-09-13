irr_code <- function(path, line) {
  as.integer(sub(":.*", "", trimws(readLines(path)[[line]])))
}

test_that("write_irr net mode matches Inet-style depletion", {
  path <- file.path(withr_local_tempdir(), "Inet.IRR")
  write_irr(path, mode = "net", raw_depletion = 30)
  expect_equal(irr_code(path, 3), 1L)
  expect_equal(irr_code(path, 5), 3L)
  expect_equal(irr_code(path, 6), 30L)
})

test_that("write_irr generate mode writes RAW depletion back to FC", {
  path <- file.path(withr_local_tempdir(), "Igen.IRR")
  write_irr(
    path,
    mode = "generate",
    time_criterion = "depletion_raw",
    depth_criterion = "field_capacity",
    rules = data.frame(from_day = 1, time_value = 80, depth_value = 0)
  )
  expect_equal(irr_code(path, 5), 2L)
  expect_equal(irr_code(path, 6), 3L)
  expect_equal(irr_code(path, 7), 1L)
  last <- scan(text = tail(readLines(path), 1), quiet = TRUE)
  expect_equal(last, c(1, 80, 0, 0))
})

test_that("write_irr events mode writes day, depth, ECw", {
  path <- file.path(withr_local_tempdir(), "sched.IRR")
  write_irr(
    path,
    mode = "events",
    method = "furrow",
    events = data.frame(day = c(1, 10), depth = c(20, 50), ecw = 0.5)
  )
  expect_equal(irr_code(path, 3), 4L)
  expect_equal(irr_code(path, 4), 90L)
  expect_equal(irr_code(path, 5), 1L)
  expect_equal(irr_code(path, 6), -9L)
  body <- tail(readLines(path), 2)
  expect_equal(scan(text = body[[1]], quiet = TRUE), c(1, 20, 0.5))
  expect_equal(scan(text = body[[2]], quiet = TRUE), c(10, 50, 0.5))
})

test_that("write_irr generate first rule must start on day 1", {
  expect_error(
    write_irr(
      tempfile(fileext = ".IRR"),
      mode = 2,
      rules = data.frame(from_day = 10, time_value = 7, depth_value = 40)
    ),
    "from_day = 1"
  )
})
