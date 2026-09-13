test_that("read_out parses daily crop output and adds Date", {
  dt <- read_out(test_path("fixtures", "wheatCROP.OUT"))
  expect_equal(nrow(dt), 2)
  expect_equal(dt$Date, as.Date(c("2011-03-01", "2011-03-02")))
  expect_equal(dt$CC, c(12.5, 14.0))
  expect_equal(dt$Biomass, c(0.10, 0.12))
})

test_that("read_out skips Run headers in combined 7.3 daily files", {
  path <- file.path(withr_local_tempdir(), "xPRMday.OUT")
  writeLines(c(
    "AquaCrop 7.3",
    "   Run:   1",
    "     Day Month  Year   DAP Stage   CC",
    "      -     -     -     -     -    %",
    "      1     3  2011     1     2  12.5",
    "   Run:   2",
    "     Day Month  Year   DAP Stage   CC",
    "      -     -     -     -     -    %",
    "      2     3  2012     1     2  20.0"
  ), path)
  dt <- read_out(path)
  expect_equal(nrow(dt), 2)
  expect_equal(dt$Run, c(1L, 2L))
  expect_equal(dt$Date, as.Date(c("2011-03-01", "2012-03-02")))
  expect_equal(dt$CC, c(12.5, 20.0))
})

test_that("list_outputs names files by stem", {
  dir <- withr_local_tempdir()
  file.copy(test_path("fixtures", "wheatCROP.OUT"), file.path(dir, "wheatCROP.OUT"))
  listed <- list_outputs(dir)
  expect_equal(names(listed), "wheatCROP")
})

test_that("list_outputs omits plugin status files", {
  dir <- withr_local_tempdir()
  file.copy(test_path("fixtures", "wheatCROP.OUT"), file.path(dir, "wheatCROP.OUT"))
  writeLines("done", file.path(dir, "AllDone.OUT"))
  writeLines("loaded", file.path(dir, "ListProjectsLoaded.OUT"))
  listed <- list_outputs(dir)
  expect_equal(names(listed), "wheatCROP")
})
