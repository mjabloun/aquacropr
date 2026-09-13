test_that("write_weather writes Tnx header and daily records", {
  path <- file.path(withr_local_tempdir(), "Tunis.Tnx")
  data <- data.frame(
    Date = as.Date("2000-01-01") + 0:1,
    Tmin = c(7, 8),
    Tmax = c(15, 16)
  )
  write_weather(data, path, type = "Tnx")
  txt <- readLines(path)
  expect_match(txt[[2]], "^1 :")
  expect_equal(txt[[3]], "1 : First day of record (1, 11 or 21 for 10-day or 1 for months)")
  expect_equal(txt[[5]], "2000 : First year of record (1901 if not linked to a specific year)")
  expect_true(any(grepl("7.0", txt) & grepl("15.0", txt)))
})

test_that("write_cli lists the four climate files", {
  path <- file.path(withr_local_tempdir(), "Tunis.CLI")
  write_cli(path, tnx = "Tunis.Tnx", eto = "Tunis.ETo", plu = "Tunis.PLU")
  txt <- readLines(path)
  expect_equal(txt[[3]], "Tunis.Tnx")
  expect_equal(txt[[6]], "MaunaLoa.CO2")
})
