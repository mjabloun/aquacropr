test_that("write_project writes a single-run .PRO with FAO file triplets", {
  root <- withr_local_tempdir()
  data <- file.path(root, "DATA")
  dir.create(data)
  crop <- file.path(data, "Wheat.CRO")
  soil <- file.path(data, "Silt.SOL")
  cli  <- file.path(data, "Tunis.CLI")
  tnx  <- file.path(data, "Tunis.Tnx")
  eto  <- file.path(data, "Tunis.ETo")
  plu  <- file.path(data, "Tunis.PLU")
  co2  <- file.path(data, "MaunaLoa.CO2")
  writeLines("crop", crop)
  writeLines("soil", soil)
  writeLines("tnx", tnx)
  writeLines("eto", eto)
  writeLines("plu", plu)
  writeLines("co2", co2)
  write_cli(cli, tnx = "Tunis.Tnx", eto = "Tunis.ETo",
               plu = "Tunis.PLU", co2 = "MaunaLoa.CO2")

  dest <- file.path(root, "field")
  out <- write_project(
    dest,
    crop = crop,
    soil = soil,
    climate = cli,
    sim_start = as.Date("1979-11-22"),
    sim_end   = as.Date("1980-05-23"),
    relativize_to = FALSE
  )
  expect_match(out, "field\\.PRO$")
  lines <- readLines(out)
  expect_equal(length(lines), 2L + 47L)
  expect_true(any(grepl("28815", lines)))
  expect_true(any(grepl("Wheat\\.CRO", lines)))
  expect_true(any(grepl("Silt\\.SOL", lines)))
  expect_true(any(grepl("Tunis\\.CLI", lines)))
  expect_true(any(grepl("\\(None\\)", lines)))
  expect_equal(sum(grepl("^-- [0-9]+\\.", lines)), 10L)
})

test_that("write_project repeats date+file blocks for multiple seasons", {
  root <- withr_local_tempdir()
  crop <- file.path(root, "c.CRO")
  soil <- file.path(root, "s.SOL")
  writeLines("x", crop)
  writeLines("x", soil)
  dest <- file.path(root, "multi.PRM")
  write_project(
    dest,
    crop = crop,
    soil = soil,
    sim_start = as.Date(c("2000-03-01", "2001-03-01")),
    sim_end   = as.Date(c("2000-07-01", "2001-07-01")),
    relativize_to = FALSE
  )
  lines <- readLines(dest)
  expect_equal(length(lines), 2L + 2L * 47L)
  expect_equal(sum(grepl("Year number of cultivation", lines)), 2L)
  expect_equal(sum(grepl("c\\.CRO", lines)), 2L)
})

test_that("write_project seasons data.frame and relative directories", {
  root <- withr_local_tempdir()
  plugin <- file.path(root, "plugin")
  data <- file.path(root, "DATA")
  dir.create(plugin)
  dir.create(data)
  crop <- file.path(data, "c.CRO")
  soil <- file.path(data, "s.SOL")
  writeLines("x", crop)
  writeLines("x", soil)
  dest <- file.path(plugin, "LIST", "run.PRM")
  write_project(
    dest,
    crop = crop,
    soil = soil,
    seasons = data.frame(
      sim_start = as.Date("2011-03-01"),
      sim_end   = as.Date("2011-08-01"),
      cultivation_year = 1L
    ),
    relativize_to = plugin
  )
  txt <- paste(readLines(dest), collapse = "\n")
  expect_false(grepl("[A-Za-z]:\\\\", txt))
  expect_match(txt, "\\.\\.\\\\DATA")
})

test_that("write_project errors when crop is missing", {
  expect_error(
    write_project(
      tempfile(fileext = ".PRO"),
      crop = tempfile(),
      soil = tempfile(),
      sim_start = as.Date("2000-01-01"),
      sim_end = as.Date("2000-02-01"),
      relativize_to = FALSE
    ),
    "crop"
  )
})

test_that("write_project(program_parameters = TRUE) writes a sibling .PP1", {
  root <- withr_local_tempdir()
  crop <- file.path(root, "c.CRO")
  soil <- file.path(root, "s.SOL")
  writeLines("x", crop)
  writeLines("x", soil)
  dest <- write_project(
    file.path(root, "one.PRO"),
    crop = crop,
    soil = soil,
    sim_start = as.Date("2011-03-01"),
    sim_end = as.Date("2011-08-01"),
    relativize_to = FALSE,
    program_parameters = list(ke_max = 1.05)
  )
  pp <- file.path(root, "one.PP1")
  expect_true(file.exists(pp))
  expect_equal(as.numeric(sub("\\s*:.*", "", trimws(readLines(pp))))[2], 1.05)
})
