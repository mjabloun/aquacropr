test_that("prepare_plugin_workdir copies SIMUL, LIST, and ListProjects.txt", {
  root <- withr_local_tempdir()
  simul <- file.path(root, "src_simul")
  dir.create(simul)
  writeLines("dummy", file.path(simul, "MaunaLoa.CO2"))
  prm <- file.path(root, "Field.PRM")
  writeLines("project", prm)

  wd <- prepare_plugin_workdir(
    path = file.path(root, "run"),
    projects = prm,
    simul_dir = simul,
    relativize_to = FALSE
  )

  expect_true(file.exists(file.path(wd, "SIMUL", "MaunaLoa.CO2")))
  expect_true(file.exists(file.path(wd, "LIST", "Field.PRM")))
  expect_true(dir.exists(file.path(wd, "OUTP")))
  expect_equal(readLines(file.path(wd, "LIST", "ListProjects.txt")), "Field.PRM")
  daily <- readLines(file.path(wd, "SIMUL", "DailyResults.SIM"))
  expect_true(any(grepl("^ 1 :", daily)))
  expect_true(any(grepl("^ 2 :", daily)))
})

test_that("write_daily_results_sim rejects unknown codes", {
  expect_error(write_daily_results_sim(tempfile(), codes = 9), "1:8")
})

test_that("relativize_project_paths rewrites absolute dirs relative to the exe folder", {
  root <- withr_local_tempdir()
  plugin <- file.path(root, "AquaCrop73_windows")
  data <- file.path(root, "GUI_AC7.3", "DATA")
  dir.create(plugin, recursive = TRUE)
  dir.create(data, recursive = TRUE)
  prm <- file.path(root, "wheat.PRM")
  writeLines(c(
    "Tunis wheat",
    "   Tunis.CLI",
    paste0("   ", normalizePath(data, winslash = "\\", mustWork = TRUE), "\\")
  ), prm)
  dest <- file.path(root, "out.PRM")
  relativize_project_paths(prm, dest, plugin)
  txt <- paste(readLines(dest), collapse = "\n")
  expect_false(grepl("[A-Za-z]:\\\\", txt))
  expect_match(txt, "\\.\\.\\\\GUI_AC7\\.3\\\\DATA")
})
