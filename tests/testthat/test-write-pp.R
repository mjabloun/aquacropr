pp_values <- function(path) {
  as.numeric(sub("\\s*:.*", "", trimws(readLines(path))))
}

test_that("write_pp writes FAO defaults next to a .PRM as .PPn", {
  root <- withr_local_tempdir()
  prm <- file.path(root, "field.PRM")
  file.create(prm)
  dest <- write_pp(prm)
  expect_equal(basename(dest), "field.PPn")
  vals <- pp_values(dest)
  expect_equal(length(vals), 25L)
  expect_equal(vals[1], 4)
  expect_equal(vals[2], 1.10)
  expect_equal(vals[6], -6)
  expect_equal(vals[19], 12.0)
  expect_equal(vals[20], 28.0)
})

test_that("write_pp uses .PP1 beside a .PRO and accepts overrides", {
  root <- withr_local_tempdir()
  pro <- file.path(root, "barley.PRO")
  file.create(pro)
  dest <- write_pp(pro, ke_max = 1.25, cn_amc = 0L)
  expect_equal(basename(dest), "barley.PP1")
  vals <- pp_values(dest)
  expect_equal(vals[2], 1.25)
  expect_equal(vals[15], 0)
})

test_that("write_pp rejects unknown parameter names", {
  expect_error(write_pp(tempfile(fileext = ".PP1"), nope = 1), "Unknown")
})

test_that(".stash_program_parameters hides LIST PP files and restores them", {
  env <- getFromNamespace(".PP_STASH", "aquacropr")
  env$depth <- 0L
  root <- withr_local_tempdir()
  list_dir <- file.path(root, "LIST")
  dir.create(list_dir)
  pp <- file.path(list_dir, "field.PPn")
  writeLines("keep", pp)
  on.exit({
    env$depth <- 0L
    off <- paste0(pp, ".aquacropr-off")
    if (file.exists(off) && !file.exists(pp)) file.rename(off, pp)
  }, add = TRUE)

  restore <- aquacropr:::.stash_program_parameters(root)
  expect_false(file.exists(pp))
  expect_true(file.exists(paste0(pp, ".aquacropr-off")))
  restore()
  expect_true(file.exists(pp))
  expect_equal(readLines(pp), "keep")
})

test_that("prepare_plugin_workdir copies sibling .PPn when present", {
  root <- withr_local_tempdir()
  simul <- file.path(root, "src_simul")
  dir.create(simul)
  writeLines("dummy", file.path(simul, "MaunaLoa.CO2"))
  prm <- file.path(root, "Field.PRM")
  writeLines("project", prm)
  write_pp(prm, ke_max = 1.15)

  wd <- prepare_plugin_workdir(
    path = file.path(root, "run"),
    projects = prm,
    simul_dir = simul,
    relativize_to = FALSE
  )
  copied <- file.path(wd, "LIST", "Field.PPn")
  expect_true(file.exists(copied))
  expect_equal(pp_values(copied)[2], 1.15)

  wd2 <- prepare_plugin_workdir(
    path = file.path(root, "run2"),
    projects = prm,
    simul_dir = simul,
    relativize_to = FALSE,
    program_parameters = FALSE
  )
  expect_false(file.exists(file.path(wd2, "LIST", "Field.PPn")))
})
