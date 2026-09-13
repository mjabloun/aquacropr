isolate_aquacrop_path <- function(code) {
  old_opt <- getOption("aquacropr.aquacrop_exe")
  old_env <- Sys.getenv("AQUACROP_EXE", unset = NA_character_)
  on.exit({
    options(aquacropr.aquacrop_exe = old_opt)
    if (is.na(old_env)) Sys.unsetenv("AQUACROP_EXE") else Sys.setenv(AQUACROP_EXE = old_env)
  }, add = TRUE)
  options(aquacropr.aquacrop_exe = NULL)
  Sys.unsetenv("AQUACROP_EXE")
  force(code)
}

fake_exe <- function() {
  tmp <- tempfile(fileext = ".exe")
  file.create(tmp)
  normalizePath(tmp, winslash = "/", mustWork = TRUE)
}

test_that("set_aquacrop_path stores a session path for get_aquacrop_path()", {
  isolate_aquacrop_path({
    path <- fake_exe()
    expect_equal(set_aquacrop_path(path), path)
    expect_equal(get_aquacrop_path(), path)
  })
})

test_that("set_aquacrop_path errors when the file is missing", {
  expect_error(set_aquacrop_path(tempfile()), "not found")
})

test_that("set_aquacrop_path(NULL) clears the session option", {
  isolate_aquacrop_path({
    path <- fake_exe()
    set_aquacrop_path(path)
    set_aquacrop_path(NULL)
    expect_null(getOption("aquacropr.aquacrop_exe"))
    expect_equal(
      aquacropr:::.find_aquacrop_exe(error = FALSE, candidates = character(0)),
      ""
    )
  })
})

test_that("get_aquacrop_path prefers set_aquacrop_path over AQUACROP_EXE", {
  isolate_aquacrop_path({
    via_opt <- fake_exe()
    via_env <- fake_exe()
    Sys.setenv(AQUACROP_EXE = via_env)
    set_aquacrop_path(via_opt)
    expect_equal(get_aquacrop_path(), via_opt)

    set_aquacrop_path(NULL)
    expect_equal(get_aquacrop_path(), via_env)
  })
})

test_that(".build_aquacrop_cmd quotes the local executable by default", {
  expect_equal(
    as.character(aquacropr:::.build_aquacrop_cmd(aquacrop_exe = "C:/aquacrop.exe")),
    '"C:/aquacrop.exe"'
  )
})

test_that(".stash_bundled_tkinter renames _tkinter.pyd and restores it", {
  env <- getFromNamespace(".TKINTER_STASH", "aquacropr")
  env$depth <- 0L
  plugin <- tempfile("ac-plugin-")
  dir.create(file.path(plugin, "_internal"), recursive = TRUE)
  exe <- file.path(plugin, "AquaCrop73.exe")
  pyd <- file.path(plugin, "_internal", "_tkinter.pyd")
  off <- paste0(pyd, ".aquacropr-off")
  file.create(exe)
  writeLines("tk", pyd)
  on.exit({
    if (file.exists(off) && !file.exists(pyd)) file.rename(off, pyd)
  }, add = TRUE)

  restore <- aquacropr:::.stash_bundled_tkinter(exe)
  expect_false(file.exists(pyd))
  expect_true(file.exists(off))
  restore()
  expect_true(file.exists(pyd))
  expect_false(file.exists(off))
})

test_that(".stash_bundled_tkinter nested calls restore only once", {
  env <- getFromNamespace(".TKINTER_STASH", "aquacropr")
  env$depth <- 0L
  plugin <- tempfile("ac-plugin-")
  dir.create(file.path(plugin, "_internal"), recursive = TRUE)
  exe <- file.path(plugin, "AquaCrop73.exe")
  pyd <- file.path(plugin, "_internal", "_tkinter.pyd")
  off <- paste0(pyd, ".aquacropr-off")
  file.create(exe)
  writeLines("tk", pyd)
  on.exit({
    if (file.exists(off) && !file.exists(pyd)) file.rename(off, pyd)
  }, add = TRUE)

  inner <- NULL
  outer <- aquacropr:::.stash_bundled_tkinter(exe)
  inner <- aquacropr:::.stash_bundled_tkinter(exe)
  expect_false(file.exists(pyd))
  inner()
  expect_false(file.exists(pyd))
  outer()
  expect_true(file.exists(pyd))
})

test_that(".build_aquacrop_cmd interpolates a custom template", {
  cmd <- aquacropr:::.build_aquacrop_cmd(
    aquacrop_exe = NULL,
    cmd = "singularity run --bind {working_dir}:{working_dir} ~/ac.sif",
    working_dir = "/work"
  )
  expect_equal(
    as.character(cmd),
    "singularity run --bind /work:/work ~/ac.sif"
  )
})
