withr_local_tempdir <- function(env = parent.frame()) {
  dir <- tempfile("aquacropr-test-")
  dir.create(dir)
  do.call("on.exit", list(bquote(unlink(.(dir), recursive = TRUE)), add = TRUE), envir = env)
  dir
}
