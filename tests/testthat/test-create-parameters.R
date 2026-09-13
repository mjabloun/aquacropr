test_that("create_parameters scaffolds scalar entries", {
  txt <- create_parameters(c("CCx", "WP"))
  expect_true(grepl("name: CCx", txt))
  expect_true(grepl("\\{CCx\\}", txt))
  expect_true(grepl("REPLACE_ME.CRO", txt))
  expect_true(grepl("name: WP", txt))
})

test_that("create_parameters writes to a file when path is given", {
  tmp <- tempfile(fileext = ".yaml")
  on.exit(unlink(tmp))
  result <- create_parameters("CCx", path = tmp)
  expect_equal(as.character(result), tmp)
  expect_true(any(grepl("CCx", readLines(tmp))))
})

test_that("scaffolded YAML is parseable after filling placeholders", {
  tmp_yaml <- tempfile(fileext = ".yaml")
  tmp_cro <- tempfile(fileext = ".CRO")
  on.exit(unlink(c(tmp_yaml, tmp_cro)))
  writeLines("  {CCx}  : Maximum canopy cover", tmp_cro)

  txt <- create_parameters("CCx")
  txt <- gsub("REPLACE_ME.CRO", basename(tmp_cro), txt)
  writeLines(txt, tmp_yaml)

  params <- read_parameters(tmp_yaml)
  expect_equal(params$parameters$name, "CCx")
  expect_true(validate_parameters(params, template_dir = dirname(tmp_cro)))
})

test_that("create_parameters rejects duplicate names", {
  expect_error(create_parameters(c("a", "a")), "duplicate")
})
