test_that("read_parameters parses YAML parameters", {
  params <- read_parameters(test_path("fixtures", "parameters_ok.yaml"))
  expect_s3_class(params, "aquacropr_parameters")
  expect_setequal(parameter_names(params), c("CCx", "WP"))
})

test_that("read_parameters rejects duplicate names", {
  yaml_txt <- "
parameters:
  - {name: p1, default: 1, min: 0, max: 2, from_file: a.CRO, to_file: b.CRO}
  - {name: p1, default: 1, min: 0, max: 2, from_file: a.CRO, to_file: b.CRO}
"
  tmp <- tempfile(fileext = ".yaml")
  writeLines(yaml_txt, tmp)
  on.exit(unlink(tmp))
  expect_error(read_parameters(tmp), "Duplicate parameter")
})

test_that("parameters must have from_file and to_file", {
  yaml_txt <- "
parameters:
  - {name: p1, default: 1, min: 0, max: 2, from_file: a.CRO}
"
  tmp <- tempfile(fileext = ".yaml")
  writeLines(yaml_txt, tmp)
  on.exit(unlink(tmp))
  expect_error(read_parameters(tmp), "from_file and to_file")
})

test_that("validate_parameters checks placeholders exist exactly once", {
  params <- read_parameters(test_path("fixtures", "parameters_ok.yaml"))
  expect_true(validate_parameters(params, template_dir = test_path("fixtures")))
})

test_that("render_templates substitutes crop placeholders", {
  params <- read_parameters(test_path("fixtures", "parameters_ok.yaml"))
  out <- withr_local_tempdir()
  render_templates(
    params,
    values = c(CCx = 0.91, WP = 15.5),
    template_dir = test_path("fixtures"),
    output_dir = out
  )
  txt <- readLines(file.path(out, "wheat.CRO"))
  expect_true(any(grepl("0.91", txt)))
  expect_true(any(grepl("15.5", txt)))
  expect_false(any(grepl("\\{CCx\\}|\\{WP\\}", txt)))
})

test_that("fill_values pins unspecified names at defaults", {
  params <- read_parameters(test_path("fixtures", "parameters_ok.yaml"))
  vals <- fill_values(params, p = 0.9, names = "CCx")
  expect_equal(unname(vals[["CCx"]]), 0.9)
  expect_equal(unname(vals[["WP"]]), 17)
})

test_that("expand_names returns names in parameters order", {
  params <- read_parameters(test_path("fixtures", "parameters_ok.yaml"))
  expect_equal(expand_names(params, NULL), c("CCx", "WP"))
  expect_equal(expand_names(params, c("WP", "CCx")), c("CCx", "WP"))
  expect_error(expand_names(params, "nope"), "Unknown parameter")
})
