# Sensitivity-analysis plotting. AquaCrop is not invoked: responses are a
# stand-in linear function of the design matrix.

skip_if_not_installed("sensitivity")
skip_if_not_installed("ggplot2")
skip_if_not_installed("ggrepel")

test_that("plot_sa errors on an incomplete Morris design", {
  parameters <- read_parameters(test_path("fixtures", "parameters_ok.yaml"))
  design <- sa_design(parameters, method = "morris", r = 5, seed = 1)
  expect_error(plot_sa(design), "no elementary effects")
})

test_that("plot_sa errors on an unrelated object", {
  expect_error(plot_sa(list(X = 1)), "morris or sobol")
})

test_that("plot_sa draws a Morris mu* vs sigma plot and returns indices", {
  parameters <- read_parameters(test_path("fixtures", "parameters_ok.yaml"))
  design <- sa_design(parameters, method = "morris", r = 5, seed = 1)
  y <- as.matrix(design$X) %*% c(10, 0.1)
  sa <- complete_sa_analysis(design, y)

  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp)
  on.exit({ grDevices::dev.off(); unlink(tmp) })

  df <- plot_sa(sa)

  expect_s3_class(df, "data.frame")
  expect_equal(df$name, parameter_names(parameters))
  expect_true(all(c("mu", "mu_star", "sigma") %in% names(df)))
  expect_true(all(is.finite(df$mu_star)))
})

test_that("plot_sa(type = 'bar') returns the same Morris indices", {
  parameters <- read_parameters(test_path("fixtures", "parameters_ok.yaml"))
  design <- sa_design(parameters, method = "morris", r = 5, seed = 1)
  y <- as.matrix(design$X) %*% c(10, 0.1)
  sa <- complete_sa_analysis(design, y)

  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp)
  on.exit({ grDevices::dev.off(); unlink(tmp) })

  expect_equal(plot_sa(sa, type = "bar"), plot_sa(sa, type = "effects"))
})

test_that("plot_sa rejects an unknown Morris type", {
  parameters <- read_parameters(test_path("fixtures", "parameters_ok.yaml"))
  design <- sa_design(parameters, method = "morris", r = 5, seed = 1)
  sa <- complete_sa_analysis(design, as.matrix(design$X) %*% c(10, 0.1))
  expect_error(plot_sa(sa, type = "spaghetti"), "should be one of")
})

test_that("plot_sa errors on an incomplete Sobol design", {
  set.seed(1)
  n <- 20
  X1 <- data.frame(p1 = stats::runif(n), p2 = stats::runif(n))
  X2 <- data.frame(p1 = stats::runif(n), p2 = stats::runif(n))
  design <- sensitivity::soboljansen(model = NULL, X1 = X1, X2 = X2)
  expect_error(plot_sa(design), "no indices")
})

test_that("plot_sa draws Sobol S vs ST bars and returns indices", {
  set.seed(1)
  n <- 30
  X1 <- data.frame(p1 = stats::runif(n), p2 = stats::runif(n))
  X2 <- data.frame(p1 = stats::runif(n), p2 = stats::runif(n))
  design <- sensitivity::soboljansen(model = NULL, X1 = X1, X2 = X2)
  sa <- complete_sa_analysis(design, as.matrix(design$X) %*% c(2, 0.1))

  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp)
  on.exit({ grDevices::dev.off(); unlink(tmp) })

  df <- plot_sa(sa)

  expect_s3_class(df, "data.frame")
  expect_equal(df$name, c("p1", "p2"))
  expect_true(all(c("S", "ST") %in% names(df)))
  expect_true(all(is.finite(df$S)))
  expect_true(all(is.finite(df$ST)))
  expect_gt(df$S[df$name == "p1"], df$S[df$name == "p2"])
})

test_that("suggest_calibrate_names keeps influential names", {
  parameters <- read_parameters(test_path("fixtures", "parameters_ok.yaml"))
  idx <- data.frame(
    name = c("CCx", "WP"),
    mu_star = c(1, 0.02),
    stringsAsFactors = FALSE
  )
  got <- suggest_calibrate_names(idx, parameters, frac = 0.1)
  expect_equal(got, "CCx")
})

test_that("sa_design stores the parameters object", {
  parameters <- read_parameters(test_path("fixtures", "parameters_ok.yaml"))
  design <- sa_design(parameters, method = "morris", r = 4, seed = 2)
  expect_s3_class(attr(design, "parameters"), "aquacropr_parameters")
  expect_equal(colnames(design$X), parameter_names(parameters))
})
