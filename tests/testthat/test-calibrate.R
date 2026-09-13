test_that("calibrate rejects unknown methods", {
  parameters <- read_parameters(test_path("fixtures", "parameters_ok.yaml"))
  expect_error(
    calibrate(parameters, structure(list(), class = "aquacropr_objective"),
              sim_file = "x.OUT", method = "nelder-mead"),
    "Unknown calibrate method"
  )
})

test_that("calibrate DDS minimises a mocked quadratic", {
  parameters <- read_parameters(test_path("fixtures", "parameters_ok.yaml"))
  local_mocked_bindings(
    evaluate_candidate = function(p, ...) {
      p <- as.numeric(p)
      (p[[1]] - 0.90)^2 + ((p[[2]] - 16) / 5)^2
    }
  )
  res <- calibrate(
    parameters, structure(list(), class = "aquacropr_objective"),
    sim_file = "x.OUT", method = "DDS",
    control = list(maxeval = 120, seed = 1, start = c(0.85, 14))
  )
  expect_equal(res$method, "DDS")
  expect_equal(res$fitted_names, c("CCx", "WP"))
  expect_lt(res$best_score, 0.02)
  expect_equal(unname(res$best_params[["CCx"]]), 0.90, tolerance = 0.03)
  expect_equal(unname(res$best_params[["WP"]]), 16, tolerance = 0.4)
})

test_that("calibrate multi-BOBYQA tries several starts", {
  skip_if_not_installed("minqa")
  parameters <- read_parameters(test_path("fixtures", "parameters_ok.yaml"))
  local_mocked_bindings(
    evaluate_candidate = function(p, ...) {
      p <- as.numeric(p)
      (p[[1]] - 0.90)^2 + ((p[[2]] - 16) / 5)^2
    }
  )
  res <- calibrate(
    parameters, structure(list(), class = "aquacropr_objective"),
    sim_file = "x.OUT", method = "multi-BOBYQA",
    control = list(nstarts = 3, maxfun = 40, seed = 1, start = c(0.85, 14))
  )
  expect_equal(res$method, "multi-BOBYQA")
  expect_equal(res$fit$nstarts, 3L)
  expect_equal(nrow(res$fit$starts), 3L)
  expect_lt(res$best_score, 1e-4)
  expect_equal(unname(res$best_params[["CCx"]]), 0.90, tolerance = 0.01)
})

test_that("calibrate BOBYQA minimises a mocked quadratic", {
  skip_if_not_installed("minqa")
  parameters <- read_parameters(test_path("fixtures", "parameters_ok.yaml"))
  local_mocked_bindings(
    evaluate_candidate = function(p, ...) {
      p <- as.numeric(p)
      (p[[1]] - 0.90)^2 + ((p[[2]] - 16) / 5)^2
    }
  )
  res <- calibrate(
    parameters, structure(list(), class = "aquacropr_objective"),
    sim_file = "x.OUT", method = "BOBYQA",
    control = list(maxfun = 80, start = c(0.85, 14))
  )
  expect_equal(res$method, "BOBYQA")
  expect_lt(res$best_score, 1e-4)
  expect_equal(unname(res$best_params[["CCx"]]), 0.90, tolerance = 0.01)
  expect_equal(unname(res$best_params[["WP"]]), 16, tolerance = 0.05)
})

test_that("calibrate CMA-ES minimises a mocked quadratic", {
  skip_if_not_installed("cmaes")
  parameters <- read_parameters(test_path("fixtures", "parameters_ok.yaml"))
  local_mocked_bindings(
    evaluate_candidate = function(p, ...) {
      p <- as.numeric(p)
      (p[[1]] - 0.90)^2 + ((p[[2]] - 16) / 5)^2
    }
  )
  res <- suppressMessages(calibrate(
    parameters, structure(list(), class = "aquacropr_objective"),
    sim_file = "x.OUT", method = "CMA-ES",
    control = list(maxit = 40, start = c(0.85, 14))
  ))
  expect_equal(res$method, "CMA-ES")
  expect_lt(res$best_score, 0.05)
})

test_that("calibrate reporter is called for DDS", {
  parameters <- read_parameters(test_path("fixtures", "parameters_ok.yaml"))
  n <- 0L
  local_mocked_bindings(
    evaluate_candidate = function(p, ...) sum(as.numeric(p)^2)
  )
  calibrate(
    parameters, structure(list(), class = "aquacropr_objective"),
    sim_file = "x.OUT", method = "DDS",
    control = list(maxeval = 5, seed = 2),
    reporter = function(info) {
      n <<- n + 1L
      expect_true(is.numeric(info$last_score))
    }
  )
  expect_equal(n, 5L)
})
