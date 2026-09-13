obs_sim_pair <- function() {
  dates <- as.Date("2020-01-01") + 0:2
  obs <- data.table::data.table(Date = dates, yield = c(10, 12, 11))
  list(
    obs = obs,
    objective = objective_spec(
      obs_data = obs,
      value_map = list(list(obs_col = "yield", sim_col = "Ydry")),
      metric = "RMSE"
    )
  )
}

test_that("score_param_design scores a stacked in-memory table", {
  pair <- obs_sim_pair()
  dates <- pair$obs$Date
  outputs <- rbind(
    data.frame(run_id = "run_0001", Date = dates, Ydry = c(10, 12, 11), stringsAsFactors = FALSE),
    data.frame(run_id = "run_0002", Date = dates, Ydry = c(0, 0, 0), stringsAsFactors = FALSE)
  )
  run <- list(
    design = data.frame(run_id = c("run_0001", "run_0002"), p1 = c(0.1, 0.9),
                        stringsAsFactors = FALSE),
    outputs = outputs
  )

  scored <- score_param_design(run, pair$objective)
  expect_s3_class(scored, "aquacropr_scored_design")
  expect_equal(scored$score[1], 0)
  expect_gt(scored$score[2], scored$score[1])
  expect_true(inherits(attr(scored, "objective"), "aquacropr_objective"))
})

test_that("score_param_design scores a named list of outputs and can maximize", {
  pair <- obs_sim_pair()
  dates <- pair$obs$Date
  crop <- rbind(
    data.frame(run_id = "run_0001", Date = dates, Ydry = c(10.5, 12, 11), stringsAsFactors = FALSE),
    data.frame(run_id = "run_0002", Date = dates, Ydry = c(10, 12, 11), stringsAsFactors = FALSE)
  )
  run <- list(
    design = data.frame(run_id = c("run_0001", "run_0002"), p1 = 1:2, stringsAsFactors = FALSE),
    outputs = list(crop = crop, climate = crop)
  )

  scored <- score_param_design(run, pair$objective, output_name = "crop")
  flipped <- score_param_design(run, pair$objective, output_name = "crop", maximize = TRUE)
  expect_equal(flipped$score, -scored$score)
})

test_that("score_param_design scores archived keep_dir files", {
  pair <- obs_sim_pair()
  keep <- withr_local_tempdir()
  dates <- pair$obs$Date
  data.table::fwrite(
    data.table::data.table(Date = dates, Ydry = c(10, 12, 11)),
    file.path(keep, "run_0001_crop.OUT")
  )
  data.table::fwrite(
    data.table::data.table(Date = dates, Ydry = c(1, 1, 1)),
    file.path(keep, "run_0002_crop.OUT")
  )
  design <- data.frame(run_id = c("run_0001", "run_0002"), p1 = c(0.2, 0.8),
                       stringsAsFactors = FALSE)
  utils::write.csv(design, file.path(keep, "design.csv"), row.names = FALSE)
  saveRDS(list(output_files = list(crop = "OUTP/crop.OUT")),
          file.path(keep, "manifest.rds"))

  obj <- objective_spec(
    obs_data = pair$obs,
    value_map = list(list(obs_col = "yield", sim_col = "Ydry")),
    sim_reader = data.table::fread,
    metric = "RMSE"
  )
  run <- list(design = design, keep_dir = keep)
  scored <- score_param_design(run, obj, output_name = "crop")
  expect_equal(scored$score[1], 0)
  expect_gt(scored$score[2], 0)
})

test_that("score_param_design maps a named objective onto the matching output", {
  dates <- as.Date("2020-01-01") + 0:1
  obs <- data.table::data.table(Date = dates, theta = c(0.2, 0.3))
  swc <- rbind(
    data.frame(run_id = "run_0001", Date = dates, swc = c(0.21, 0.28), stringsAsFactors = FALSE),
    data.frame(run_id = "run_0002", Date = dates, swc = c(0.0, 0.0), stringsAsFactors = FALSE)
  )
  other <- rbind(
    data.frame(run_id = "run_0001", Date = dates, Ydry = c(1, 1), stringsAsFactors = FALSE),
    data.frame(run_id = "run_0002", Date = dates, Ydry = c(9, 9), stringsAsFactors = FALSE)
  )
  spec <- objective_spec(
    obs_data = obs,
    value_map = list(list(obs_col = "theta", sim_col = "swc")),
    sim_reader = function(path, data = NULL) {
      if (!is.null(data)) data.table::as.data.table(data) else data.table::fread(path)
    },
    metric = "RMSE",
    output_file = "wheatPRMday.OUT"
  )
  obj <- composite_objective(list(swc = spec), weights = 1, name = "swc_of")
  run <- list(
    design = data.frame(run_id = c("run_0001", "run_0002"), p1 = 1:2, stringsAsFactors = FALSE),
    outputs = list(crop = other, wheatPRMday = swc)
  )
  scored <- score_param_design(run, obj)
  expect_equal(scored$score[1], hydroGOF::rmse(c(0.21, 0.28), c(0.2, 0.3)))
  expect_gt(scored$score[2], scored$score[1])
})

test_that("score_param_design errors without outputs or keep_dir", {
  pair <- obs_sim_pair()
  expect_error(
    score_param_design(list(design = data.frame(run_id = "run_0001")), pair$objective),
    "neither"
  )
})

test_that("fit_metamodel and predict wrap DiceKriging", {
  skip_if_not_installed("DiceKriging")
  set.seed(1)
  n <- 12
  scored <- data.frame(p1 = stats::runif(n), p2 = stats::runif(n))
  scored$score <- 2 * scored$p1 + 0.3 * scored$p2
  params <- data.frame(name = c("p1", "p2"), min = 0, max = 1)

  mm <- fit_metamodel(scored, params, control = list(trace = FALSE))
  expect_s3_class(mm, "aquacropr_metamodel")
  expect_equal(mm$param_names, c("p1", "p2"))
  expect_output(print(mm), "aquacropr_metamodel")

  pred <- predict(mm, newdata = data.frame(p1 = 0.5, p2 = 0.5))
  expect_true("mean" %in% names(pred))
  expect_length(pred$mean, 1)

  check <- validate_metamodel(mm, scored, method = "loo")
  expect_s3_class(check, "aquacropr_metamodel_validation")
  expect_true(is.finite(check$result$Q2))
  expect_output(print(check), "Q2")
})

test_that("validate_metamodel kfold uses DiceEval when installed", {
  skip_if_not_installed("DiceKriging")
  skip_if_not_installed("DiceEval")
  set.seed(2)
  n <- 15
  scored <- data.frame(p1 = stats::runif(n), p2 = stats::runif(n))
  scored$score <- scored$p1 - scored$p2
  params <- data.frame(name = c("p1", "p2"), min = 0, max = 1)
  mm <- fit_metamodel(scored, params, control = list(trace = FALSE))
  check <- suppressMessages(validate_metamodel(mm, scored, method = "kfold", K = 3))
  expect_equal(check$method, "kfold")
  expect_true("Q2" %in% names(check$result) || !is.null(check$result[["Q2", exact = TRUE]]))
})

test_that("calibrate_ego takes EGO steps with a mocked AquaCrop objective", {
  skip_if_not_installed("DiceKriging")
  skip_if_not_installed("DiceOptim")
  parameters <- read_parameters(test_path("fixtures", "parameters_ok.yaml"))
  set.seed(3)
  n <- 10
  fnames <- parameter_names(parameters)
  scored <- as.data.frame(matrix(stats::runif(n * length(fnames)), n, length(fnames)))
  names(scored) <- fnames
  scored$score <- as.numeric(as.matrix(scored[, fnames]) %*% seq_along(fnames) / 10)

  mm <- fit_metamodel(scored, parameters, control = list(trace = FALSE))

  local_mocked_bindings(
    evaluate_candidate = function(p, ...) {
      sum((as.numeric(p) - 0.4)^2)
    }
  )

  result <- suppressWarnings(calibrate_ego(
    mm, parameters,
    objective = structure(list(), class = "aquacropr_objective"),
    sim_file = "out.OUT",
    nsteps = 1,
    control = list(pop.size = 20, max.generations = 3, wait.generations = 1,
                   BFGSburnin = 0, print.level = 0)
  )
  expect_true(all(fnames %in% names(result$best_params)))
  expect_true(is.finite(result$best_score))
  expect_s3_class(result$metamodel, "aquacropr_metamodel")
})
