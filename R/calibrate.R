#' One calibration iteration: render templates, run AquaCrop, score
#'
#' Building block of [calibrate()]. Call it directly to score one candidate
#' or the best-fit vector from an optimizer.
#'
#' @param p Numeric vector of values, in the order of `names` (default:
#'   every name in [parameter_names()]).
#' @param parameters An `aquacropr_parameters` object.
#' @param objective An objective from [objective_spec()].
#' @param sim_file Path to the `.OUT` file scored after the run (relative
#'   to `working_dir` if that is set).
#' @param aquacrop_exe,working_dir,show_log,cmd,progress_window Passed to
#'   [run_aquacrop()].
#' @param template_dir,output_dir Passed to [render_templates()].
#' @param maximize If `TRUE`, return `-summary` so a minimizer can maximize
#'   a metric such as KGE.
#' @param names Names to vary. Others stay at defaults. `NULL` means all.
#'
#' @return Numeric scalar: the objective summary (or its negation).
#' @export
evaluate_candidate <- function(p, parameters, objective, sim_file,
                               aquacrop_exe = NULL, working_dir = NULL,
                               template_dir = ".", output_dir = ".",
                               show_log = FALSE, maximize = FALSE, cmd = NULL,
                               names = NULL, progress_window = FALSE) {
  parameters <- as_parameters(parameters)
  values <- fill_values(parameters, p, names = names)
  render_templates(parameters, values, template_dir = template_dir,
                   output_dir = output_dir)
  run_aquacrop(
    working_dir = working_dir, aquacrop_exe = aquacrop_exe,
    show_log = show_log, cmd = cmd, progress_window = progress_window
  )
  path <- sim_file
  if (!is.null(working_dir) && !grepl("^(?:[A-Za-z]:[/\\\\]|[/\\\\])", sim_file)) {
    path <- file.path(working_dir, sim_file)
  }
  score <- evaluate_objective(objective, path)$summary
  if (maximize) score <- -score
  score
}

#' Calibrate AquaCrop parameters against observations
#'
#' Fits `names` (or every name in [parameter_names()]) against
#' [evaluate_candidate()]. Bounds come from `min` / `max`. Methods:
#'
#' \describe{
#'   \item{`DEoptim`}{Differential evolution (`DEoptim`). Default. Control:
#'     `NP`, `itermax`, plus other `DEoptim.control()` keys.}
#'   \item{`BOBYQA`}{Bound-constrained local search (`minqa::bobyqa`).
#'     Good as a polish after screening. Control: `maxfun` / `maxeval`,
#'     `npt`, `rhobeg`, `rhoend`, `start`.}
#'   \item{`multi-BOBYQA`}{BOBYQA from several starts. The first start is
#'     `control$start` (or the defaults) unless `include_default = FALSE`.
#'     The rest are an LHS (or uniform) sample in the box. Control:
#'     `nstarts` (default 8), `seed`, `include_default`, plus BOBYQA keys.}
#'   \item{`DDS`}{Dynamically Dimensioned Search (Tolson & Shoemaker 2007),
#'     implemented in the package. Control: `maxeval` (default 200),
#'     `r` (default 0.2), `seed`, `start`.}
#'   \item{`CMA-ES`}{Covariance matrix adaptation (`cmaes::cma_es`).
#'     Control: `maxit`, `maxeval` (used to set `maxit` if omitted),
#'     `sigma`, `start`, plus other `cma_es` control keys.}
#' }
#'
#' Pass `names` to fit a subset (typical after Morris / Sobol); the rest
#' stay at defaults. `control$start` is the initial vector for BOBYQA, DDS,
#' and CMA-ES (defaults, clipped inside the box).
#'
#' @inheritParams evaluate_candidate
#' @param method `"DEoptim"` (default), `"BOBYQA"`, `"multi-BOBYQA"`,
#'   `"DDS"`, or `"CMA-ES"`.
#' @param control Method-specific list (see above).
#' @param reporter Optional function called after every evaluation. It
#'   receives `evals`, `iter`, `np`, `n_total`, `generation_end`,
#'   `last_score`, `best_score`, and `best_params` (full named vector).
#'
#' @return A list with `best_params`, `fitted_names`, `best_score`, `method`,
#'   and `fit` (the raw optimizer object).
#' @export
calibrate <- function(parameters, objective, sim_file,
                      aquacrop_exe = NULL, working_dir = NULL,
                      template_dir = ".", output_dir = ".",
                      show_log = FALSE, maximize = FALSE,
                      method = "DEoptim", control = list(),
                      reporter = NULL, cmd = NULL,
                      names = NULL, progress_window = FALSE) {
  method <- .normalize_calibrate_method(method)
  if (is.null(control)) control <- list()

  parameters <- as_parameters(parameters)
  params <- parameters$parameters
  param_names <- expand_names(parameters, names)
  if (!length(param_names)) stop("No parameters selected to calibrate.", call. = FALSE)
  hit <- match(param_names, params$name)
  lower <- as.numeric(params$min[hit])
  upper <- as.numeric(params$max[hit])
  defaults <- parameter_defaults(parameters)
  start <- .calibrate_start(defaults, param_names, lower, upper, control)

  if (!is.null(reporter) && identical(method, "DEoptim")) control$trace <- FALSE
  extra <- list(
    parameters = parameters, objective = objective, sim_file = sim_file,
    aquacrop_exe = aquacrop_exe, working_dir = working_dir,
    template_dir = template_dir, output_dir = output_dir,
    show_log = show_log, maximize = maximize, cmd = cmd,
    names = param_names, progress_window = progress_window
  )
  eval_fn <- function(p, ...) do.call(evaluate_candidate, c(list(p = p), extra))
  n_total <- .calibrate_n_total(method, control, length(param_names))
  np <- .calibrate_np(method, control, length(param_names))
  fill_best <- function(shown) fill_values(parameters, shown, names = param_names)
  fn <- .wrap_calibrate_reporter(
    eval_fn, reporter, maximize, fill_best, n_total, np
  )

  got <- .run_calibrate_optimizer(method, fn, start, lower, upper, control)
  best_params <- fill_values(parameters, got$par, names = param_names)
  best_score <- if (maximize) -got$value else got$value

  list(
    best_params = best_params,
    fitted_names = param_names,
    best_score = best_score,
    method = method,
    fit = got$fit
  )
}

#' Surrogate-assisted calibration via Efficient Global Optimization
#'
#' Each of `nsteps` iterations evaluates one real AquaCrop candidate via
#' [evaluate_candidate()] and refits the Kriging model.
#'
#' @param metamodel An `aquacropr_metamodel` from [fit_metamodel()].
#' @inheritParams evaluate_candidate
#' @param nsteps Number of real AquaCrop evaluations.
#' @param control,kmcontrol Forwarded to `DiceOptim::EGO.nsteps()`.
#' @return A list with `best_params`, `best_score`, `fit`, and a refit
#'   `metamodel`.
#' @export
calibrate_ego <- function(metamodel, parameters, objective, sim_file,
                          nsteps = 10,
                          aquacrop_exe = NULL, working_dir = NULL,
                          template_dir = ".", output_dir = ".",
                          show_log = FALSE, maximize = FALSE,
                          control = NULL, kmcontrol = NULL, cmd = NULL,
                          progress_window = FALSE) {
  if (!requireNamespace("DiceOptim", quietly = TRUE)) {
    stop("Package 'DiceOptim' is required for calibrate_ego(); please install it",
         call. = FALSE)
  }
  stopifnot(inherits(metamodel, "aquacropr_metamodel"))
  parameters <- as_parameters(parameters)

  param_names <- parameter_names(parameters)
  if (!identical(metamodel$param_names, param_names)) {
    stop("metamodel's names do not match parameter_names(parameters)",
         call. = FALSE)
  }

  params <- parameters$parameters
  real_fun <- function(p) {
    evaluate_candidate(
      p, parameters, objective, sim_file,
      aquacrop_exe = aquacrop_exe, working_dir = working_dir,
      template_dir = template_dir, output_dir = output_dir,
      show_log = show_log, maximize = maximize, cmd = cmd,
      progress_window = progress_window
    )
  }

  fit <- DiceOptim::EGO.nsteps(
    model = metamodel$km, fun = real_fun, nsteps = nsteps,
    lower = params$min, upper = params$max,
    control = control, kmcontrol = kmcontrol
  )

  new_X <- as.matrix(as.data.frame(fit[["par", exact = TRUE]])[, param_names, drop = FALSE])
  new_y <- as.numeric(unlist(fit[["value", exact = TRUE]]))
  all_X <- rbind(as.matrix(metamodel$km@X), new_X)
  all_y <- c(as.numeric(metamodel$km@y), new_y)

  best_i <- which.min(all_y)
  best_params <- stats::setNames(as.numeric(all_X[best_i, ]), param_names)
  best_score <- if (maximize) -all_y[best_i] else all_y[best_i]

  refit <- structure(
    list(km = fit[["lastmodel", exact = TRUE]], param_names = param_names,
         params = metamodel$params, response_col = metamodel$response_col),
    class = "aquacropr_metamodel"
  )

  list(best_params = best_params, best_score = best_score, fit = fit,
       metamodel = refit)
}
