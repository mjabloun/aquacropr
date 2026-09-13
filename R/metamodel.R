#' Drop a stacked `run_id` column from a design output table
#' @keywords internal
.drop_run_id <- function(tbl) {
  tbl <- data.table::as.data.table(tbl)
  if ("run_id" %in% names(tbl)) tbl[, run_id := NULL]
  tbl[]
}

.slice_design_run_outputs <- function(outputs, rid) {
  rid <- as.character(rid)
  take <- function(tbl) {
    if (is.null(tbl) || !NROW(tbl)) return(tbl)
    tbl <- data.table::as.data.table(tbl)
    if ("run_id" %in% names(tbl))
      tbl <- tbl[as.character(tbl$run_id) == rid]
    .drop_run_id(tbl)
  }
  if (is.data.frame(outputs)) return(take(outputs))
  stats::setNames(lapply(outputs, take), names(outputs))
}

.objective_output_stems <- function(objective) {
  comps <- if (inherits(objective, "aquacropr_composite_objective"))
    objective$components else list(objective)
  stems <- vapply(comps, function(comp) {
    ofile <- as.character(comp$output_file)
    if (!length(ofile) || is.na(ofile[[1]]) || !nzchar(ofile[[1]]))
      return(NA_character_)
    tools::file_path_sans_ext(basename(ofile[[1]]))
  }, character(1))
  stems[nzchar(stems) & !is.na(stems)]
}

.match_design_output_key <- function(stem, keys) {
  if (!length(keys)) return(NA_character_)
  san <- gsub("[^A-Za-z0-9_]+", "_", stem)
  if (stem %in% keys) return(stem)
  if (san %in% keys) return(san)
  hit <- keys[tolower(keys) == tolower(stem) | tolower(keys) == tolower(san)]
  if (length(hit)) return(hit[[1]])
  NA_character_
}

.select_sim_outputs_for_objective <- function(sliced, objective, output_name = NULL) {
  if (is.data.frame(sliced)) return(sliced)
  keys <- names(sliced)
  stems <- .objective_output_stems(objective)
  mapped <- list()
  for (stem in stems) {
    key <- .match_design_output_key(stem, keys)
    if (is.na(key)) {
      stop("Design outputs do not include '", stem,
           "', the .OUT mapped on the selected objective. Available: ",
           paste(keys, collapse = ", "), call. = FALSE)
    }
    mapped[[stem]] <- sliced[[key]]
    if (!identical(key, stem)) mapped[[key]] <- sliced[[key]]
  }
  if (length(mapped)) return(mapped)
  if (!is.null(output_name) && nzchar(output_name) && output_name %in% keys)
    return(sliced[[output_name]])
  sliced
}

#' Score every run of a parameter design against an objective
#'
#' Appends a `score` column to `run$design` for [fit_metamodel()].
#'
#' @param run Return value of [run_param_design()] or [read_param_design()].
#' @param objective An [objective_spec()].
#' @param output_name Name in `output_files` / `run$outputs`.
#' @param output_files Original named paths; needed only for archived runs
#'   without `manifest.rds`.
#' @param maximize If `TRUE`, store negated scores (smaller-is-better).
#' @param return_per_pair Keep per-variable scores as an attribute.
#' @return `run$design` plus `score` (class `aquacropr_scored_design`).
#' @export
score_param_design <- function(run, objective, output_name = NULL, output_files = NULL,
                               maximize = FALSE, return_per_pair = FALSE) {
  stopifnot(inherits(objective, "aquacropr_objective"))
  design <- as.data.frame(run$design, stringsAsFactors = FALSE)
  if (nrow(design) < 1L || !"run_id" %in% names(design)) {
    stop("run must have a `design` data.frame with a run_id column", call. = FALSE)
  }
  run_ids <- as.character(design$run_id)

  has_outputs <- !is.null(run$outputs)
  has_archive <- !is.null(run$keep_dir)
  if (!has_outputs && !has_archive) {
    stop("run has neither `keep_dir` nor `outputs`", call. = FALSE)
  }
  use_archive <- !has_outputs

  if (use_archive) {
    if (is.null(output_files)) {
      man_path <- file.path(run$keep_dir, "manifest.rds")
      if (file.exists(man_path))
        output_files <- unlist(readRDS(man_path)$output_files, use.names = TRUE)
    }
    stems <- .objective_output_stems(objective)
    if (is.null(output_name) || !nzchar(output_name)) {
      if (length(stems)) {
        key <- .match_design_output_key(stems[[1]], names(output_files))
        if (!is.na(key)) output_name <- key
      }
    }
    if (is.null(output_name) || !nzchar(as.character(output_name)[[1]])) {
      stop("`output_name` is required when scoring an archived run that has no objective output_file",
           call. = FALSE)
    }
    if (is.null(output_files) || !output_name %in% names(output_files)) {
      stop("output_files must name '", output_name, "'", call. = FALSE)
    }
    rel <- output_files[[output_name]]
  }

  score_one <- function(rid) {
    if (use_archive) {
      sim_file <- file.path(run$keep_dir, .archived_filename(rid, rel))
      return(evaluate_objective(objective, sim_file, return_per_pair = return_per_pair))
    }
    sliced <- .slice_design_run_outputs(run$outputs, rid)
    sim_outputs <- .select_sim_outputs_for_objective(sliced, objective, output_name)
    evaluate_objective(objective, sim_file = "", sim_outputs = sim_outputs,
                       return_per_pair = return_per_pair)
  }

  results <- stats::setNames(lapply(run_ids, score_one), run_ids)
  scores <- vapply(results, `[[`, numeric(1), "summary")
  if (maximize) scores <- -scores

  scored <- design
  scored$score <- as.numeric(scores)
  class(scored) <- c("aquacropr_scored_design", "data.frame")
  attr(scored, "objective") <- objective
  attr(scored, "maximize") <- maximize
  if (return_per_pair)
    attr(scored, "per_variable") <- lapply(results, `[[`, "per_variable")
  scored
}

#' Fit a Kriging metamodel to a scored parameter design
#'
#' @param scored_design Table with parameter columns and a response (usually
#'   `score` from [score_param_design()]).
#' @param params Bounds or an `aquacropr_parameters` object.
#' @param response_col Response column name (default `"score"`).
#' @param covtype Passed to `DiceKriging::km()`.
#' @param ... Further arguments to `DiceKriging::km()`.
#' @return An `aquacropr_metamodel`.
#' @export
fit_metamodel <- function(scored_design, params, response_col = "score",
                          covtype = "matern5_2", ...) {
  if (!requireNamespace("DiceKriging", quietly = TRUE)) {
    stop("Package 'DiceKriging' is required for fit_metamodel(); please install it",
         call. = FALSE)
  }

  bounds <- .normalize_param_bounds(params)
  param_names <- bounds$name
  missing <- setdiff(param_names, names(scored_design))
  if (length(missing) > 0) {
    stop("scored_design is missing parameter(s): ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  if (!response_col %in% names(scored_design)) {
    stop("scored_design has no '", response_col, "' column", call. = FALSE)
  }

  X <- as.data.frame(scored_design[, param_names, drop = FALSE], stringsAsFactors = FALSE)
  y <- scored_design[[response_col]]

  km_fit <- DiceKriging::km(design = X, response = y, covtype = covtype, ...)
  structure(
    list(km = km_fit, param_names = param_names, params = bounds,
         response_col = response_col),
    class = "aquacropr_metamodel"
  )
}

#' Predict from a fitted AquaCrop metamodel
#'
#' @param object An `aquacropr_metamodel`.
#' @param newdata A data.frame with `object$param_names`.
#' @param type `"UK"` or `"SK"`.
#' @param ... Forwarded to DiceKriging's `predict` method.
#' @export
#' @method predict aquacropr_metamodel
predict.aquacropr_metamodel <- function(object, newdata, type = "UK", ...) {
  if (!requireNamespace("DiceKriging", quietly = TRUE)) {
    stop("Package 'DiceKriging' is required to predict from an aquacropr_metamodel",
         call. = FALSE)
  }
  newX <- as.data.frame(newdata)[, object$param_names, drop = FALSE]
  stats::predict(object$km, newdata = newX, type = type, ...)
}

#' @export
print.aquacropr_metamodel <- function(x, ...) {
  cat("<aquacropr_metamodel>\n")
  cat("  names: ", paste(x$param_names, collapse = ", "), "\n", sep = "")
  cat("  training points: ", nrow(x$km@X), "\n", sep = "")
  cat("  covariance: ", x$km@covariance@name, "\n", sep = "")
  invisible(x)
}

#' Validate a fitted metamodel
#'
#' @param metamodel An `aquacropr_metamodel`.
#' @param scored_design The training table.
#' @param method `"loo"` or `"kfold"`.
#' @param K Folds for k-fold.
#' @param ... Passed to `leaveOneOut.km` or `DiceEval::modelFit`.
#' @return An `aquacropr_metamodel_validation`.
#' @export
validate_metamodel <- function(metamodel, scored_design, method = c("loo", "kfold"),
                               K = 10, ...) {
  method <- match.arg(method)
  stopifnot(inherits(metamodel, "aquacropr_metamodel"))
  y <- scored_design[[metamodel$response_col]]

  if (method == "loo") {
    if (!requireNamespace("DiceKriging", quietly = TRUE)) {
      stop("Package 'DiceKriging' is required for method = \"loo\"", call. = FALSE)
    }
    loo <- DiceKriging::leaveOneOut.km(metamodel$km, type = "UK", ...)
    loo_mean <- loo[["mean", exact = TRUE]]
    loo_sd <- loo[["sd", exact = TRUE]]
    resid <- y - loo_mean
    result <- list(
      predicted = loo_mean, sd = loo_sd, residuals = resid,
      Q2 = 1 - sum(resid^2) / sum((y - mean(y))^2),
      RMSE = sqrt(mean(resid^2))
    )
  } else {
    if (!requireNamespace("DiceEval", quietly = TRUE)) {
      stop("Package 'DiceEval' is required for method = \"kfold\"", call. = FALSE)
    }
    X <- scored_design[, metamodel$param_names, drop = FALSE]
    modfit <- DiceEval::modelFit(X, y, type = "Kriging",
                                 covtype = metamodel$km@covariance@name, ...)
    result <- DiceEval::crossValidation(modfit, K = K)
  }
  structure(list(method = method, result = result),
            class = "aquacropr_metamodel_validation")
}

#' @export
print.aquacropr_metamodel_validation <- function(x, ...) {
  cat("<aquacropr_metamodel_validation: ", x$method, ">\n", sep = "")
  if (x$method == "loo") {
    cat(sprintf("  Q2   = %.4f\n", x$result$Q2))
    cat(sprintf("  RMSE = %.4g\n", x$result$RMSE))
  } else {
    cat(sprintf("  Q2      = %.4f\n", x$result[["Q2", exact = TRUE]]))
    cat(sprintf("  RMSE_CV = %.4g\n", x$result[["RMSE_CV", exact = TRUE]]))
  }
  invisible(x)
}
