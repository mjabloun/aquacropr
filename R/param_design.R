#' Normalise parameter bounds to a data.frame of name/min/max
#' @keywords internal
.normalize_param_bounds <- function(params) {
  if (inherits(params, "aquacropr_parameters"))
    params <- params$parameters

  as_row <- function(p, fallback_name = NA_character_) {
    if (is.null(p)) stop("params contains a NULL entry", call. = FALSE)
    if (is.numeric(p) && length(p) == 2L && is.null(names(p))) {
      nm <- fallback_name
      if (is.na(nm) || !nzchar(nm)) {
        stop("A numeric min/max pair needs a name (use a named list or include `name`)",
             call. = FALSE)
      }
      return(data.frame(name = nm, min = as.numeric(p[1]), max = as.numeric(p[2]),
                        stringsAsFactors = FALSE))
    }
    if (is.list(p) || is.data.frame(p)) {
      nm <- p[["name", exact = TRUE]]
      if (is.null(nm) || (length(nm) == 1L && is.na(nm))) nm <- fallback_name
      if (is.null(nm) || (length(nm) == 1L && (is.na(nm) || !nzchar(as.character(nm))))) {
        stop("Each params entry must have a `name` (or be a named list element)",
             call. = FALSE)
      }
      if (is.null(p[["min", exact = TRUE]]) || is.null(p[["max", exact = TRUE]])) {
        stop("Each params entry must have `min` and `max`", call. = FALSE)
      }
      return(data.frame(name = as.character(nm),
                        min = as.numeric(p[["min", exact = TRUE]]),
                        max = as.numeric(p[["max", exact = TRUE]]),
                        stringsAsFactors = FALSE))
    }
    stop("params must be a data.frame or a list of {name, min, max} entries",
         call. = FALSE)
  }

  if (is.data.frame(params)) {
    if (!all(c("name", "min", "max") %in% names(params))) {
      stop("params data.frame must have columns `name`, `min`, and `max`",
           call. = FALSE)
    }
    df <- data.frame(
      name = as.character(params$name),
      min = as.numeric(params$min),
      max = as.numeric(params$max),
      stringsAsFactors = FALSE
    )
  } else if (is.list(params)) {
    nms <- names(params)
    rows <- vector("list", length(params))
    for (i in seq_along(params)) {
      fallback <- if (!is.null(nms) && nzchar(nms[i])) nms[i] else NA_character_
      rows[[i]] <- as_row(params[[i]], fallback)
    }
    df <- do.call(rbind, rows)
  } else {
    stop("params must be a data.frame or a list of {name, min, max} entries",
         call. = FALSE)
  }

  if (nrow(df) < 1L) stop("params must contain at least one parameter", call. = FALSE)
  if (anyNA(df$name) || any(!nzchar(df$name))) {
    stop("Every parameter must have a non-empty name", call. = FALSE)
  }
  if (anyDuplicated(df$name) > 0) {
    stop("Duplicate parameter name(s): ",
         paste(unique(df$name[duplicated(df$name)]), collapse = ", "),
         call. = FALSE)
  }
  if (anyNA(df$min) || anyNA(df$max)) {
    stop("params min/max must be numeric and non-missing", call. = FALSE)
  }
  if (any(df$min > df$max)) stop("Each parameter's min must be <= max", call. = FALSE)
  rownames(df) <- NULL
  df
}

#' @keywords internal
.scale_unit_design <- function(unit, bounds) {
  k <- nrow(bounds)
  if (ncol(unit) != k) {
    stop("Internal error: unit design has ", ncol(unit), " columns but ", k,
         " parameters", call. = FALSE)
  }
  out <- vapply(seq_len(k), function(j) {
    bounds$min[j] + (bounds$max[j] - bounds$min[j]) * unit[, j]
  }, numeric(nrow(unit)))
  if (is.null(dim(out))) out <- matrix(out, ncol = k)
  colnames(out) <- bounds$name
  as.data.frame(out, stringsAsFactors = FALSE)
}

#' @keywords internal
.unit_param_sample <- function(n, k, method, seed, ...) {
  method <- match.arg(method, c("lhs", "sobol", "dice_lhs"))
  if (!is.null(seed)) set.seed(seed)

  if (method == "lhs") {
    if (!requireNamespace("lhs", quietly = TRUE)) {
      stop("Package 'lhs' is required for method = \"lhs\"; please install it",
           call. = FALSE)
    }
    U <- lhs::maximinLHS(n, k)
  } else if (method == "dice_lhs") {
    if (!requireNamespace("DiceDesign", quietly = TRUE)) {
      stop("Package 'DiceDesign' is required for method = \"dice_lhs\"; please install it",
           call. = FALSE)
    }
    start <- DiceDesign::lhsDesign(n, dimension = k, seed = seed)
    optimized <- DiceDesign::maximinESE_LHS(start[["design", exact = TRUE]], ...)
    U <- optimized[["design", exact = TRUE]]
  } else {
    if (!requireNamespace("randtoolbox", quietly = TRUE)) {
      stop("Package 'randtoolbox' is required for method = \"sobol\"; please install it",
           call. = FALSE)
    }
    sobol_seed <- if (is.null(seed)) 4711L else as.integer(seed)
    U <- withCallingHandlers(
      randtoolbox::sobol(n, dim = k, scrambling = 3, seed = sobol_seed),
      warning = function(w) {
        if (grepl("scrambling is currently disabled", conditionMessage(w),
                  fixed = TRUE))
          invokeRestart("muffleWarning")
      }
    )
  }

  if (is.null(dim(U))) U <- matrix(U, ncol = 1L)
  if (k == 1L && ncol(U) != 1L) U <- matrix(U, ncol = 1L)
  U
}

#' @keywords internal
.as_param_design <- function(df, bounds, method, seed, n_drawn) {
  rownames(df) <- NULL
  class(df) <- c("aquacropr_param_design", "data.frame")
  attr(df, "params") <- bounds
  attr(df, "method") <- method
  attr(df, "seed") <- seed
  attr(df, "n_drawn") <- as.integer(n_drawn)
  df
}

#' Space-filling parameter design for a metamodel
#'
#' Draws `n` points in the hyper-rectangle defined by `params`. Methods:
#' `"lhs"` (`lhs::maximinLHS`), `"dice_lhs"` (`DiceDesign`), `"sobol"`
#' (`randtoolbox`). Only Sobol' designs can be extended later.
#'
#' @param params A `data.frame` with `name`/`min`/`max`, a list of those
#'   entries, or an `aquacropr_parameters` object.
#' @param n Number of rows.
#' @param method `"lhs"`, `"dice_lhs"`, or `"sobol"`.
#' @param seed Optional integer.
#' @param ... Forwarded to `DiceDesign::maximinESE_LHS()` when
#'   `method = "dice_lhs"`.
#' @return A `data.frame` of class `aquacropr_param_design`.
#' @export
#' @seealso [extend_param_design()], [run_param_design()]
generate_param_design <- function(params, n, method = c("lhs", "dice_lhs", "sobol"),
                                  seed = NULL, ...) {
  method <- match.arg(method)
  bounds <- .normalize_param_bounds(params)
  n <- as.integer(n)
  if (length(n) != 1L || is.na(n) || n < 1L)
    stop("`n` must be a positive integer", call. = FALSE)

  U <- .unit_param_sample(n, k = nrow(bounds), method = method, seed = seed, ...)
  df <- .scale_unit_design(U, bounds)
  .as_param_design(df, bounds = bounds, method = method, seed = seed, n_drawn = n)
}

#' Add points to an existing Sobol' parameter design
#'
#' @param existing_design From [generate_param_design()] or a previous call.
#' @param params Bounds; defaults to `attr(existing_design, "params")`.
#' @param n_additional Number of new rows.
#' @param method Must be `"sobol"`.
#' @return An `aquacropr_param_design` of the new rows only.
#' @export
extend_param_design <- function(existing_design, params = NULL, n_additional,
                                method = "sobol") {
  method <- match.arg(method, c("lhs", "dice_lhs", "sobol"))
  if (!identical(method, "sobol")) {
    stop("extend_param_design() is only meaningful for method = \"sobol\". ",
         "Optimized LHS designs (\"lhs\" or \"dice_lhs\") are not extensible; ",
         "call generate_param_design() with a larger n instead.",
         call. = FALSE)
  }

  existing_method <- attr(existing_design, "method")
  if (!is.null(existing_method) && !identical(existing_method, "sobol")) {
    stop("existing_design was built with method = \"", existing_method, "\". ",
         "Optimized LHS designs are not extensible; call generate_param_design() ",
         "with a larger n instead.",
         call. = FALSE)
  }

  if (is.null(params)) params <- attr(existing_design, "params")
  if (is.null(params)) {
    stop("`params` is missing and existing_design has no `params` attribute",
         call. = FALSE)
  }
  bounds <- .normalize_param_bounds(params)

  n_additional <- as.integer(n_additional)
  if (length(n_additional) != 1L || is.na(n_additional) || n_additional < 1L) {
    stop("`n_additional` must be a positive integer", call. = FALSE)
  }

  design_names <- names(existing_design)
  if (!identical(design_names, bounds$name)) {
    stop("params names do not match existing_design columns", call. = FALSE)
  }

  n_drawn <- attr(existing_design, "n_drawn")
  if (is.null(n_drawn)) n_drawn <- nrow(existing_design)
  n_drawn <- as.integer(n_drawn)
  seed <- attr(existing_design, "seed")

  n_total <- n_drawn + n_additional
  U <- .unit_param_sample(n_total, k = nrow(bounds), method = "sobol", seed = seed)
  U_new <- U[seq.int(n_drawn + 1L, n_total), , drop = FALSE]
  df <- .scale_unit_design(U_new, bounds)
  .as_param_design(df, bounds = bounds, method = "sobol", seed = seed,
                   n_drawn = n_total)
}

#' @keywords internal
.drop_nulls <- function(x) x[!vapply(x, is.null, logical(1))]

#' @keywords internal
.design_src_path <- function(rel, src_root) {
  if (grepl("^(?:[A-Za-z]:[\\\\/]|[\\\\/])", rel)) return(rel)
  if (!is.null(src_root) && nzchar(src_root)) file.path(src_root, rel) else rel
}

#' @keywords internal
.archived_filename <- function(run_id, path) {
  paste0(run_id, "_", basename(path))
}

#' @keywords internal
.check_unique_basenames <- function(output_files) {
  bases <- vapply(output_files, basename, character(1))
  if (anyDuplicated(bases) > 0) {
    stop("output_files must have unique basenames when kept as ",
         "run_<id>_<filename>; duplicates: ",
         paste(unique(bases[duplicated(bases)]), collapse = ", "),
         call. = FALSE)
  }
  invisible(bases)
}

#' @keywords internal
.copy_design_outputs <- function(output_files, src_root, keep_dir, run_id) {
  copied <- stats::setNames(rep(NA_character_, length(output_files)), names(output_files))
  for (nm in names(output_files)) {
    rel <- output_files[[nm]]
    src <- .design_src_path(rel, src_root)
    dest_name <- .archived_filename(run_id, rel)
    dest <- file.path(keep_dir, dest_name)
    if (!file.exists(src)) {
      warning(sprintf("run_param_design: output file not found, skipped: %s", src))
      next
    }
    ok <- file.copy(src, dest, overwrite = TRUE)
    if (!isTRUE(ok)) {
      warning(sprintf("run_param_design: failed to copy %s -> %s", src, dest))
      next
    }
    copied[[nm]] <- dest_name
  }
  copied
}

#' @keywords internal
.read_named_outputs <- function(output_files, src_root, output_reader, reader_args) {
  out <- vector("list", length(output_files))
  names(out) <- names(output_files)
  for (nm in names(output_files)) {
    rel <- output_files[[nm]]
    path <- .design_src_path(rel, src_root)
    if (!file.exists(path)) {
      warning(sprintf("Output file not found, skipped: %s", path))
      next
    }
    out[[nm]] <- do.call(output_reader, c(list(path), reader_args))
  }
  out
}

#' @keywords internal
.apply_sim_mutator <- function(tables, sim_mutator) {
  if (is.null(sim_mutator)) return(tables)
  mutate_one <- function(dt, fun, label) {
    if (is.null(dt) || is.null(fun)) return(dt)
    if (!is.function(fun)) {
      stop("sim_mutator", if (!is.null(label)) paste0("[['", label, "']]"),
           " must be a function", call. = FALSE)
    }
    fun(dt)
  }
  if (is.function(sim_mutator)) {
    return(lapply(tables, mutate_one, fun = sim_mutator, label = NULL))
  }
  if (is.list(sim_mutator)) {
    for (nm in names(tables))
      tables[[nm]] <- mutate_one(tables[[nm]], sim_mutator[[nm]], nm)
    return(tables)
  }
  stop("sim_mutator must be NULL, a function, or a named list of functions",
       call. = FALSE)
}

#' @keywords internal
.stack_design_outputs <- function(outputs_by_run) {
  file_nms <- unique(unlist(lapply(outputs_by_run, names), use.names = FALSE))
  stacked <- lapply(file_nms, function(nm) {
    pieces <- vector("list", length(outputs_by_run))
    for (i in seq_along(outputs_by_run)) {
      dt <- outputs_by_run[[i]][[nm]]
      if (is.null(dt)) next
      if (!is.data.frame(dt))
        dt <- data.table::data.table(value = dt)
      else
        dt <- data.table::copy(data.table::as.data.table(dt))
      rid <- names(outputs_by_run)[[i]]
      dt[, run_id := rid]
      data.table::setcolorder(dt, c("run_id", setdiff(names(dt), "run_id")))
      pieces[[i]] <- dt
    }
    data.table::rbindlist(pieces, fill = TRUE, use.names = TRUE)
  })
  names(stacked) <- file_nms
  if (length(stacked) == 1L) stacked[[1L]] else stacked
}

#' @keywords internal
.normalize_output_files <- function(output_files) {
  if (is.null(output_files) || length(output_files) == 0L) {
    stop("`output_files` must name at least one output file to keep and/or read",
         call. = FALSE)
  }
  files <- unlist(output_files, use.names = TRUE)
  if (is.null(names(files)) || any(!nzchar(names(files)))) {
    nms <- names(files)
    if (is.null(nms)) nms <- rep("", length(files))
    missing <- !nzchar(nms)
    nms[missing] <- vapply(files[missing], function(p) {
      tools::file_path_sans_ext(basename(p))
    }, character(1))
    names(files) <- nms
  }
  if (anyDuplicated(names(files)) > 0)
    stop("output_files names must be unique", call. = FALSE)
  files
}

#' Run AquaCrop for every row of a parameter design
#'
#' Renders templates, runs the plugin, then archives and/or reads selected
#' `.OUT` files. Archived copies are `run_<id>_<filename>` in one `keep_dir`.
#'
#' @param design A table from [generate_param_design()] (columns = names).
#' @param parameters An `aquacropr_parameters` object.
#' @param output_files Named paths relative to `working_dir`.
#' @param aquacrop_exe,working_dir,show_log,cmd,progress_window Passed to
#'   [run_aquacrop()].
#' @param template_dir,output_dir Passed to [render_templates()].
#' @param keep_files,keep_dir Archive copies after each run.
#' @param read Read outputs in-loop. Defaults to `!keep_files`.
#' @param output_reader Defaults to [read_out()].
#' @param digits,reader_args Extra arguments for `output_reader`.
#' @param sim_mutator Optional `function(dt)` or named list of those.
#' @param progress `TRUE`, `FALSE`, or `function(i, n)`.
#' @return A list with `design`, `keep_dir`, and `outputs`.
#' @export
run_param_design <- function(design, parameters, output_files,
                             aquacrop_exe = NULL, working_dir = NULL,
                             template_dir = ".", output_dir = ".",
                             show_log = FALSE,
                             keep_files = TRUE, keep_dir = "param_design_runs",
                             read = NULL, output_reader = read_out,
                             digits = NULL, reader_args = list(),
                             sim_mutator = NULL, progress = TRUE,
                             cmd = NULL, progress_window = FALSE) {
  if (is.null(design) || nrow(design) < 1L)
    stop("`design` must have at least one row", call. = FALSE)
  parameters <- as_parameters(parameters)

  keep_files <- isTRUE(keep_files)
  if (is.null(read)) read <- !keep_files
  read <- isTRUE(read)
  if (!keep_files && !read) {
    stop("When keep_files = FALSE, outputs must be read in-memory (read = TRUE)",
         call. = FALSE)
  }

  output_files <- .normalize_output_files(output_files)
  if (keep_files) .check_unique_basenames(output_files)
  reader_args <- .drop_nulls(utils::modifyList(list(digits = digits),
                                               reader_args))

  design_df <- as.data.frame(design, stringsAsFactors = FALSE)
  extra <- intersect(names(design_df), "run_id")
  if (length(extra)) design_df <- design_df[setdiff(names(design_df), extra)]

  param_names <- parameter_names(parameters)
  missing <- setdiff(param_names, names(design_df))
  if (length(missing) > 0) {
    stop("design is missing parameter(s): ", paste(missing, collapse = ", "),
         call. = FALSE)
  }

  n_runs <- nrow(design_df)
  run_ids <- if (!is.null(rownames(design)) &&
                 !identical(rownames(design), as.character(seq_len(n_runs)))) {
    rownames(design)
  } else {
    sprintf("run_%0*d", max(4L, nchar(as.character(n_runs))), seq_len(n_runs))
  }

  src_root <- if (!is.null(working_dir)) working_dir else "."
  if (keep_files) {
    dir.create(keep_dir, recursive = TRUE, showWarnings = FALSE)
    design_out <- data.frame(run_id = run_ids, design_df[, param_names, drop = FALSE],
                             stringsAsFactors = FALSE)
    utils::write.csv(design_out, file.path(keep_dir, "design.csv"), row.names = FALSE)
    saveRDS(list(output_files = as.list(output_files)),
            file.path(keep_dir, "manifest.rds"))
  }

  outputs <- if (read) vector("list", n_runs) else NULL
  if (read) names(outputs) <- run_ids

  prog <- .run_progress_ticker(progress, n_runs)
  if (!is.null(prog$pb)) on.exit(close(prog$pb), add = TRUE)
  prog$tick(0L)

  for (i in seq_len(n_runs)) {
    prog$tick(i - 1L)
    p <- as.numeric(design_df[i, param_names, drop = TRUE])
    values <- fill_values(parameters, p, names = param_names)
    render_templates(parameters, values, template_dir = template_dir,
                     output_dir = output_dir)
    run_aquacrop(
      working_dir = working_dir, aquacrop_exe = aquacrop_exe,
      show_log = show_log, cmd = cmd, progress_window = progress_window
    )

    if (keep_files)
      .copy_design_outputs(output_files, src_root, keep_dir, run_ids[i])

    if (read) {
      tables <- .read_named_outputs(output_files, src_root, output_reader, reader_args)
      tables <- .apply_sim_mutator(tables, sim_mutator)
      outputs[[i]] <- tables
    }
    prog$tick(i)
  }

  if (read) outputs <- .stack_design_outputs(outputs)

  list(
    design = data.frame(run_id = run_ids, design_df[, param_names, drop = FALSE],
                        stringsAsFactors = FALSE),
    keep_dir = if (keep_files) keep_dir else NULL,
    outputs = outputs
  )
}

#' Read archived outputs from a [run_param_design()] directory
#'
#' @param keep_dir Directory created with `keep_files = TRUE`.
#' @param output_files Original named paths; `NULL` uses `manifest.rds`.
#' @param output_reader,digits,reader_args,sim_mutator Same as
#'   [run_param_design()].
#' @return A list with `design` and `outputs`.
#' @export
read_param_design <- function(keep_dir, output_files = NULL,
                              output_reader = read_out,
                              digits = NULL, reader_args = list(),
                              sim_mutator = NULL) {
  if (!dir.exists(keep_dir)) stop("keep_dir does not exist: ", keep_dir, call. = FALSE)

  reader_args <- .drop_nulls(utils::modifyList(list(digits = digits),
                                               reader_args))

  design_path <- file.path(keep_dir, "design.csv")
  design <- if (file.exists(design_path)) {
    utils::read.csv(design_path, stringsAsFactors = FALSE)
  } else {
    NULL
  }

  if (is.null(output_files)) {
    man_path <- file.path(keep_dir, "manifest.rds")
    if (!file.exists(man_path)) {
      stop("output_files is NULL and keep_dir has no manifest.rds", call. = FALSE)
    }
    output_files <- unlist(readRDS(man_path)$output_files, use.names = TRUE)
  }
  output_files <- .normalize_output_files(output_files)

  if (is.null(design) || !"run_id" %in% names(design)) {
    stop("keep_dir must contain design.csv with a run_id column", call. = FALSE)
  }
  run_ids <- as.character(design$run_id)

  outputs <- vector("list", length(run_ids))
  names(outputs) <- run_ids

  for (i in seq_along(run_ids)) {
    paths <- vapply(output_files, function(rel) {
      file.path(keep_dir, .archived_filename(run_ids[i], rel))
    }, character(1))
    tables <- .read_named_outputs(paths, src_root = NULL, output_reader, reader_args)
    tables <- .apply_sim_mutator(tables, sim_mutator)
    outputs[[i]] <- tables
  }

  list(design = design, outputs = .stack_design_outputs(outputs))
}
