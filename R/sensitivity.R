#' Build a Morris or Sobol-Jansen design from parameters
#'
#' Wraps `sensitivity::morris()` / `sensitivity::soboljansen()` with
#' `model = NULL`. Bounds come from each parameter's `min` / `max`. Evaluate
#' the design with [run_sa_design()] and attach responses with
#' [complete_sa_analysis()].
#'
#' @param parameters An `aquacropr_parameters` object.
#' @param method `"morris"` or `"sobol"`.
#' @param r Number of Morris trajectories.
#' @param design Passed to `sensitivity::morris()`.
#' @param N Sobol-Jansen base sample size.
#' @param scale Whether to scale with `min`/`max`.
#' @param seed Optional seed.
#' @return A `morris` or Sobol object whose `$X` is the sample matrix.
#' @export
sa_design <- function(parameters, method = c("morris", "sobol"), r = 10,
                      design = list(type = "oat", levels = 5, grid.jump = 3),
                      N = 1000, scale = TRUE, seed = NULL) {
  if (!requireNamespace("sensitivity", quietly = TRUE)) {
    stop("Package 'sensitivity' is required for sa_design(); please install it",
         call. = FALSE)
  }
  method <- match.arg(method)
  parameters <- as_parameters(parameters)
  params <- parameters$parameters
  if (!is.null(seed)) set.seed(seed)

  sa_obj <- if (method == "morris") {
    sensitivity::morris(
      model = NULL, factors = params$name, r = r, design = design,
      binf = params$min, bsup = params$max, scale = scale
    )
  } else {
    sensitivity::soboljansen(
      model = NULL, factors = params$name, N = N,
      binf = params$min, bsup = params$max, scale = scale
    )
  }
  attr(sa_obj, "parameters") <- parameters
  sa_obj
}

#' @noRd
.run_progress_ticker <- function(progress, n_runs) {
  pb <- NULL
  tick <- function(i) invisible(NULL)
  if (is.function(progress)) {
    tick <- function(i) {
      progress(i, n_runs)
      invisible(NULL)
    }
  } else if (isTRUE(progress) && n_runs > 0L) {
    pb <- utils::txtProgressBar(min = 0, max = n_runs, style = 3)
    tick <- function(i) utils::setTxtProgressBar(pb, i)
  }
  list(tick = tick, pb = pb)
}

#' @noRd
.sa_resolve_path <- function(path, working_dir) {
  if (is.null(working_dir) || grepl("^(?:[A-Za-z]:[/\\\\]|[/\\\\])", path)) {
    return(path)
  }
  file.path(working_dir, path)
}

#' Run AquaCrop for every row of a sensitivity design
#'
#' @param sa_obj From [sa_design()].
#' @param parameters An `aquacropr_parameters` object (defaults to
#'   `attr(sa_obj, "parameters")`).
#' @param output_files Named paths of `.OUT` files to read after each run.
#' @param response_fun Function of a named list of tables, returning a
#'   numeric scalar (or fixed-length vector).
#' @param aquacrop_exe,working_dir,show_log,cmd,progress_window Passed to
#'   [run_aquacrop()].
#' @param template_dir,output_dir Passed to [render_templates()].
#' @param output_reader Defaults to [read_out()].
#' @param progress `TRUE` for a console bar, `FALSE` for none, or
#'   `function(i, n)`.
#' @return A list with `run_log` and `responses`.
#' @export
run_sa_design <- function(sa_obj, parameters = NULL, output_files, response_fun,
                          aquacrop_exe = NULL, working_dir = NULL,
                          template_dir = ".", output_dir = ".",
                          show_log = FALSE, output_reader = read_out,
                          progress = TRUE, cmd = NULL,
                          progress_window = FALSE) {
  if (is.null(sa_obj$X)) stop("sa_obj has no design matrix (X)", call. = FALSE)
  if (is.null(parameters)) parameters <- attr(sa_obj, "parameters")
  if (is.null(parameters)) {
    stop("`parameters` is missing; pass it or build sa_obj with sa_design()",
         call. = FALSE)
  }
  parameters <- as_parameters(parameters)
  if (!length(output_files)) {
    stop("`output_files` must name at least one output file", call. = FALSE)
  }
  if (is.null(names(output_files)) || any(!nzchar(names(output_files)))) {
    nms <- names(output_files)
    if (is.null(nms)) nms <- rep("", length(output_files))
    nms[!nzchar(nms)] <- tools::file_path_sans_ext(basename(output_files[!nzchar(nms)]))
    names(output_files) <- nms
  }

  design_matrix <- as.matrix(sa_obj$X)
  param_names <- parameter_names(parameters)
  if (!all(colnames(design_matrix) %in% param_names)) {
    stop("sa_obj design columns do not match parameter_names(parameters)",
         call. = FALSE)
  }

  n_runs <- nrow(design_matrix)
  param_ids <- if (!is.null(rownames(design_matrix))) {
    rownames(design_matrix)
  } else {
    sprintf("run_%04d", seq_len(n_runs))
  }

  run_log <- data.table::data.table(param_id = param_ids,
                                    values = vector("list", n_runs))
  responses <- vector("list", n_runs)

  prog <- .run_progress_ticker(progress, n_runs)
  if (!is.null(prog$pb)) on.exit(close(prog$pb), add = TRUE)
  prog$tick(0L)

  for (i in seq_len(n_runs)) {
    prog$tick(i - 1L)
    p <- as.numeric(design_matrix[i, , drop = TRUE])
    names(p) <- colnames(design_matrix)
    values <- fill_values(parameters, p, names = names(p))
    run_log$values[[i]] <- as.list(values)

    render_templates(parameters, values, template_dir = template_dir,
                     output_dir = output_dir)
    run_aquacrop(
      working_dir = working_dir, aquacrop_exe = aquacrop_exe,
      show_log = show_log, cmd = cmd, progress_window = progress_window
    )

    outputs <- lapply(output_files, function(p) {
      output_reader(.sa_resolve_path(p, working_dir))
    })
    names(outputs) <- names(output_files)
    responses[[i]] <- response_fun(outputs)
    prog$tick(i)
  }

  list(
    run_log = run_log,
    responses = do.call(rbind, lapply(responses, matrix, nrow = 1))
  )
}

#' Suggest calibration names from Morris \eqn{\mu^*} or Sobol ST
#'
#' Keeps names at least `frac` of the maximum index. If none pass, keeps
#' the top-ranked name.
#'
#' @param indices A data.frame with `name` and `mu_star` or `ST`.
#' @param parameters An `aquacropr_parameters` object.
#' @param frac Relative threshold (default `0.1`).
#' @return Character vector of names, in parameters order.
#' @export
suggest_calibrate_names <- function(indices, parameters, frac = 0.1) {
  parameters <- as_parameters(parameters)
  idx <- as.data.frame(indices, stringsAsFactors = FALSE)
  if (!nrow(idx) || !"name" %in% names(idx)) {
    return(character())
  }
  score <- if ("mu_star" %in% names(idx)) idx$mu_star else if ("ST" %in% names(idx)) {
    idx$ST
  } else {
    NULL
  }
  if (is.null(score)) return(expand_names(parameters, idx$name))
  mx <- max(score, na.rm = TRUE)
  keep <- character()
  if (is.finite(mx) && mx > 0) {
    keep <- as.character(idx$name[is.finite(score) & score >= frac * mx])
  }
  if (!length(keep)) {
    ord <- order(score, decreasing = TRUE, na.last = TRUE)
    keep <- as.character(idx$name[[ord[[1]]]])
  }
  expand_names(parameters, keep)
}

#' Attach responses and compute Morris / Sobol indices
#'
#' @param sa_obj From [sa_design()].
#' @param responses One numeric value per row of `sa_obj$X`.
#' @return The updated SA object.
#' @export
complete_sa_analysis <- function(sa_obj, responses) {
  if (is.null(sa_obj$X)) stop("sa_obj is missing the design matrix (X)", call. = FALSE)
  responses <- as.numeric(responses)
  if (length(responses) != nrow(sa_obj$X)) {
    stop("`responses` must contain ", nrow(sa_obj$X), " values (one per sample row)",
         call. = FALSE)
  }
  sensitivity::tell(sa_obj, responses)
}

#' @noRd
.sa_indices <- function(sa_obj) {
  if (inherits(sa_obj, "morris")) {
    ee <- sa_obj[["ee", exact = TRUE]]
    if (is.null(ee)) {
      stop("Morris analysis has no elementary effects; call complete_sa_analysis() first",
           call. = FALSE)
    }
    if (length(dim(ee)) == 3) {
      stop("plot_sa() does not support multivariate Morris results", call. = FALSE)
    }
    param_names <- colnames(ee)
    if (is.null(param_names)) param_names <- paste0("X", seq_len(ncol(ee)))
    indices <- data.frame(
      name  = param_names,
      mu      = colMeans(ee),
      mu_star = colMeans(abs(ee)),
      sigma   = apply(ee, 2, stats::sd),
      row.names = NULL,
      stringsAsFactors = FALSE
    )
    return(list(method = "morris", indices = indices))
  }

  if (inherits(sa_obj, "sobol") || inherits(sa_obj, "soboljansen")) {
    S <- sa_obj[["S", exact = TRUE]]
    T_idx <- sa_obj[["T", exact = TRUE]]
    if (is.null(S) || is.null(T_idx)) {
      stop("Sobol analysis has no indices; call complete_sa_analysis() first",
           call. = FALSE)
    }
    param_names <- rownames(S)
    if (is.null(param_names)) param_names <- paste0("X", seq_len(nrow(S)))
    indices <- data.frame(
      name = param_names,
      S      = S[["original"]],
      ST     = T_idx[["original"]],
      row.names = NULL,
      stringsAsFactors = FALSE
    )
    if ("min. c.i." %in% names(S) && "max. c.i." %in% names(S)) {
      indices$S_min <- S[["min. c.i."]]
      indices$S_max <- S[["max. c.i."]]
    }
    if ("min. c.i." %in% names(T_idx) && "max. c.i." %in% names(T_idx)) {
      indices$ST_min <- T_idx[["min. c.i."]]
      indices$ST_max <- T_idx[["max. c.i."]]
    }
    return(list(method = "sobol", indices = indices))
  }

  stop("plot_sa() expects a morris or sobol object from complete_sa_analysis(), ",
       "not an object of class ", paste(class(sa_obj), collapse = "/"),
       call. = FALSE)
}

#' Plot Morris or Sobol indices
#'
#' @param sa_obj From [complete_sa_analysis()].
#' @param type Morris: `"effects"` (default) or `"bar"`. Sobol: `"bar"` or
#'   `"ranked"`.
#' @param main Plot title.
#' @param ... Unused.
#' @return Invisibly, a data.frame of indices.
#' @export
plot_sa <- function(sa_obj, type = NULL, main = NULL, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required for plot_sa(); please install it", call. = FALSE)
  }
  if (!requireNamespace("ggrepel", quietly = TRUE)) {
    stop("ggrepel is required for plot_sa(); please install it", call. = FALSE)
  }
  parsed <- .sa_indices(sa_obj)
  idx <- parsed$indices
  if (parsed$method == "morris") {
    type <- if (is.null(type)) "effects" else type
    type <- match.arg(type, c("effects", "bar"))
    p <- .plot_sa_morris(idx, type = type, main = main)
  } else {
    type <- if (is.null(type)) "bar" else type
    type <- match.arg(type, c("bar", "ranked"))
    p <- .plot_sa_sobol(idx, type = type, main = main)
  }
  print(p)
  invisible(idx)
}

.sa_ggplot_theme <- function() {
  ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(legend.position = "bottom")
}

#' @noRd
.plot_sa_morris <- function(idx, type, main) {
  if (type == "effects") {
    if (is.null(main)) main <- "Morris elementary effects"
    ggplot2::ggplot(idx, ggplot2::aes(x = mu_star, y = sigma, label = name)) +
      ggplot2::geom_point(size = 2.8, colour = "#1b5e40") +
      ggrepel::geom_text_repel(
        size = 3.4, colour = "#1b5e40", min.segment.length = 0,
        box.padding = 0.35, point.padding = 0.25, max.overlaps = Inf, seed = 1
      ) +
      ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.05, 0.18))) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.08, 0.08))) +
      ggplot2::labs(title = main, x = expression(mu^"*"), y = expression(sigma)) +
      .sa_ggplot_theme() +
      ggplot2::theme(legend.position = "none")
  } else {
    if (is.null(main)) main <- expression("Morris " * mu^"*")
    ord <- order(idx$mu_star, decreasing = FALSE)
    df <- idx
    df$name <- factor(df$name, levels = df$name[ord])
    ggplot2::ggplot(df, ggplot2::aes(x = mu_star, y = name)) +
      ggplot2::geom_col(fill = "#1b5e40", width = 0.7) +
      ggplot2::labs(title = main, x = expression(mu^"*"), y = NULL) +
      .sa_ggplot_theme() +
      ggplot2::theme(legend.position = "none")
  }
}

#' @noRd
.plot_sa_sobol <- function(idx, type, main) {
  if (identical(type, "ranked")) {
    if (is.null(main)) main <- "Sobol total indices"
    df <- idx
    df$name <- factor(df$name, levels = df$name[order(df$ST, decreasing = FALSE)])
    return(
      ggplot2::ggplot(df, ggplot2::aes(x = ST, y = name)) +
        ggplot2::geom_col(fill = "#1b5e40", width = 0.7) +
        ggplot2::labs(title = main, x = "Total-order index (ST)", y = NULL) +
        .sa_ggplot_theme() +
        ggplot2::theme(legend.position = "none")
    )
  }
  if (is.null(main)) main <- "Sobol indices"
  long <- rbind(
    data.frame(
      name = idx$name, index = "First-order (S)", value = idx$S,
      ymin = if ("S_min" %in% names(idx)) idx$S_min else NA_real_,
      ymax = if ("S_max" %in% names(idx)) idx$S_max else NA_real_,
      stringsAsFactors = FALSE
    ),
    data.frame(
      name = idx$name, index = "Total (ST)", value = idx$ST,
      ymin = if ("ST_min" %in% names(idx)) idx$ST_min else NA_real_,
      ymax = if ("ST_max" %in% names(idx)) idx$ST_max else NA_real_,
      stringsAsFactors = FALSE
    )
  )
  p <- ggplot2::ggplot(long, ggplot2::aes(x = name, y = value, fill = index)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.7) +
    ggplot2::scale_fill_manual(values = c("First-order (S)" = "#4a9b6e",
                                         "Total (ST)" = "#1b5e40")) +
    ggplot2::labs(title = main, x = NULL, y = "Sensitivity index", fill = NULL) +
    ggplot2::coord_cartesian(ylim = c(0, NA)) +
    .sa_ggplot_theme()
  has_ci <- any(is.finite(long$ymin) & is.finite(long$ymax))
  if (has_ci) {
    p <- p + ggplot2::geom_errorbar(
      ggplot2::aes(ymin = ymin, ymax = ymax),
      position = ggplot2::position_dodge(width = 0.8),
      width = 0.2, na.rm = TRUE
    )
  }
  p
}
