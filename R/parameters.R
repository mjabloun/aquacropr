#' Build or coerce an AquaCrop parameters object
#'
#' A parameters object lists the free scalars used in calibration and
#' sensitivity analysis, and which template files hold `{name}`
#' placeholders. Supply a YAML path, a data.frame, or a list of rows with
#' `name`, `default`, `min`, `max`, `from_file`, and `to_file`.
#'
#' @param x Path to a YAML file, a data.frame, a list of parameter rows,
#'   or an existing `aquacropr_parameters` object.
#' @return An object of class `aquacropr_parameters`.
#' @export
#' @seealso [validate_parameters()], [render_templates()], [calibrate()]
as_parameters <- function(x) {
  if (inherits(x, "aquacropr_parameters")) return(x)
  if (is.character(x) && length(x) == 1L) return(read_parameters(x))
  params <- .parameters_table(x)
  out <- list(parameters = params)
  class(out) <- "aquacropr_parameters"
  .validate_parameters_structure(out)
  out
}

#' Read parameters from a YAML file
#'
#' The file should have a top-level `parameters:` list. Each entry needs
#' `name`, `default`, `min`, `max`, `from_file`, and `to_file`.
#'
#' @param path Character scalar path.
#' @return An `aquacropr_parameters` object.
#' @export
read_parameters <- function(path) {
  if (!file.exists(path)) stop("Parameters file not found: ", path, call. = FALSE)
  raw <- yaml::read_yaml(path)
  rows <- raw$parameters
  if (is.null(rows) || !length(rows)) {
    stop("YAML must define at least one entry under `parameters`", call. = FALSE)
  }
  as_parameters(rows)
}

#' @noRd
.parameters_table <- function(x) {
  if (is.data.frame(x)) {
    rows <- lapply(seq_len(nrow(x)), function(i) as.list(x[i, , drop = FALSE]))
  } else if (is.list(x)) {
    rows <- x
  } else {
    stop("`parameters` must be a data.frame, a list of rows, a YAML path, ",
         "or an aquacropr_parameters object", call. = FALSE)
  }
  params <- data.table::rbindlist(lapply(rows, function(p) {
    required <- c("name", "default", "min", "max")
    missing <- setdiff(required, names(p))
    if (length(missing)) {
      stop("Parameter entry is missing required field(s): ",
           paste(missing, collapse = ", "), call. = FALSE)
    }
    data.table::data.table(
      name      = as.character(p$name),
      default   = as.numeric(p$default),
      min       = as.numeric(p$min),
      max       = as.numeric(p$max),
      from_file = if (is.null(p$from_file)) NA_character_ else as.character(p$from_file),
      to_file   = if (is.null(p$to_file)) NA_character_ else as.character(p$to_file)
    )
  }), fill = TRUE)
  if (anyDuplicated(params$name) > 0) {
    stop("Duplicate parameter name(s): ",
         paste(unique(params$name[duplicated(params$name)]), collapse = ", "),
         call. = FALSE)
  }
  params
}

#' @noRd
.validate_parameters_structure <- function(parameters) {
  params <- parameters$parameters
  bad <- params[is.na(from_file) | is.na(to_file)]
  if (nrow(bad) > 0) {
    stop("Parameter(s) must have both from_file and to_file: ",
         paste(bad$name, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

#' Check that every `{placeholder}` exists once in its template
#'
#' @param parameters An `aquacropr_parameters` object.
#' @param template_dir Directory that `from_file` paths are relative to.
#' @return Invisibly `TRUE`.
#' @export
validate_parameters <- function(parameters, template_dir = ".") {
  parameters <- as_parameters(parameters)
  problems <- character(0)
  check_placeholder <- function(from_file, placeholder, label) {
    full_path <- file.path(template_dir, from_file)
    if (!file.exists(full_path)) {
      problems <<- c(problems, sprintf("%s: template file not found: %s",
                                       label, full_path))
      return(invisible())
    }
    txt <- paste(readLines(full_path, warn = FALSE), collapse = "\n")
    pattern <- paste0("\\{", placeholder, "\\}")
    n_hits <- lengths(regmatches(txt, gregexpr(pattern, txt)))
    if (n_hits == 0) {
      problems <<- c(problems, sprintf("%s: placeholder {%s} not found in %s",
                                        label, placeholder, from_file))
    } else if (n_hits > 1) {
      problems <<- c(problems, sprintf(
        "%s: placeholder {%s} appears %d times in %s (expected exactly once)",
        label, placeholder, n_hits, from_file
      ))
    }
  }
  direct <- parameters$parameters
  for (i in seq_len(nrow(direct))) {
    check_placeholder(direct$from_file[i], direct$name[i],
                      sprintf("parameter '%s'", direct$name[i]))
  }
  if (length(problems)) {
    stop("Parameter validation failed:\n  - ", paste(problems, collapse = "\n  - "),
         call. = FALSE)
  }
  invisible(TRUE)
}

#' Names of every free parameter
#'
#' @param parameters An `aquacropr_parameters` object.
#' @return Character vector.
#' @export
parameter_names <- function(parameters) {
  as_parameters(parameters)$parameters$name
}

#' Default values for every parameter
#'
#' @param parameters An `aquacropr_parameters` object.
#' @return Named numeric vector.
#' @export
parameter_defaults <- function(parameters) {
  parameters <- as_parameters(parameters)
  stats::setNames(as.numeric(parameters$parameters$default),
                  parameters$parameters$name)
}

#' Restrict a parameter selection to known names
#'
#' @param parameters An `aquacropr_parameters` object.
#' @param names Character vector of names, or `NULL` for all.
#' @return Character vector, in parameters order.
#' @export
expand_names <- function(parameters, names = NULL) {
  all_n <- parameter_names(parameters)
  if (is.null(names) || !length(names)) return(all_n)
  names <- unique(as.character(names))
  names <- names[nzchar(names)]
  missing <- setdiff(names, all_n)
  if (length(missing)) {
    stop("Unknown parameter(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }
  all_n[all_n %in% names]
}

#' Full parameter vector, pinning unspecified names at defaults
#'
#' @param parameters An `aquacropr_parameters` object.
#' @param p Numeric vector of values for `names`.
#' @param names Names to overwrite. `NULL` means every parameter.
#' @return Named numeric vector covering [parameter_names()].
#' @export
fill_values <- function(parameters, p, names = NULL) {
  names <- expand_names(parameters, names)
  p <- as.numeric(p)
  if (length(p) != length(names)) {
    stop("`p` has length ", length(p), " but `names` has ", length(names),
         " name(s).", call. = FALSE)
  }
  values <- parameter_defaults(parameters)
  values[names] <- p
  values
}
