#' Placeholder delimiters used by [render_one_template()]
#'
#' Single braces (`{name}`). Unlike Daisy, AquaCrop text files have no
#' `{...}` syntax of their own, so the extra brace daisyr uses is not needed.
#' @keywords internal
.placeholder_open <- "{"
#' @rdname dot-placeholder_open
#' @keywords internal
.placeholder_close <- "}"

#' Render a single text template given raw substitutions
#'
#' @param subs Named list/vector of placeholder -> replacement text.
#' @param from_file Template path.
#' @param to_file Destination path.
#' @return Invisibly, the rendered text.
#' @keywords internal
render_one_template <- function(subs, from_file, to_file) {
  txt <- readLines(from_file, warn = FALSE)
  rendered <- as.character(glue::glue_data(
    subs, paste(txt, collapse = "\n"),
    .open = .placeholder_open, .close = .placeholder_close
  ))
  dir.create(dirname(to_file), recursive = TRUE, showWarnings = FALSE)
  writeLines(rendered, to_file)
  invisible(rendered)
}

#' @keywords internal
build_substitutions <- function(parameters, values) {
  parameters <- as_parameters(parameters)
  needed <- parameter_names(parameters)
  missing <- setdiff(needed, names(values))
  if (length(missing) > 0) {
    stop("Missing value(s) for parameter(s): ", paste(missing, collapse = ", "))
  }

  groups <- list()
  add_sub <- function(from_file, to_file, placeholder, text) {
    key <- paste(from_file, "->", to_file)
    if (is.null(groups[[key]])) {
      groups[[key]] <<- list(from_file = from_file, to_file = to_file, subs = list())
    }
    groups[[key]]$subs[[placeholder]] <<- text
  }

  direct <- parameters$parameters
  for (i in seq_len(nrow(direct))) {
    add_sub(
      direct$from_file[i], direct$to_file[i], direct$name[i],
      format(values[[direct$name[i]]])
    )
  }
  groups
}

#' Render every template implied by a parameters object
#'
#' @param parameters An `aquacropr_parameters` object from [as_parameters()]
#'   or [read_parameters()].
#' @param values Named numeric vector covering [parameter_names()].
#' @param template_dir Directory `from_file` paths are relative to.
#' @param output_dir Directory `to_file` paths are written relative to.
#'
#' @return Invisibly, the `to_file` paths written.
#' @export
render_templates <- function(parameters, values, template_dir = ".", output_dir = ".") {
  values <- unlist(values)
  groups <- build_substitutions(parameters, values)

  written <- vapply(groups, function(g) {
    from_path <- file.path(template_dir, g$from_file)
    to_path <- file.path(output_dir, g$to_file)
    render_one_template(g$subs, from_path, to_path)
    to_path
  }, character(1))

  invisible(unname(written))
}
