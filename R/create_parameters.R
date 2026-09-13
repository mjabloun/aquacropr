#' Create a starter parameters YAML file
#'
#' Writes one stub entry per name, with `REPLACE_ME.CRO` paths and `# TODO`
#' bounds. Fill those in, then [read_parameters()] / [validate_parameters()].
#'
#' @param param_names Character vector of parameter names.
#' @param path Optional file to write. If `NULL`, return the YAML text.
#' @return `path` invisibly, or the YAML as a character scalar.
#' @export
#' @examples
#' cat(create_parameters(c("CCx", "WP")))
create_parameters <- function(param_names, path = NULL) {
  n <- length(param_names)
  if (n == 0L) stop("`param_names` must have at least one name", call. = FALSE)
  if (anyDuplicated(param_names) > 0) {
    stop("`param_names` must not contain duplicate names: ",
         paste(unique(param_names[duplicated(param_names)]), collapse = ", "),
         call. = FALSE)
  }
  entries <- vapply(param_names, function(nm) {
    sprintf(
"  - name: %s
    from_file: REPLACE_ME.CRO   # TODO: template containing {%s}
    to_file: REPLACE_ME.CRO     # TODO: file AquaCrop will read
    default: 0                  # TODO: default/starting value
    min: 0                      # TODO: lower bound
    max: 1                      # TODO: upper bound
",
      nm, nm)
  }, character(1))
  yaml_txt <- paste0("parameters:\n", paste0(entries, collapse = ""))
  if (!is.null(path)) {
    writeLines(yaml_txt, path)
    return(invisible(path))
  }
  yaml_txt
}
