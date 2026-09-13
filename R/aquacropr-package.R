#' aquacropr: Drive the FAO AquaCrop stand-alone plugin from R
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom data.table :=
#' @importFrom utils globalVariables
## usethis namespace: end
NULL

utils::globalVariables(c(
  "name", "from_file", "to_file",
  "obs_col", "label", "weight",
  "obs", "sim", "run_id",
  "mu_star", "sigma", "name", "ST", "value", "index",
  "ymin", "ymax"
))
