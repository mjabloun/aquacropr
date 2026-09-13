#' FAO default program parameters (Table 2.19a / `.PP1` / `.PPn`).
#'
#' @return Named list of 25 values.
#' @noRd
.ac_pp_defaults <- function() {
  list(
    evap_decline       = 4L,
    ke_max             = 1.10,
    hi_cc_threshold    = 5L,
    root_expand_start  = 70L,
    root_expand_max    = 5.00,
    root_stress_shape  = -6L,
    germination_taw    = 20L,
    p_eto_adj          = 1.0,
    aeration_days      = 3L,
    senescence_exp     = 1.00,
    psen_decrease      = 12L,
    topsoil_cm         = 10L,
    evap_extract_cm    = 30L,
    cn_depth_m         = 0.30,
    cn_amc             = 1L,
    salt_diffusion     = 20L,
    salt_solubility    = 100L,
    capillary_shape    = 16L,
    default_tmin       = 12.0,
    default_tmax       = 28.0,
    gdd_method         = 3L,
    rain_procedure     = 1L,
    rain_effective_pct = 70L,
    rain_showers       = 2L,
    rain_evap_param    = 5L
  )
}

#' @noRd
.ac_pp_comments <- function() {
  c(
    evap_decline       = "Evaporation decline factor for stage II",
    ke_max             = "Ke(x) Soil evaporation coefficient for fully wet and non-shaded soil surface",
    hi_cc_threshold    = "Threshold for green CC below which HI can no longer increase (% cover)",
    root_expand_start  = "Starting depth of root zone expansion curve (% of Zmin)",
    root_expand_max    = "Maximum allowable root zone expansion (fixed at 5 cm/day)",
    root_stress_shape  = "Shape factor for effect water stress on root zone expansion",
    germination_taw    = "Required soil water content in top soil for germination (% TAW)",
    p_eto_adj          = "Adjustment factor for FAO-adjustment soil water depletion (p) by ETo",
    aeration_days      = "Number of days after which deficient aeration is fully effective",
    senescence_exp     = "Exponent of senescence factor adjusting drop in photosynthetic activity of dying crop",
    psen_decrease      = "Decrease of p(sen) once early canopy senescence is triggered (% of p(sen))",
    topsoil_cm         = "Thickness top soil (cm) in which soil water depletion has to be determined",
    evap_extract_cm    = "Depth [cm] of soil profile affected by water extraction by soil evaporation",
    cn_depth_m         = "Considered depth (m) of soil profile for calculation of mean soil water content for CN adjustment",
    cn_amc             = "CN is adjusted to Antecedent Moisture Class",
    salt_diffusion     = "Salt diffusion factor (capacity for salt diffusion in micro pores) [%]",
    salt_solubility    = "Salt solubility [g/liter]",
    capillary_shape    = "Shape factor for effect of soil water content gradient on capillary rise",
    default_tmin       = "Default minimum temperature (C) if no temperature file is specified",
    default_tmax       = "Default maximum temperature (C) if no temperature file is specified",
    gdd_method         = "Default method for the calculation of growing degree days",
    rain_procedure     = "Daily rainfall is estimated by USDA-SCS procedure (when input is 10-day/monthly rainfall)",
    rain_effective_pct = "Percentage of effective rainfall (when input is 10-day/monthly rainfall)",
    rain_showers       = "Number of showers in a decade for run-off estimate (when input is 10-day/monthly rainfall)",
    rain_evap_param    = "Parameter for reduction of soil evaporation (when input is 10-day/monthly rainfall)"
  )
}

#' @noRd
.ac_pp_formats <- function() {
  c(
    evap_decline       = "%7d",
    ke_max             = "%10.2f",
    hi_cc_threshold    = "%7d",
    root_expand_start  = "%7d",
    root_expand_max    = "%10.2f",
    root_stress_shape  = "%7d",
    germination_taw    = "%7d",
    p_eto_adj          = "%11.1f",
    aeration_days      = "%7d",
    senescence_exp     = "%10.2f",
    psen_decrease      = "%7d",
    topsoil_cm         = "%7d",
    evap_extract_cm    = "%7d",
    cn_depth_m         = "%10.2f",
    cn_amc             = "%7d",
    salt_diffusion     = "%7d",
    salt_solubility    = "%7d",
    capillary_shape    = "%7d",
    default_tmin       = "%11.1f",
    default_tmax       = "%11.1f",
    gdd_method         = "%7d",
    rain_procedure     = "%7d",
    rain_effective_pct = "%7d",
    rain_showers       = "%7d",
    rain_evap_param    = "%7d"
  )
}

#' Sibling `.PP1` / `.PPn` path for a project (or the file itself if already PP).
#'
#' @noRd
.ac_pp_sibling <- function(path) {
  ext <- .ac_file_ext(path)
  dir <- dirname(path)
  stem <- sub("\\.[^.]+$", "", basename(path))
  if (identical(ext, "pro")) return(file.path(dir, paste0(stem, ".PP1")))
  if (identical(ext, "prm")) return(file.path(dir, paste0(stem, ".PPn")))
  if (ext %in% c("pp1", "ppn")) return(path)
  NA_character_
}

#' @noRd
.ac_pp_path <- function(path) {
  dest <- .ac_pp_sibling(path)
  if (is.na(dest) || !nzchar(dest)) {
    stop("`path` must be .PRO, .PRM, .PP1, or .PPn", call. = FALSE)
  }
  dest
}

#' Write an AquaCrop program-parameter file (`.PP1` or `.PPn`)
#'
#' FAO loads this file by **basename** when a project is opened: `field.PRO`
#' looks for `field.PP1`, `field.PRM` looks for `field.PPn`, in the same
#' folder. It is **not** named inside the project file. If the sibling is
#' missing, AquaCrop uses the Table 2.19a defaults (the same numbers this
#' helper writes unless you override them).
#'
#' @param path Destination `.PP1` / `.PPn`, or the matching `.PRO` / `.PRM`
#'   (the extension is then derived).
#' @param ... Named overrides of the 25 FAO program parameters (see Details).
#' @param params Optional named list of overrides (merged with `...`).
#'
#' @details Parameter names and FAO defaults:
#' \itemize{
#'   \item `evap_decline` (4), `ke_max` (1.10), `hi_cc_threshold` (5)
#'   \item `root_expand_start` (70), `root_expand_max` (5.00), `root_stress_shape` (-6)
#'   \item `germination_taw` (20), `p_eto_adj` (1.0), `aeration_days` (3)
#'   \item `senescence_exp` (1.00), `psen_decrease` (12), `topsoil_cm` (10)
#'   \item `evap_extract_cm` (30), `cn_depth_m` (0.30), `cn_amc` (1)
#'   \item `salt_diffusion` (20), `salt_solubility` (100), `capillary_shape` (16)
#'   \item `default_tmin` (12.0), `default_tmax` (28.0), `gdd_method` (3)
#'   \item `rain_procedure` (1), `rain_effective_pct` (70), `rain_showers` (2),
#'     `rain_evap_param` (5)
#' }
#'
#' @return The path actually written, invisibly.
#' @export
#' @seealso [write_project()], [run_aquacrop()]
write_pp <- function(path, ..., params = NULL) {
  if (!is.character(path) || length(path) != 1L || !nzchar(path)) {
    stop("`path` must be a non-empty character scalar", call. = FALSE)
  }
  dest <- .ac_pp_path(path)
  values <- .ac_pp_defaults()
  dots <- list(...)
  if (!is.null(params)) {
    if (!is.list(params) || is.null(names(params)) || any(!nzchar(names(params)))) {
      stop("`params` must be a named list", call. = FALSE)
    }
    bad <- setdiff(names(params), names(values))
    if (length(bad)) {
      stop("Unknown program parameter(s): ", paste(bad, collapse = ", "),
           call. = FALSE)
    }
    values[names(params)] <- params
  }
  if (length(dots)) {
    if (is.null(names(dots)) || any(!nzchar(names(dots)))) {
      stop("Overrides in `...` must be named", call. = FALSE)
    }
    bad <- setdiff(names(dots), names(values))
    if (length(bad)) {
      stop("Unknown program parameter(s): ", paste(bad, collapse = ", "),
           call. = FALSE)
    }
    values[names(dots)] <- dots
  }
  comments <- .ac_pp_comments()
  fmts <- .ac_pp_formats()
  lines <- vapply(names(values), function(nm) {
    sprintf(paste0(fmts[[nm]], "         : %s"), values[[nm]], comments[[nm]])
  }, character(1))
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, dest, useBytes = TRUE)
  invisible(normalizePath(dest, winslash = "/", mustWork = TRUE))
}

#' Program-parameter files next to projects in `working_dir` / `LIST/`.
#'
#' @noRd
.list_program_parameter_files <- function(working_dir) {
  dirs <- unique(c(working_dir, file.path(working_dir, "LIST")))
  hits <- unlist(lapply(dirs, function(d) {
    if (!dir.exists(d)) return(character())
    list.files(d, pattern = "\\.(PP1|PPn)$", ignore.case = TRUE,
               full.names = TRUE)
  }), use.names = FALSE)
  hits[!grepl("\\.aquacropr-off$", hits, ignore.case = TRUE)]
}

.PP_STASH <- new.env(parent = emptyenv())
.PP_STASH$depth <- 0L
.PP_STASH$files <- character()

#' Put leftover `.PP1`/`.PPn` stashes back.
#'
#' @noRd
.restore_program_parameters <- function(working_dir) {
  dirs <- unique(c(working_dir, file.path(working_dir, "LIST")))
  offs <- unlist(lapply(dirs, function(d) {
    if (!dir.exists(d)) return(character())
    list.files(d, pattern = "\\.(PP1|PPn)\\.aquacropr-off$",
               ignore.case = TRUE, full.names = TRUE)
  }), use.names = FALSE)
  for (off in offs) {
    orig <- sub("\\.aquacropr-off$", "", off, ignore.case = TRUE)
    if (!file.exists(orig) && file.exists(off)) {
      file.rename(off, orig)
    }
  }
  invisible(NULL)
}

#' Temporarily hide `.PP1`/`.PPn` so AquaCrop uses Table 2.19a defaults.
#'
#' @noRd
.stash_program_parameters <- function(working_dir) {
  if (.PP_STASH$depth == 0L) {
    .restore_program_parameters(working_dir)
    hits <- .list_program_parameter_files(working_dir)
    moved <- character()
    for (f in hits) {
      off <- paste0(f, ".aquacropr-off")
      if (file.rename(f, off)) moved <- c(moved, f)
    }
    .PP_STASH$files <- moved
  }
  .PP_STASH$depth <- .PP_STASH$depth + 1L
  .PP_STASH$dir <- working_dir
  function() {
    if (.PP_STASH$depth <= 0L) return(invisible(NULL))
    .PP_STASH$depth <- .PP_STASH$depth - 1L
    if (.PP_STASH$depth == 0L) {
      .restore_program_parameters(.PP_STASH$dir)
      .PP_STASH$files <- character()
    }
    invisible(NULL)
  }
}
