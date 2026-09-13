#' Assemble a stand-alone plugin working directory
#'
#' The FAO executable is started with no arguments and looks for
#' `LIST/ListProjects.txt` in the current directory. This helper copies a
#' `SIMUL/` tree, copies one or more `.PRO`/`.PRM` files into `LIST/`,
#' writes `ListProjects.txt`, and creates empty `OUTP/`.
#'
#' Crop, soil, and climate files are **not** copied: the project file
#' already stores their directories. Point those paths at files that exist
#' on disk (or copy them yourself into `path`).
#'
#' @param path Character scalar. Directory to create (or reuse).
#' @param projects Character vector of `.PRO` / `.PRM` file paths to copy
#'   into `LIST/`.
#' @param simul_dir Character scalar path to a `SIMUL` folder, or `NULL`
#'   to use [get_simul_dir()].
#' @param overwrite If `TRUE` (default), replace `LIST/`, `SIMUL/`, and
#'   `OUTP/` inside `path`.
#' @param daily_results Integer vector of DailyResults.SIM codes to write
#'   into the copied `SIMUL/` (see [write_daily_results_sim()]). `NULL`
#'   leaves the copied file unchanged. Default writes water balance (1),
#'   crop (2), and climate (7) daily files.
#' @param relativize_to Directory that AquaCrop prepends onto every path
#'   in the project file (the folder containing `AquaCrop73.exe`). The
#'   v7.3 Python plugin concatenates that folder with the directory lines
#'   in `.PRM`/`.PRO` files, so absolute Windows paths break. Pass `FALSE`
#'   to leave paths unchanged. Default: directory of [get_aquacrop_path()]
#'   when the executable can be found.
#' @param program_parameters If `TRUE` (default), also copy a sibling
#'   `.PP1` / `.PPn` when one sits next to a project being copied.
#'
#' @return `path`, invisibly.
#' @export
prepare_plugin_workdir <- function(path, projects, simul_dir = NULL,
                                   overwrite = TRUE,
                                   daily_results = c(1L, 2L, 7L),
                                   relativize_to = NULL,
                                   program_parameters = TRUE) {
  if (!is.character(path) || length(path) != 1L || !nzchar(path)) {
    stop("`path` must be a non-empty character scalar", call. = FALSE)
  }
  if (!length(projects)) {
    stop("`projects` must list at least one .PRO or .PRM file", call. = FALSE)
  }
  if (is.null(simul_dir) || !nzchar(simul_dir)) {
    simul_dir <- get_simul_dir()
  }
  if (!dir.exists(simul_dir)) {
    stop("SIMUL directory not found: ", simul_dir, call. = FALSE)
  }
  missing <- projects[!file.exists(projects)]
  if (length(missing)) {
    stop("Project file(s) not found: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }

  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  list_dir <- file.path(path, "LIST")
  simul_dest <- file.path(path, "SIMUL")
  outp_dir <- file.path(path, "OUTP")

  if (overwrite) {
    if (dir.exists(list_dir)) unlink(list_dir, recursive = TRUE)
    if (dir.exists(simul_dest)) unlink(simul_dest, recursive = TRUE)
    if (dir.exists(outp_dir)) unlink(outp_dir, recursive = TRUE)
  }

  dir.create(list_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(outp_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(simul_dest, recursive = TRUE, showWarnings = FALSE)
  simul_files <- list.files(simul_dir, all.files = TRUE, no.. = TRUE)
  if (length(simul_files)) {
    copied <- file.copy(
      file.path(simul_dir, simul_files),
      simul_dest,
      recursive = TRUE
    )
    if (!all(copied)) {
      stop("Failed to copy some SIMUL files into ", simul_dest, call. = FALSE)
    }
  }

  if (isFALSE(relativize_to)) {
    relativize_to <- NULL
  } else if (is.null(relativize_to)) {
    exe <- tryCatch(.find_aquacrop_exe(error = FALSE), error = function(e) "")
    if (nzchar(exe)) relativize_to <- dirname(exe)
  }

  dest_names <- character(length(projects))
  for (i in seq_along(projects)) {
    dest_names[[i]] <- basename(projects[[i]])
    dest <- file.path(list_dir, dest_names[[i]])
    if (!is.null(relativize_to) && nzchar(relativize_to)) {
      relativize_project_paths(projects[[i]], dest, relativize_to)
    } else {
      file.copy(projects[[i]], dest, overwrite = TRUE)
    }
    if (isTRUE(program_parameters)) {
      sib <- .ac_pp_sibling(projects[[i]])
      if (!is.na(sib) && file.exists(sib)) {
        file.copy(sib, file.path(list_dir, basename(sib)), overwrite = TRUE)
      }
    }
  }
  writeLines(dest_names, file.path(list_dir, "ListProjects.txt"))
  if (!is.null(daily_results)) {
    write_daily_results_sim(file.path(simul_dest, "DailyResults.SIM"),
                            codes = daily_results)
  }
  invisible(normalizePath(path, winslash = "/", mustWork = TRUE))
}

#' Rewrite directory lines in a `.PRM`/`.PRO` so they are relative to `root`
#'
#' AquaCrop 7.3's stand-alone plugin joins the executable directory with
#' each project directory line. Absolute paths such as `C:\\...\\DATA\\`
#' become invalid (`exe_dir\\C:\\...`). This helper rewrites those lines
#' to `..\\GUI_AC7.3\\DATA\\`-style relatives.
#'
#' @param from_file Source project file.
#' @param to_file Destination (may be the same as `from_file`).
#' @param root Directory the plugin prefixes (usually the exe folder).
#' @return `to_file`, invisibly.
#' @export
relativize_project_paths <- function(from_file, to_file, root) {
  if (!file.exists(from_file)) {
    stop("Project file not found: ", from_file, call. = FALSE)
  }
  if (!dir.exists(root)) {
    stop("`root` directory not found: ", root, call. = FALSE)
  }
  lines <- readLines(from_file, warn = FALSE)
  out <- vapply(lines, function(line) {
    trimmed <- trimws(line)
    if (!grepl("^[A-Za-z]:[/\\\\]", trimmed)) return(line)
    trail <- grepl("[/\\\\]$", trimmed)
    rel <- .ac_path_rel(trimmed, root)
    rel <- gsub("/", "\\", rel, fixed = TRUE)
    if (trail && !grepl("\\\\$", rel)) rel <- paste0(rel, "\\")
    if (identical(rel, ".")) rel <- if (trail) ".\\" else "."
    sub(trimmed, rel, line, fixed = TRUE)
  }, character(1), USE.NAMES = FALSE)
  dir.create(dirname(to_file), recursive = TRUE, showWarnings = FALSE)
  writeLines(out, to_file)
  invisible(to_file)
}

#' @noRd
.ac_path_rel <- function(path, root) {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  p <- strsplit(path, "/", fixed = TRUE)[[1]]
  r <- strsplit(root, "/", fixed = TRUE)[[1]]
  p <- p[nzchar(p)]
  r <- r[nzchar(r)]
  n <- min(length(p), length(r))
  i <- 0L
  while (i < n && identical(tolower(p[i + 1L]), tolower(r[i + 1L]))) {
    i <- i + 1L
  }
  if (i == 0L) {
    return(path)
  }
  up <- length(r) - i
  rest <- if (i < length(p)) p[(i + 1L):length(p)] else character()
  parts <- c(rep("..", up), rest)
  if (!length(parts)) return(".")
  paste(parts, collapse = "/")
}

#' Write `SIMUL/DailyResults.SIM` with selected daily output codes
#'
#' The stand-alone plugin only writes daily `.OUT` files when this file
#' lists the wanted codes (FAO: 1 water balance, 2 crop, 3–6 soil profiles,
#' 7 climate, 8 irrigation).
#'
#' @param path Destination path (typically `.../SIMUL/DailyResults.SIM`).
#' @param codes Integer vector in `1:8`.
#' @return `path`, invisibly.
#' @export
write_daily_results_sim <- function(path, codes = c(1L, 2L, 7L)) {
  labels <- c(
    "1" = "Various parameters of the soil water balance",
    "2" = "Crop development and production",
    "3" = "Soil water content in the soil profile and root zone",
    "4" = "Soil salinity in the soil profile and root zone",
    "5" = "Soil water content at various depths of the soil profile",
    "6" = "Soil salinity at various depths of the soil profile",
    "7" = "Climate input parameters",
    "8" = "Irrigation events and intervals"
  )
  codes <- as.integer(unique(codes))
  bad <- setdiff(codes, 1:8)
  if (length(bad)) {
    stop("`codes` must be in 1:8; unknown: ", paste(bad, collapse = ", "),
         call. = FALSE)
  }
  lines <- sprintf(" %d : %s", codes, labels[as.character(codes)])
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, path)
  invisible(path)
}
