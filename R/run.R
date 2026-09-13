.AQUACROPR_EXE_OPTION <- "aquacropr.aquacrop_exe"
.AQUACROPR_SIMUL_OPTION <- "aquacropr.simul_dir"

#' Default locations searched by [get_aquacrop_path()].
#'
#' @noRd
.default_aquacrop_exe_candidates <- function() {
  c(
    "C:/JHI/Projects/Daisy/R-DAISY_v2/AquaCrop73_windows/AquaCrop73.exe",
    "C:/Program Files/AquaCrop/aquacrop.exe",
    "C:/Program Files/AquaCrop 7.3/aquacrop.exe",
    "C:/FAO/AQUACROP/aquacrop.exe",
    "C:/FAO/AQUACROP/AquaCrop73.exe",
    "C:/FAO/AQUACROP/ACsaV73.exe",
    "C:/FAO/AQUACROP/ACsaV72.exe",
    "C:/FAO/AQUACROP/ACsaV71.exe",
    unname(Sys.which("AquaCrop73.exe")),
    unname(Sys.which("aquacrop.exe")),
    unname(Sys.which("aquacrop")),
    unname(Sys.which("ACsaV73.exe")),
    unname(Sys.which("ACsaV72.exe")),
    unname(Sys.which("ACsaV71.exe"))
  )
}

#' Set the AquaCrop stand-alone executable used by [run_aquacrop()].
#'
#' Stores a session-wide path in `options(aquacropr.aquacrop_exe = ...)`.
#' Pass `NULL` to clear it (resolution then falls back to `AQUACROP_EXE`
#' and common install names; see [get_aquacrop_path()]).
#'
#' @param path Character scalar path to `aquacrop.exe` / `aquacrop` /
#'   `ACsaV73.exe`, or `NULL` to unset.
#'
#' @return The normalised path, invisibly; `NULL` when clearing.
#' @export
#' @seealso [get_aquacrop_path()], [run_aquacrop()]
set_aquacrop_path <- function(path) {
  if (is.null(path)) {
    options(aquacropr.aquacrop_exe = NULL)
    return(invisible(NULL))
  }
  if (!is.character(path) || length(path) != 1L || !nzchar(path)) {
    stop("`path` must be a non-empty character scalar, or NULL to unset",
         call. = FALSE)
  }
  if (!file.exists(path)) {
    stop("AquaCrop executable not found: ", path, call. = FALSE)
  }
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  options(aquacropr.aquacrop_exe = path)
  invisible(path)
}

#' Resolve the AquaCrop stand-alone executable path.
#'
#' Looks up, in order:
#' 1. `getOption("aquacropr.aquacrop_exe")` from [set_aquacrop_path()]
#' 2. the `AQUACROP_EXE` environment variable
#' 3. common Windows install paths and `Sys.which()`
#'
#' @return A single existing path (character scalar).
#' @export
#' @seealso [set_aquacrop_path()], [run_aquacrop()]
get_aquacrop_path <- function() {
  .find_aquacrop_exe(error = TRUE)
}

#' Set the default `SIMUL/` directory copied by [prepare_plugin_workdir()].
#'
#' @param path Character scalar path to a `SIMUL` folder, or `NULL` to unset.
#' @return The normalised path, invisibly; `NULL` when clearing.
#' @export
set_simul_dir <- function(path) {
  if (is.null(path)) {
    options(aquacropr.simul_dir = NULL)
    return(invisible(NULL))
  }
  if (!is.character(path) || length(path) != 1L || !nzchar(path)) {
    stop("`path` must be a non-empty character scalar, or NULL to unset",
         call. = FALSE)
  }
  if (!dir.exists(path)) {
    stop("SIMUL directory not found: ", path, call. = FALSE)
  }
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  options(aquacropr.simul_dir = path)
  invisible(path)
}

#' Resolve the default AquaCrop `SIMUL/` directory.
#'
#' Looks up `getOption("aquacropr.simul_dir")`, then `AQUACROP_SIMUL`,
#' then `SIMUL/` next to [get_aquacrop_path()] if that executable is found.
#'
#' @param error If `TRUE` (default), stop when nothing is found.
#' @return A directory path, or `""` when `error = FALSE` and none exists.
#' @export
get_simul_dir <- function(error = TRUE) {
  opt <- getOption(.AQUACROPR_SIMUL_OPTION, default = NULL)
  if (is.character(opt) && length(opt) == 1L && nzchar(opt)) {
    if (dir.exists(opt)) {
      return(normalizePath(opt, winslash = "/", mustWork = TRUE))
    }
    if (error) {
      stop("getOption(\"aquacropr.simul_dir\") is set but missing: ", opt,
           call. = FALSE)
    }
    return("")
  }
  env <- Sys.getenv("AQUACROP_SIMUL", unset = "")
  if (nzchar(env) && dir.exists(env)) {
    return(normalizePath(env, winslash = "/", mustWork = TRUE))
  }
  exe <- tryCatch(.find_aquacrop_exe(error = FALSE), error = function(e) "")
  if (nzchar(exe)) {
    sibling <- file.path(dirname(exe), "SIMUL")
    if (dir.exists(sibling)) {
      return(normalizePath(sibling, winslash = "/", mustWork = TRUE))
    }
  }
  if (error) {
    stop(
      "Could not find an AquaCrop SIMUL directory. Call set_simul_dir(), ",
      "set AQUACROP_SIMUL, or pass simul_dir= to prepare_plugin_workdir().",
      call. = FALSE
    )
  }
  ""
}

#' @noRd
.resolve_aquacrop_exe <- function(aquacrop_exe = NULL) {
  if (!is.null(aquacrop_exe) && nzchar(aquacrop_exe)) {
    return(aquacrop_exe)
  }
  get_aquacrop_path()
}

#' @noRd
.find_aquacrop_exe <- function(error = TRUE,
                              candidates = .default_aquacrop_exe_candidates()) {
  opt <- getOption(.AQUACROPR_EXE_OPTION, default = NULL)
  if (is.character(opt) && length(opt) == 1L && nzchar(opt)) {
    if (file.exists(opt)) {
      return(normalizePath(opt, winslash = "/", mustWork = TRUE))
    }
    if (error) {
      stop("getOption(\"aquacropr.aquacrop_exe\") is set but the file is missing: ",
           opt, "\nCall set_aquacrop_path() with a valid path.", call. = FALSE)
    }
    return("")
  }

  env <- Sys.getenv("AQUACROP_EXE", unset = "")
  hits <- c(env, candidates)
  hits <- hits[nzchar(hits) & file.exists(hits)]
  if (length(hits)) {
    return(normalizePath(hits[[1]], winslash = "/", mustWork = TRUE))
  }
  if (error) {
    stop(
      "Could not find the AquaCrop executable. Call set_aquacrop_path(), set ",
      "the AQUACROP_EXE environment variable, or pass aquacrop_exe= to ",
      "run_aquacrop().",
      call. = FALSE
    )
  }
  ""
}

#' Run the AquaCrop stand-alone plugin
#'
#' Thin wrapper around [system()]. The FAO plugin does **not** take a project
#' file on the command line: it must be launched from a working directory
#' that already contains `LIST/ListProjects.txt`, `LIST/*.PRM` (or `.PRO`),
#' `SIMUL/`, and `OUTP/` (see [prepare_plugin_workdir()]).
#'
#' @param working_dir Character scalar. Directory containing `LIST/`,
#'   `SIMUL/`, and `OUTP/`. For AquaCrop 7.3 this must be the folder that
#'   holds `AquaCrop73.exe` (the plugin ignores other working directories).
#'   Defaults to `dirname(get_aquacrop_path())`.
#' @param aquacrop_exe Character scalar path to the executable, or `NULL`
#'   (default) to use [get_aquacrop_path()]. Ignored when `cmd` is set and
#'   does not contain `{aquacrop_exe}`.
#' @param show_log If `TRUE`, stream console output; otherwise it is
#'   captured/discarded.
#' @param progress_window If `FALSE` (default), suppress AquaCrop 7.3's Tk
#'   progress window. FAO's plugin already runs headless when `import
#'   tkinter` fails; we force that by briefly renaming the bundled
#'   `_internal/_tkinter.pyd` next to the executable (restored when the
#'   call finishes). Set `TRUE` to keep the official progress dialog.
#' @param program_parameters If `TRUE` (default), AquaCrop may load sibling
#'   `.PP1` / `.PPn` files next to each project in `LIST/` (same basename;
#'   they are not listed in the `.PRM`). If `FALSE`, those files are
#'   temporarily renamed for the duration of the call so the run uses FAO
#'   Table 2.19a defaults. Write custom files with [write_pp()].
#' @param cmd Optional command template, interpolated with [glue::glue()].
#'   Substitutions: `{aquacrop_exe}`, `{working_dir}`. If `NULL`, runs
#'   `"{aquacrop_exe}"`.
#'
#' @return Integer exit/status code from [system()] (invisibly).
#' @export
#' @seealso [set_aquacrop_path()], [prepare_plugin_workdir()], [write_pp()]
run_aquacrop <- function(working_dir = NULL, aquacrop_exe = NULL,
                         show_log = FALSE, progress_window = FALSE,
                         program_parameters = TRUE,
                         cmd = NULL) {
  aquacrop_exe <- .resolve_aquacrop_exe(aquacrop_exe)
  if (is.null(working_dir)) {
    working_dir <- dirname(aquacrop_exe)
  }
  if (!dir.exists(working_dir)) {
    stop("working_dir does not exist: ", working_dir, call. = FALSE)
  }
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(working_dir)

  if (isTRUE(progress_window)) {
    .restore_bundled_tkinter(aquacrop_exe)
  } else {
    restore_tk <- .stash_bundled_tkinter(aquacrop_exe)
    on.exit(restore_tk(), add = TRUE)
  }

  if (isTRUE(program_parameters)) {
    .restore_program_parameters(getwd())
  } else {
    restore_pp <- .stash_program_parameters(getwd())
    on.exit(restore_pp(), add = TRUE)
  }

  cmd <- .build_aquacrop_cmd(
    aquacrop_exe = aquacrop_exe,
    cmd = cmd,
    working_dir = getwd()
  )

  status <- system(cmd, show.output.on.console = show_log)
  if (status != 0)
    warning(sprintf("AquaCrop exited with status %d in %s", status, getwd()))

  invisible(status)
}

.TKINTER_STASH <- new.env(parent = emptyenv())
.TKINTER_STASH$depth <- 0L

#' Paths of the PyInstaller `_tkinter` extension next to AquaCrop 7.3.
#'
#' @return Named character vector `pyd`, `off`.
#' @noRd
.bundled_tkinter_paths <- function(aquacrop_exe) {
  internal <- file.path(dirname(aquacrop_exe), "_internal")
  pyd <- file.path(internal, "_tkinter.pyd")
  list(pyd = pyd, off = paste0(pyd, ".aquacropr-off"))
}

#' Undo a leftover `_tkinter.pyd` stash (crash, or `progress_window = TRUE`).
#'
#' @noRd
.restore_bundled_tkinter <- function(aquacrop_exe) {
  paths <- .bundled_tkinter_paths(aquacrop_exe)
  if (file.exists(paths$off) && !file.exists(paths$pyd)) {
    ok <- file.rename(paths$off, paths$pyd)
    if (!isTRUE(ok)) {
      warning("Could not restore AquaCrop Tk support: ", paths$off,
              call. = FALSE)
    }
  }
  invisible(NULL)
}

#' Temporarily hide bundled `_tkinter.pyd` so FAO skips the progress GUI.
#'
#' Nested [run_aquacrop()] calls share one stash (refcount). The file is
#' always put back when the outermost call exits.
#'
#' @return A zero-argument restorer.
#' @noRd
.stash_bundled_tkinter <- function(aquacrop_exe) {
  paths <- .bundled_tkinter_paths(aquacrop_exe)
  if (.TKINTER_STASH$depth == 0L) {
    if (file.exists(paths$pyd)) {
      ok <- file.rename(paths$pyd, paths$off)
      if (!isTRUE(ok)) {
        warning(
          "Could not hide AquaCrop's progress window (failed to rename ",
          paths$pyd, "). Pass progress_window = TRUE to keep the dialog, ",
          "or close any running AquaCrop73.exe and retry.",
          call. = FALSE
        )
        return(function() invisible(NULL))
      }
    } else if (!file.exists(paths$off)) {
      return(function() invisible(NULL))
    }
  }
  .TKINTER_STASH$depth <- .TKINTER_STASH$depth + 1L
  .TKINTER_STASH$exe <- aquacrop_exe
  function() {
    if (.TKINTER_STASH$depth <= 0L) {
      return(invisible(NULL))
    }
    .TKINTER_STASH$depth <- .TKINTER_STASH$depth - 1L
    if (.TKINTER_STASH$depth == 0L) {
      .restore_bundled_tkinter(.TKINTER_STASH$exe)
    }
    invisible(NULL)
  }
}

#' Build the shell command for [run_aquacrop()].
#'
#' @noRd
.build_aquacrop_cmd <- function(aquacrop_exe = NULL, cmd = NULL,
                               working_dir = "") {
  if (is.null(cmd) || !nzchar(cmd)) {
    cmd <- '"{aquacrop_exe}"'
  }
  if (grepl("{aquacrop_exe}", cmd, fixed = TRUE)) {
    aquacrop_exe <- .resolve_aquacrop_exe(aquacrop_exe)
  }
  glue::glue(
    cmd,
    aquacrop_exe = if (is.null(aquacrop_exe)) "" else aquacrop_exe,
    working_dir = if (is.null(working_dir)) "" else working_dir
  )
}
