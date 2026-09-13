.AC_MONTHS_EN <- c(
  "January", "February", "March", "April", "May", "June",
  "July", "August", "September", "October", "November", "December"
)

#' @noRd
.ac_file_ext <- function(path) {
  b <- basename(path)
  if (!grepl(".", b, fixed = TRUE)) return("")
  tolower(sub(".*\\.", "", b))
}

#' Write an AquaCrop project file (`.PRO` or `.PRM`)
#'
#' Builds a FAO stand-alone project from crop, soil, climate, dates, and
#' optional management files. A **single** simulation is written as `.PRO`
#' (or `.PRM` if you choose that extension); **several seasons** are written
#' as a multiple-run `.PRM`. The layout matches reference manual Table
#' 2.23w-1: version and description, then for each run the cultivation year,
#' four AquaCrop day numbers, and the 14 input-file triplets (name +
#' directory). Missing optional files are written as `(None)`.
#'
#' Directory lines are rewritten relative to `relativize_to` when that
#' argument is a folder (needed by AquaCrop 7.3, which concatenates the
#' executable directory onto every project path). Pass `FALSE` to keep
#' absolute paths, as the GUI does.
#'
#' @param path Destination `.PRO` or `.PRM`. If the extension is omitted,
#'   `.PRO` is used for one run and `.PRM` for several.
#' @param crop Path to a `.CRO` file.
#' @param soil Path to a `.SOL` file.
#' @param sim_start,sim_end First and last day of the simulation period
#'   (`Date`, or vectors recycled across seasons).
#' @param crop_start,crop_end First and last day of the growing cycle.
#'   Default: the simulation period.
#' @param climate Optional path to a `.CLI` file. Temperature, ETo, rain,
#'   and CO2 names are read from it unless you override them.
#' @param tnx,eto,plu,co2 Optional paths that override the files named in
#'   `climate`. `co2` defaults to `MaunaLoa.CO2` in [get_simul_dir()] when
#'   the CLI names that file and it is not next to the `.CLI`.
#' @param irrigation,management,calendar,groundwater,initial,offseason,observations
#'   Optional paths to `.IRR`, `.MAN`, `.CAL`, `.GWT`, `.SW0`, `.OFF`,
#'   `.OBS`. `NULL` means FAO defaults (`(None)`).
#' @param cultivation_year Integer, usually `1` for annuals (seeding year).
#'   Recycled across seasons.
#' @param seasons Optional data.frame with `sim_start` and `sim_end`, and
#'   optionally `crop_start`, `crop_end`, `cultivation_year`. Overrides the
#'   corresponding vector arguments.
#' @param description Project title (line 1). Empty string matches GUI
#'   files that start with a blank line.
#' @param version AquaCrop version token (default `"7.3"`).
#' @param version_note Text in parentheses after the version
#'   (default `"July 2026"`).
#' @param relativize_to Directory the plugin prefixes onto project paths
#'   (the folder that contains `AquaCrop73.exe`). Default: directory of
#'   [get_aquacrop_path()] when the executable is found. `FALSE` leaves
#'   absolute paths.
#' @param check If `TRUE` (default), require that supplied files exist.
#' @param program_parameters If `FALSE` (default), do not write a sibling
#'   `.PP1`/`.PPn`. `TRUE` writes FAO defaults via [write_pp()]. A named
#'   list is passed as overrides to [write_pp()].
#'
#' @return `path`, invisibly.
#' @export
#' @seealso [prepare_plugin_workdir()], [relativize_project_paths()],
#'   [write_cli()], [write_pp()], [aquacrop_day_number()]
#' @examples
#' \dontrun{
#' write_project(
#'   "LIST/field.PRM",
#'   crop = "DATA/WheatGDD.CRO",
#'   soil = "DATA/Silt.SOL",
#'   climate = "DATA/Tunis.CLI",
#'   sim_start = as.Date("1979-11-22"),
#'   sim_end   = as.Date("1980-05-23")
#' )
#' }
write_project <- function(path,
                             crop,
                             soil,
                             sim_start = NULL,
                             sim_end = NULL,
                             crop_start = NULL,
                             crop_end = NULL,
                             climate = NULL,
                             tnx = NULL,
                             eto = NULL,
                             plu = NULL,
                             co2 = NULL,
                             irrigation = NULL,
                             management = NULL,
                             calendar = NULL,
                             groundwater = NULL,
                             initial = NULL,
                             offseason = NULL,
                             observations = NULL,
                             cultivation_year = 1L,
                             seasons = NULL,
                             description = "",
                             version = "7.3",
                             version_note = "July 2026",
                             relativize_to = NULL,
                             check = TRUE,
                             program_parameters = FALSE) {
  if (!is.character(path) || length(path) != 1L || !nzchar(path)) {
    stop("`path` must be a non-empty character scalar", call. = FALSE)
  }

  dates <- .ac_project_seasons(
    seasons = seasons,
    sim_start = sim_start,
    sim_end = sim_end,
    crop_start = crop_start,
    crop_end = crop_end,
    cultivation_year = cultivation_year
  )
  n_run <- nrow(dates)

  ext <- .ac_file_ext(path)
  if (!nzchar(ext)) {
    path <- paste0(path, if (n_run > 1L) ".PRM" else ".PRO")
    ext <- .ac_file_ext(path)
  }
  if (!ext %in% c("pro", "prm")) {
    stop("`path` must have extension .PRO or .PRM", call. = FALSE)
  }
  if (n_run > 1L && identical(ext, "pro")) {
    warning("Multiple seasons written to a .PRO file; AquaCrop expects .PRM",
            call. = FALSE)
  }

  files <- .ac_project_files(
    climate = climate,
    tnx = tnx,
    eto = eto,
    plu = plu,
    co2 = co2,
    calendar = calendar,
    crop = crop,
    irrigation = irrigation,
    management = management,
    soil = soil,
    groundwater = groundwater,
    initial = initial,
    offseason = offseason,
    observations = observations,
    check = check
  )

  if (isFALSE(relativize_to)) {
    relativize_to <- NULL
  } else if (is.null(relativize_to)) {
    exe <- tryCatch(.find_aquacrop_exe(error = FALSE), error = function(e) "")
    if (nzchar(exe)) relativize_to <- dirname(exe)
  }

  hdr <- c(
    as.character(description)[[1]],
    sprintf(
      "%10s       : AquaCrop Version (%s)",
      as.character(version)[[1]],
      as.character(version_note)[[1]]
    )
  )
  blocks <- lapply(seq_len(n_run), function(i) {
    c(
      .ac_project_date_block(dates[i, ]),
      .ac_project_file_block(files, relativize_to = relativize_to)
    )
  })
  lines <- c(hdr, unlist(blocks, use.names = FALSE))
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, path, useBytes = TRUE)
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  if (!isFALSE(program_parameters)) {
    if (isTRUE(program_parameters)) {
      write_pp(path)
    } else if (is.list(program_parameters)) {
      write_pp(path, params = program_parameters)
    } else {
      stop("`program_parameters` must be TRUE, FALSE, or a named list",
           call. = FALSE)
    }
  }
  invisible(path)
}

#' @noRd
.ac_project_seasons <- function(seasons, sim_start, sim_end,
                                crop_start, crop_end, cultivation_year) {
  if (!is.null(seasons)) {
    if (!is.data.frame(seasons)) {
      stop("`seasons` must be a data.frame", call. = FALSE)
    }
    need <- c("sim_start", "sim_end")
    miss <- setdiff(need, names(seasons))
    if (length(miss)) {
      stop("`seasons` is missing column(s): ", paste(miss, collapse = ", "),
           call. = FALSE)
    }
    sim_start <- seasons$sim_start
    sim_end <- seasons$sim_end
    if ("crop_start" %in% names(seasons)) crop_start <- seasons$crop_start
    if ("crop_end" %in% names(seasons)) crop_end <- seasons$crop_end
    if ("cultivation_year" %in% names(seasons)) {
      cultivation_year <- seasons$cultivation_year
    }
  }
  if (is.null(sim_start) || is.null(sim_end)) {
    stop("Provide `sim_start` and `sim_end`, or a `seasons` data.frame",
         call. = FALSE)
  }
  sim_start <- as.Date(sim_start)
  sim_end <- as.Date(sim_end)
  n <- length(sim_start)
  sim_end <- .ac_recycle(sim_end, n, "sim_end")
  if (is.null(crop_start)) crop_start <- sim_start
  if (is.null(crop_end)) crop_end <- sim_end
  crop_start <- .ac_recycle(as.Date(crop_start), n, "crop_start")
  crop_end <- .ac_recycle(as.Date(crop_end), n, "crop_end")
  cultivation_year <- .ac_recycle(as.integer(cultivation_year), n,
                                  "cultivation_year")
  if (any(sim_end < sim_start, na.rm = TRUE)) {
    stop("`sim_end` must be on or after `sim_start` for every season",
         call. = FALSE)
  }
  if (any(crop_end < crop_start, na.rm = TRUE)) {
    stop("`crop_end` must be on or after `crop_start` for every season",
         call. = FALSE)
  }
  data.frame(
    sim_start = sim_start,
    sim_end = sim_end,
    crop_start = crop_start,
    crop_end = crop_end,
    cultivation_year = cultivation_year,
    stringsAsFactors = FALSE
  )
}

#' @noRd
.ac_recycle <- function(x, n, name) {
  if (length(x) == 1L) return(rep(x, n))
  if (length(x) != n) {
    stop("`", name, "` must have length 1 or ", n, call. = FALSE)
  }
  x
}

#' @noRd
.ac_project_files <- function(climate, tnx, eto, plu, co2, calendar, crop,
                              irrigation, management, soil, groundwater,
                              initial, offseason, observations, check) {
  crop <- .ac_require_file(crop, "crop", check = check)
  soil <- .ac_require_file(soil, "soil", check = check)
  parts <- .ac_climate_parts(climate, tnx, eto, plu, co2, check = check)
  list(
    cli = parts$cli,
    tnx = parts$tnx,
    eto = parts$eto,
    plu = parts$plu,
    co2 = parts$co2,
    calendar = .ac_optional_file(calendar, "calendar", check),
    crop = crop,
    irrigation = .ac_optional_file(irrigation, "irrigation", check),
    management = .ac_optional_file(management, "management", check),
    soil = soil,
    groundwater = .ac_optional_file(groundwater, "groundwater", check),
    initial = .ac_optional_file(initial, "initial", check),
    offseason = .ac_optional_file(offseason, "offseason", check),
    observations = .ac_optional_file(observations, "observations", check)
  )
}

#' @noRd
.ac_require_file <- function(path, name, check) {
  if (is.null(path) || !nzchar(path)) {
    stop("`", name, "` is required", call. = FALSE)
  }
  if (isTRUE(check) && !file.exists(path)) {
    stop("`", name, "` file not found: ", path, call. = FALSE)
  }
  path
}

#' @noRd
.ac_optional_file <- function(path, name, check) {
  if (is.null(path) || !nzchar(path) || identical(path, "(None)")) {
    return(NULL)
  }
  if (isTRUE(check) && !file.exists(path)) {
    stop("`", name, "` file not found: ", path, call. = FALSE)
  }
  path
}

#' @noRd
.ac_climate_parts <- function(climate, tnx, eto, plu, co2, check) {
  empty <- list(cli = NULL, tnx = NULL, eto = NULL, plu = NULL, co2 = NULL)
  if (is.null(climate) || !nzchar(climate)) {
    if (!is.null(tnx) || !is.null(eto) || !is.null(plu) || !is.null(co2)) {
      stop("`climate` (.CLI) is required when tnx/eto/plu/co2 are set",
           call. = FALSE)
    }
    return(empty)
  }
  climate <- .ac_require_file(climate, "climate", check = check)
  names <- .parse_cli_envelopes(climate)
  cli_dir <- dirname(climate)
  resolve <- function(override, fname, kind) {
    if (!is.null(override) && nzchar(override)) {
      return(.ac_optional_file(override, kind, check))
    }
    if (is.null(fname) || !nzchar(fname)) return(NULL)
    candidate <- file.path(cli_dir, fname)
    if (identical(kind, "co2") && !file.exists(candidate)) {
      sim <- tryCatch(get_simul_dir(error = FALSE), error = function(e) "")
      if (nzchar(sim) && file.exists(file.path(sim, fname))) {
        candidate <- file.path(sim, fname)
      }
    }
    .ac_optional_file(candidate, kind, check)
  }
  list(
    cli = climate,
    tnx = resolve(tnx, names$tnx, "tnx"),
    eto = resolve(eto, names$eto, "eto"),
    plu = resolve(plu, names$plu, "plu"),
    co2 = resolve(co2, names$co2, "co2")
  )
}

#' Read the four weather file names stored in a `.CLI` wrapper.
#'
#' @noRd
.parse_cli_envelopes <- function(path) {
  lines <- trimws(readLines(path, warn = FALSE))
  lines <- lines[nzchar(lines)]
  pick <- function(pattern) {
    hit <- grep(pattern, lines, ignore.case = TRUE, perl = TRUE)
    if (!length(hit)) return(NULL)
    basename(lines[[hit[[1]]]])
  }
  list(
    tnx = pick("\\.(tnx|tmp)$"),
    eto = pick("\\.eto$"),
    plu = pick("\\.plu$"),
    co2 = pick("\\.co2$")
  )
}

#' @noRd
.ac_english_date <- function(x) {
  x <- as.Date(x)
  paste(
    as.integer(format(x, "%d")),
    .AC_MONTHS_EN[as.integer(format(x, "%m"))],
    format(x, "%Y")
  )
}

#' @noRd
.ac_day_comment_line <- function(date, label) {
  daynr <- aquacrop_day_number(date)
  sprintf("%7d         : %s - %s", daynr, label, .ac_english_date(date))
}

#' @noRd
.ac_project_date_block <- function(row) {
  c(
    sprintf(
      "%7d         : Year number of cultivation (Seeding/planting year)",
      as.integer(row$cultivation_year)
    ),
    .ac_day_comment_line(row$sim_start, "First day of simulation period"),
    .ac_day_comment_line(row$sim_end, "Last day of simulation period"),
    .ac_day_comment_line(row$crop_start, "First day of cropping period"),
    .ac_day_comment_line(
      row$crop_end,
      "Last day of cropping period (maturity or premature end when too cold to reach maturity)"
    )
  )
}

#' @noRd
.ac_file_triplet <- function(header, file_path, relativize_to) {
  if (is.null(file_path) || !nzchar(file_path)) {
    return(c(header, "   (None)", "   (None)"))
  }
  fname <- basename(file_path)
  dirn <- dirname(file_path)
  if (!is.null(relativize_to) && nzchar(relativize_to) && dir.exists(relativize_to)) {
    rel <- .ac_path_rel(dirn, relativize_to)
    dirn <- gsub("/", "\\", rel, fixed = TRUE)
  } else {
    dirn <- gsub("/", "\\", normalizePath(dirn, winslash = "/", mustWork = FALSE),
                 fixed = TRUE)
  }
  if (!grepl("\\\\$", dirn) && !identical(dirn, "(None)")) {
    dirn <- paste0(dirn, "\\")
  }
  c(header, paste0("   ", fname), paste0("   ", dirn))
}

#' @noRd
.ac_project_file_block <- function(files, relativize_to) {
  c(
    .ac_file_triplet("-- 1. Climate (CLI) file", files$cli, relativize_to),
    .ac_file_triplet("   1.1 Temperature (Tnx or TMP) file", files$tnx, relativize_to),
    .ac_file_triplet("   1.2 Reference ET (ETo) file", files$eto, relativize_to),
    .ac_file_triplet("   1.3 Rain (PLU) file", files$plu, relativize_to),
    .ac_file_triplet("   1.4 Atmospheric CO2 concentration (CO2) file",
                     files$co2, relativize_to),
    .ac_file_triplet("-- 2. Calendar (CAL) file", files$calendar, relativize_to),
    .ac_file_triplet("-- 3. Crop (CRO) file", files$crop, relativize_to),
    .ac_file_triplet("-- 4. Irrigation management (IRR) file",
                     files$irrigation, relativize_to),
    .ac_file_triplet("-- 5. Field management (MAN) file",
                     files$management, relativize_to),
    .ac_file_triplet("-- 6. Soil profile (SOL) file", files$soil, relativize_to),
    .ac_file_triplet("-- 7. Groundwater table (GWT) file",
                     files$groundwater, relativize_to),
    .ac_file_triplet("-- 8. Initial conditions (SW0) file",
                     files$initial, relativize_to),
    .ac_file_triplet("-- 9. Off-season conditions (OFF) file",
                     files$offseason, relativize_to),
    .ac_file_triplet("-- 10. Field data (OBS) file",
                     files$observations, relativize_to)
  )
}
