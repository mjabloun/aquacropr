#' List AquaCrop `.OUT` files in a directory
#'
#' Omits plugin status files (`AllDone.OUT`, `ListProjectsLoaded.OUT`).
#'
#' @param out_dir Character scalar, typically `.../OUTP`.
#' @return Character vector of paths, named by basename without `.OUT`.
#' @export
list_outputs <- function(out_dir) {
  files <- list.files(out_dir, pattern = "\\.(OUT|out)$", full.names = TRUE)
  files <- files[!.is_plugin_status_out(files)]
  nms <- tools::file_path_sans_ext(basename(files))
  stats::setNames(files, nms)
}

#' Plugin housekeeping `.OUT` files that are not simulation results.
#' @noRd
.is_plugin_status_out <- function(paths) {
  stems <- tolower(tools::file_path_sans_ext(basename(as.character(paths))))
  stems %in% c("alldone", "listprojectsloaded")
}

#' Read a daily AquaCrop output file
#'
#' Parses daily files in `OUTP/`. AquaCrop 7.3's stand-alone plugin writes a
#' combined `*PRMday.OUT` (water balance + crop + climate in one table).
#' Older plugins wrote separate `*CROP.OUT` / `*CLIM.OUT` / `*WABAL.OUT`
#' files; those still parse. Repeated `Run:` blocks in multi-year `.PRM`
#' files are skipped and recorded in a `Run` column.
#'
#' @param path Character scalar path to an `.OUT` file.
#' @return A `data.table` with `Date` (and `Run` when present). Duplicate
#'   header names are made unique (`Rain`, `Rain.1`, ...).
#' @export
read_out <- function(path) {
  if (!file.exists(path)) {
    stop("Output file not found: ", path, call. = FALSE)
  }
  lines <- readLines(path, warn = FALSE, encoding = "latin1")
  header_idx <- .ac_out_header_line(lines)
  if (is.na(header_idx)) {
    stop("Could not find a 'Day Month Year' header in ", path, call. = FALSE)
  }
  col_names <- .ac_unique_names(.ac_split_ws(lines[[header_idx]]))
  run <- 1L
  keep <- character()
  runs <- integer()
  for (line in lines[(header_idx + 1L):length(lines)]) {
    if (grepl("^\\s*Run:", line)) {
      n <- suppressWarnings(as.integer(sub(".*Run:\\s*", "", line)))
      if (!is.na(n)) run <- n
      next
    }
    if (.ac_is_daily_data_line(line)) {
      keep <- c(keep, line)
      runs <- c(runs, run)
    }
  }
  if (!length(keep)) {
    return(data.table::data.table())
  }
  dt <- data.table::fread(
    text = paste(keep, collapse = "\n"),
    header = FALSE,
    fill = TRUE,
    strip.white = TRUE,
    data.table = TRUE
  )
  n <- min(length(col_names), ncol(dt))
  data.table::setnames(dt, seq_len(n), col_names[seq_len(n)])
  data.table::set(dt, j = "Run", value = runs)
  num_cols <- setdiff(names(dt), "Run")
  for (nm in num_cols) {
    data.table::set(dt, j = nm, value = suppressWarnings(as.numeric(dt[[nm]])))
  }
  if (all(c("Day", "Month", "Year") %in% names(dt))) {
    dates <- as.Date(sprintf("%04d-%02d-%02d",
                             as.integer(dt$Year),
                             as.integer(dt$Month),
                             as.integer(dt$Day)))
    data.table::set(dt, j = "Date", value = dates)
    data.table::setcolorder(dt, c("Date", "Run", setdiff(names(dt), c("Date", "Run"))))
  }
  dt[]
}

#' @noRd
.ac_out_header_line <- function(lines) {
  hits <- grepl("\\bDay\\b", lines, ignore.case = FALSE) &
    grepl("\\bMonth\\b", lines) &
    grepl("\\bYear\\b", lines) &
    !grepl("Day1", lines, fixed = TRUE)
  if (!any(hits)) return(NA_integer_)
  which(hits)[[1]]
}

#' @noRd
.ac_split_ws <- function(line) {
  strsplit(trimws(line), "\\s+")[[1]]
}

#' @noRd
.ac_is_units_line <- function(line) {
  !.ac_is_daily_data_line(line)
}

#' @noRd
.ac_is_daily_data_line <- function(line) {
  toks <- .ac_split_ws(line)
  if (length(toks) < 3L) return(FALSE)
  grepl("^[0-9]{1,2}$", toks[[1]]) &&
    grepl("^[0-9]{1,2}$", toks[[2]]) &&
    grepl("^[0-9]{4}$", toks[[3]])
}

#' @noRd
.ac_unique_names <- function(nms) {
  nms <- make.names(nms, unique = FALSE)
  make.names(nms, unique = TRUE)
}
