.AC_IRR_METHODS <- c(
  "1" = "Sprinkler irrigation",
  "2" = "Surface irrigation: Basin",
  "3" = "Surface irrigation: Border",
  "4" = "Surface irrigation: Furrow",
  "5" = "Drip irrigation"
)

.AC_IRR_WETTED <- c("1" = 100L, "2" = 100L, "3" = 100L, "4" = 90L, "5" = 30L)

.AC_IRR_MODES <- c(
  "1" = "Irrigation schedule",
  "2" = "Generate irrigation schedule",
  "3" = "Determination of Net Irrigation requirement"
)

.AC_IRR_TIME <- c(
  "1" = "Time criterion = fixed intervals",
  "2" = "Time criterion = allowable depletion (mm)",
  "3" = "Time criterion = allowable fraction of RAW",
  "4" = "Time criterion = keep minimum surface water layer"
)

.AC_IRR_DEPTH <- c(
  "1" = "Depth criterion = back to FC",
  "2" = "Fixed application depth"
)

#' @noRd
.ac_irr_method_code <- function(method) {
  if (is.numeric(method)) {
    code <- as.integer(method)[[1]]
  } else {
    names <- c(sprinkler = 1L, basin = 2L, border = 3L, furrow = 4L, drip = 5L)
    key <- tolower(as.character(method)[[1]])
    if (!key %in% names(names)) {
      stop("`method` must be 1:5 or one of: ",
           paste(names(names), collapse = ", "), call. = FALSE)
    }
    code <- unname(names[[key]])
  }
  if (!code %in% 1:5) stop("`method` must be in 1:5", call. = FALSE)
  code
}

#' @noRd
.ac_irr_mode_code <- function(mode) {
  if (is.numeric(mode)) {
    code <- as.integer(mode)[[1]]
  } else {
    names <- c(
      events = 1L, schedule = 1L, generate = 2L, generation = 2L,
      net = 3L, requirement = 3L
    )
    key <- tolower(as.character(mode)[[1]])
    if (!key %in% names(names)) {
      stop("`mode` must be 1:3 or one of: events, generate, net", call. = FALSE)
    }
    code <- unname(names[[key]])
  }
  if (!code %in% 1:3) stop("`mode` must be in 1:3", call. = FALSE)
  code
}

#' @noRd
.ac_irr_criterion <- function(x, max_code, what) {
  if (is.numeric(x)) {
    code <- as.integer(x)[[1]]
  } else {
    maps <- list(
      time = c(interval = 1L, depletion_mm = 2L, depletion_raw = 3L,
               raw = 3L, bund = 4L, bund_water = 4L),
      depth = c(field_capacity = 1L, fc = 1L, fixed = 2L, fixed_depth = 2L)
    )
    key <- tolower(as.character(x)[[1]])
    if (!key %in% names(maps[[what]])) {
      stop("Unknown ", what, " criterion: ", x, call. = FALSE)
    }
    code <- unname(maps[[what]][[key]])
  }
  if (!code %in% seq_len(max_code)) {
    stop("`", what, "_criterion` must be in 1:", max_code, call. = FALSE)
  }
  code
}

#' Write an AquaCrop irrigation file (`.IRR`)
#'
#' FAO Table 2.23q-1. Three modes:
#'
#' 1. **events** (`mode = 1`): specified irrigation days, depths, and ECw
#' 2. **generate** (`mode = 2`): rules that AquaCrop uses to schedule irrigations
#' 3. **net** (`mode = 3`): net irrigation requirement for a RAW depletion
#'
#' Rainfed cropping is `(None)` in the project file, not an `.IRR`.
#'
#' @param path Destination `.IRR`.
#' @param mode `1`/`"events"`, `2`/`"generate"`, or `3`/`"net"`.
#' @param method Irrigation method `1:5` or `"sprinkler"`, `"basin"`,
#'   `"border"`, `"furrow"`, `"drip"`. Default sprinkler.
#' @param surface_wetted Percent of soil surface wetted. Default depends on
#'   `method` (100 / 100 / 100 / 90 / 30).
#' @param description File title (line 1).
#' @param version,version_note AquaCrop version line.
#' @param events For `mode = 1`: data.frame with `day` (days after planting,
#'   or after `reference_day`), `depth` (mm), optional `ecw` (dS/m).
#' @param reference_day For `mode = 1`: AquaCrop day number of Day 1, a
#'   `Date`, or `-9` (default) for the start of the growing period.
#' @param time_criterion For `mode = 2`: `1` fixed interval, `2` depletion
#'   (mm), `3` depletion (% RAW), `4` minimum bund water. Names
#'   `"interval"`, `"depletion_mm"`, `"depletion_raw"`, `"bund"` also work.
#' @param depth_criterion For `mode = 2`: `1` back to field capacity, `2`
#'   fixed net depth. Names `"field_capacity"` / `"fixed"`.
#' @param rules For `mode = 2`: data.frame with `from_day`, `time_value`,
#'   `depth_value`, optional `ecw`. First `from_day` must be 1.
#' @param raw_depletion For `mode = 3`: allowable RAW depletion (%) so the
#'   root zone does not drop below this threshold (0 = field capacity).
#'
#' @return `path`, invisibly.
#' @export
#' @seealso [write_project()]
#' @examples
#' \dontrun{
#' write_irr("DATA/Inet.IRR", mode = "net", raw_depletion = 30)
#' write_irr("DATA/Igen.IRR", mode = "generate",
#'              time_criterion = "depletion_raw",
#'              depth_criterion = "field_capacity",
#'              rules = data.frame(from_day = 1, time_value = 80, depth_value = 0))
#' }
write_irr <- function(path,
                         mode,
                         method = "sprinkler",
                         surface_wetted = NULL,
                         description = NULL,
                         version = "7.3",
                         version_note = "July 2026",
                         events = NULL,
                         reference_day = -9,
                         time_criterion = 1L,
                         depth_criterion = 1L,
                         rules = NULL,
                         raw_depletion = 30) {
  if (!is.character(path) || length(path) != 1L || !nzchar(path)) {
    stop("`path` must be a non-empty character scalar", call. = FALSE)
  }
  method <- .ac_irr_method_code(method)
  mode <- .ac_irr_mode_code(mode)
  if (is.null(surface_wetted)) {
    surface_wetted <- unname(.AC_IRR_WETTED[[as.character(method)]])
  }
  surface_wetted <- as.integer(surface_wetted)[[1]]
  if (surface_wetted < 0L || surface_wetted > 100L) {
    stop("`surface_wetted` must be in 0:100", call. = FALSE)
  }

  if (is.null(description) || !nzchar(description)) {
    description <- switch(
      as.character(mode),
      "1" = "Irrigation schedule",
      "2" = "Generation of irrigation schedule",
      "3" = sprintf(
        "Determination of Net irrigation requirement (allowable depletion %d %% RAW)",
        as.integer(raw_depletion)[[1]]
      )
    )
  }

  hdr <- c(
    as.character(description)[[1]],
    sprintf("   %s   : AquaCrop Version (%s)",
            as.character(version)[[1]], as.character(version_note)[[1]]),
    sprintf("   %d     : %s", method, unname(.AC_IRR_METHODS[[as.character(method)]])),
    sprintf(" %3d     : Percentage of soil surface wetted by irrigation",
            surface_wetted),
    sprintf("   %d     : %s", mode, unname(.AC_IRR_MODES[[as.character(mode)]]))
  )

  body <- switch(
    as.character(mode),
    "1" = .ac_irr_events_block(events, reference_day),
    "2" = .ac_irr_generate_block(rules, time_criterion, depth_criterion),
    "3" = {
      raw_depletion <- as.integer(raw_depletion)[[1]]
      if (raw_depletion < 0L || raw_depletion > 100L) {
        stop("`raw_depletion` must be in 0:100", call. = FALSE)
      }
      sprintf("  %2d     : Allowable depletion of RAW (%%)", raw_depletion)
    }
  )

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(c(hdr, body), path, useBytes = TRUE)
  invisible(normalizePath(path, winslash = "/", mustWork = TRUE))
}

#' @noRd
.ac_irr_reference_day <- function(reference_day) {
  if (inherits(reference_day, "Date")) {
    return(aquacrop_day_number(reference_day)[[1]])
  }
  as.integer(reference_day)[[1]]
}

#' @noRd
.ac_irr_events_block <- function(events, reference_day) {
  ref <- .ac_irr_reference_day(reference_day)
  if (is.null(events)) {
    events <- data.frame(day = integer(), depth = numeric(), ecw = numeric())
  }
  events <- as.data.frame(events, stringsAsFactors = FALSE)
  names(events) <- tolower(names(events))
  if (nrow(events) && !all(c("day", "depth") %in% names(events))) {
    stop("`events` must have columns day and depth", call. = FALSE)
  }
  if (!"ecw" %in% names(events)) events$ecw <- 0
  if (nrow(events)) {
    events$day <- as.integer(events$day)
    events$depth <- as.numeric(events$depth)
    events$ecw <- as.numeric(events$ecw)
    if (any(events$day < 1L) || any(events$depth < 0) || any(events$ecw < 0)) {
      stop("`events` day must be >= 1 and depth/ecw >= 0", call. = FALSE)
    }
  }
  c(
    sprintf("  %d     : Day number of reference day (-9 = start of growing period)",
            ref),
    "",
    "   Day    Depth (mm)   ECw (dS/m)",
    "====================================",
    if (nrow(events)) {
      sprintf("%6d%10.0f%12.1f", events$day, events$depth, events$ecw)
    } else {
      character()
    }
  )
}

#' @noRd
.ac_irr_generate_block <- function(rules, time_criterion, depth_criterion) {
  time_criterion <- .ac_irr_criterion(time_criterion, 4L, "time")
  depth_criterion <- .ac_irr_criterion(depth_criterion, 2L, "depth")
  if (is.null(rules) || !nrow(as.data.frame(rules))) {
    stop("`rules` must have at least one row for generate mode", call. = FALSE)
  }
  rules <- as.data.frame(rules, stringsAsFactors = FALSE)
  names(rules) <- tolower(names(rules))
  if ("interval" %in% names(rules) && !"time_value" %in% names(rules)) {
    rules$time_value <- rules$interval
  }
  if ("depletion" %in% names(rules) && !"time_value" %in% names(rules)) {
    rules$time_value <- rules$depletion
  }
  need <- c("from_day", "time_value", "depth_value")
  miss <- setdiff(need, names(rules))
  if (length(miss)) {
    stop("`rules` is missing column(s): ", paste(miss, collapse = ", "),
         call. = FALSE)
  }
  if (!"ecw" %in% names(rules)) rules$ecw <- 0
  rules$from_day <- as.integer(rules$from_day)
  rules$time_value <- as.numeric(rules$time_value)
  rules$depth_value <- as.numeric(rules$depth_value)
  rules$ecw <- as.numeric(rules$ecw)
  if (!identical(rules$from_day[[1]], 1L)) {
    stop("The first generate rule must have from_day = 1", call. = FALSE)
  }
  time_hdr <- c(
    "1" = "Interval (days)",
    "2" = "Depletion (mm)",
    "3" = "Depleted RAW (%)",
    "4" = "Min water (mm)"
  )[[as.character(time_criterion)]]
  depth_hdr <- c(
    "1" = "Back to FC (+/- mm)",
    "2" = "Application depth (mm)"
  )[[as.character(depth_criterion)]]
  c(
    sprintf("   %d     : %s", time_criterion,
            unname(.AC_IRR_TIME[[as.character(time_criterion)]])),
    sprintf("   %d     : %s", depth_criterion,
            unname(.AC_IRR_DEPTH[[as.character(depth_criterion)]])),
    "",
    sprintf("  From day    %s   %s       ECw (dS/m)", time_hdr, depth_hdr),
    "=========================================================================",
    sprintf("%8d%14g%20g%20.1f",
            rules$from_day, rules$time_value, rules$depth_value, rules$ecw)
  )
}
