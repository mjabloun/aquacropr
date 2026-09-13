#' Write an AquaCrop climate series file (`.Tnx`, `.ETo`, or `.PLU`)
#'
#' Format follows FAO reference manual Tables 2.23c–h: five header lines,
#' a blank line, a title line, a dashed line, then one record per day
#' (or 10-day / month).
#'
#' @param data A data.frame with a `Date` column (or `Year`/`Month`/`Day`)
#'   plus the value columns required by `type`.
#' @param path Character scalar destination path.
#' @param type One of `"Tnx"`, `"ETo"`, `"PLU"`.
#' @param description Character scalar written as line 1.
#' @param time_code Integer: `1` daily (default), `2` 10-day, `3` monthly.
#' @param tmin,tmax Column names for air temperature (when `type = "Tnx"`).
#' @param eto Column name for ETo (when `type = "ETo"`).
#' @param rain Column name for rainfall (when `type = "PLU"`).
#'
#' @return `path`, invisibly.
#' @export
write_weather <- function(data, path, type = c("Tnx", "ETo", "PLU"),
                             description = NULL,
                             time_code = 1L,
                             tmin = "Tmin", tmax = "Tmax",
                             eto = "ETo", rain = "Rain") {
  type <- match.arg(type)
  data <- .ac_coerce_weather_dates(data)
  data <- data[order(data$Date), , drop = FALSE]
  first <- data$Date[[1]]
  if (is.null(description) || !nzchar(description)) {
    description <- switch(
      type,
      Tnx = "Daily air temperature data",
      ETo = "Daily reference evapotranspiration (ETo)",
      PLU = "Daily rainfall"
    )
  }
  rec_label <- switch(
    as.character(time_code),
    "1" = "Daily records (1=daily, 2=10-daily and 3=monthly data)",
    "2" = "10-day records (1=daily, 2=10-daily and 3=monthly data)",
    "3" = "Monthly records (1=daily, 2=10-daily and 3=monthly data)",
    stop("`time_code` must be 1, 2, or 3", call. = FALSE)
  )
  title <- switch(
    type,
    Tnx = " Tmin (C)     TMax (C)",
    ETo = "Average ETo (mm/day)",
    PLU = "Total Rain (mm)"
  )
  hdr <- c(
    description,
    sprintf("%d : %s", as.integer(time_code), rec_label),
    sprintf("%d : First day of record (1, 11 or 21 for 10-day or 1 for months)",
            as.integer(format(first, "%d"))),
    sprintf("%d : First month of record", as.integer(format(first, "%m"))),
    sprintf("%d : First year of record (1901 if not linked to a specific year)",
            as.integer(format(first, "%Y"))),
    "",
    title,
    "========================"
  )
  body <- switch(
    type,
    Tnx = {
      if (!all(c(tmin, tmax) %in% names(data))) {
        stop("`data` must contain columns ", tmin, " and ", tmax, call. = FALSE)
      }
      sprintf("%6.1f %6.1f", as.numeric(data[[tmin]]), as.numeric(data[[tmax]]))
    },
    ETo = {
      if (!eto %in% names(data)) {
        stop("`data` must contain column ", eto, call. = FALSE)
      }
      sprintf("%6.1f", as.numeric(data[[eto]]))
    },
    PLU = {
      if (!rain %in% names(data)) {
        stop("`data` must contain column ", rain, call. = FALSE)
      }
      sprintf("%6.1f", as.numeric(data[[rain]]))
    }
  )
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(c(hdr, body), path, useBytes = TRUE)
  invisible(path)
}

#' Write an AquaCrop `.CLI` climate wrapper
#'
#' @param path Destination `.CLI` path.
#' @param tnx,eto,plu,co2 File names (not full paths) of the four climate
#'   files, as AquaCrop stores them.
#' @param description Character scalar for line 1.
#' @param version AquaCrop version number written on line 2 (default 7.3).
#'
#' @return `path`, invisibly.
#' @export
write_cli <- function(path, tnx, eto, plu, co2 = "MaunaLoa.CO2",
                         description = "Climatic data",
                         version = 7.3) {
  lines <- c(
    description,
    sprintf("%.1f : AquaCrop Version", as.numeric(version)),
    as.character(tnx),
    as.character(eto),
    as.character(plu),
    as.character(co2)
  )
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, path, useBytes = TRUE)
  invisible(path)
}

#' @noRd
.ac_coerce_weather_dates <- function(data) {
  data <- as.data.frame(data)
  if ("Date" %in% names(data)) {
    data$Date <- as.Date(data$Date)
    return(data)
  }
  y <- intersect(c("Year", "YEAR"), names(data))
  m <- intersect(c("Month", "MM", "month"), names(data))
  d <- intersect(c("Day", "DD", "day"), names(data))
  if (length(y) && length(m) && length(d)) {
    data$Date <- as.Date(sprintf("%04d-%02d-%02d",
                                 as.integer(data[[y[[1]]]]),
                                 as.integer(data[[m[[1]]]]),
                                 as.integer(data[[d[[1]]]])))
    return(data)
  }
  stop("`data` must have a Date column or Year/Month/Day", call. = FALSE)
}
