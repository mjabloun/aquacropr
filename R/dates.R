.AC_ELAPSED_DAYS <- c(
  0, 31, 59.25, 90.25, 120.25, 151.25, 181.25,
  212.25, 243.25, 273.25, 304.25, 334.25
)

#' AquaCrop day number from a calendar date
#'
#' FAO day numbers count days elapsed since 0 January 1901, 00:00. The
#' official formula (reference manual Table 2.23w-4) is valid **1901–2099**.
#'
#' @param x A `Date` (or something coercible with [as.Date()]).
#' @return Integer vector of AquaCrop day numbers.
#' @export
#' @examples
#' aquacrop_day_number(as.Date("1982-08-24"))
aquacrop_day_number <- function(x) {
  x <- as.Date(x)
  y <- as.integer(format(x, "%Y"))
  m <- as.integer(format(x, "%m"))
  d <- as.integer(format(x, "%d"))
  if (any(y < 1901L | y > 2099L, na.rm = TRUE)) {
    stop("AquaCrop day numbers are only defined for 1901-2099", call. = FALSE)
  }
  as.integer(trunc((y - 1901) * 365.25 + .AC_ELAPSED_DAYS[m] + d + 0.05))
}

#' Calendar date from an AquaCrop day number
#'
#' Inverse of [aquacrop_day_number()] by matching against the FAO formula
#' (not a simple Unix-epoch offset).
#'
#' @param daynr Integer AquaCrop day number(s).
#' @return `Date` vector.
#' @export
#' @examples
#' aquacrop_date(29821)
aquacrop_date <- function(daynr) {
  daynr <- as.integer(daynr)
  out <- rep(as.Date(NA), length(daynr))
  for (i in seq_along(daynr)) {
    if (is.na(daynr[[i]])) next
    d <- as.Date("1900-12-31") + daynr[[i]]
    for (step in 1:4) {
      got <- aquacrop_day_number(d)
      delta <- daynr[[i]] - got
      if (identical(delta, 0L)) break
      d <- d + delta
    }
    if (!identical(aquacrop_day_number(d), daynr[[i]])) {
      stop("Could not invert AquaCrop day number ", daynr[[i]], call. = FALSE)
    }
    out[i] <- d
  }
  out
}
