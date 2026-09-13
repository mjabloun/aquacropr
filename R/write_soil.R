#' FAO soil class (1–4) used to generate CRa/CRb.
#'
#' Matches Fortran `NumberSoilClass()` (AquaCrop 7.x).
#'
#' @param sat,fc,wp Volumetric water content at saturation, field capacity,
#'   and permanent wilting point (%).
#' @param ksat Saturated hydraulic conductivity (mm/day).
#' @return Integer 1 (sandy), 2 (loamy), 3 (sandy clayey), or 4 (silty clayey).
#' @noRd
.ac_soil_class <- function(sat, fc, wp, ksat) {
  sat <- as.numeric(sat)
  fc <- as.numeric(fc)
  wp <- as.numeric(wp)
  ksat <- as.numeric(ksat)
  n <- length(sat)
  out <- integer(n)
  for (i in seq_len(n)) {
    if (sat[[i]] <= 55) {
      if (wp[[i]] >= 20) {
        out[[i]] <- if (sat[[i]] >= 49 && fc[[i]] >= 40) 4L else 3L
      } else if (fc[[i]] < 23) {
        out[[i]] <- 1L
      } else if (wp[[i]] > 16 && ksat[[i]] < 100) {
        out[[i]] <- 3L
      } else if (wp[[i]] < 6 && fc[[i]] < 28 && ksat[[i]] > 750) {
        out[[i]] <- 1L
      } else {
        out[[i]] <- 2L
      }
    } else {
      out[[i]] <- 4L
    }
  }
  out
}

#' Default CRa and CRb (Janssens 2006 / Fortran `DetermineParametersCR`).
#'
#' @return data.frame with `class`, `cra`, `crb`.
#' @noRd
.ac_cra_crb <- function(sat, fc, wp, ksat) {
  cls <- .ac_soil_class(sat, fc, wp, ksat)
  ksat <- as.numeric(ksat)
  cra <- rep(NA_real_, length(cls))
  crb <- rep(NA_real_, length(cls))
  for (i in seq_along(cls)) {
    k <- ksat[[i]]
    if (!is.finite(k) || k <= 0) next
    lk <- log(k)
    if (cls[[i]] == 1L) {
      cra[[i]] <- -0.3112 - k / 100000
      crb[[i]] <- -1.4936 + 0.2416 * lk
    } else if (cls[[i]] == 2L) {
      cra[[i]] <- -0.4986 + 9 * k / 100000
      crb[[i]] <- -2.1320 + 0.4778 * lk
    } else if (cls[[i]] == 3L) {
      cra[[i]] <- -0.5677 - 4 * k / 100000
      crb[[i]] <- -3.7189 + 0.5922 * lk
    } else {
      cra[[i]] <- -0.6366 + 8 * k / 10000
      crb[[i]] <- -1.9165 + 0.7063 * lk
    }
  }
  data.frame(class = cls, cra = cra, crb = crb)
}

#' Default CN from top-horizon Ksat (Fortran `DetermineCN_default`).
#'
#' @noRd
.ac_cn_default <- function(ksat) {
  ksat <- as.numeric(ksat)[[1]]
  if (ksat > 864) 46L else if (ksat >= 347) 61L else if (ksat >= 36) 72L else 77L
}

#' Default REW (mm) from top-horizon FC and PWP (Table 2.23s-6).
#'
#' @noRd
.ac_rew_default <- function(fc, wp, z_surf = 0.04) {
  rew <- as.integer(round(10 * (as.numeric(fc)[[1]] - as.numeric(wp)[[1]] / 2) *
                            z_surf))
  max(0L, min(15L, rew))
}

#' Write an AquaCrop soil profile file (`.SOL`)
#'
#' FAO Table 2.23s-1 layout (AquaCrop 7.x): description, version, CN, REW,
#' number of horizons (max 5), unused `-9`, then one row per horizon with
#' thickness, θsat/θFC/θPWP, Ksat, penetrability, gravel, CRa, CRb, and a
#' label. If `cn` / `rew` / CRa / CRb are omitted they are filled with the
#' official defaults (CN from top-horizon Ksat; REW from top-horizon FC and
#' PWP; CRa/CRb from soil class + Ksat, Janssens 2006).
#'
#' @param path Destination `.SOL`.
#' @param layers A data.frame with one row per horizon. Required columns:
#'   `thickness` (m), `sat`, `fc`, `wp` (vol %), `ksat` (mm/day). Optional:
#'   `penetrability` (default 100), `gravel` (default 0), `description`,
#'   `cra`, `crb` (computed when missing).
#' @param description File title (line 1).
#' @param cn Curve Number. Default from the top horizon's Ksat.
#' @param rew Readily evaporable water of the surface layer (mm, 0–15).
#'   Default from the top horizon's FC and PWP.
#' @param version AquaCrop version token (default `"7.3"`).
#' @param version_note Text in parentheses after the version.
#'
#' @return `path`, invisibly.
#' @export
#' @seealso [write_project()]
#' @examples
#' \dontrun{
#' write_sol("DATA/field.SOL", data.frame(
#'   thickness = 4, sat = 46, fc = 31, wp = 15, ksat = 500,
#'   description = "loam"
#' ))
#' }
write_sol <- function(path,
                         layers,
                         description = NULL,
                         cn = NULL,
                         rew = NULL,
                         version = "7.3",
                         version_note = "July 2026") {
  if (!is.character(path) || length(path) != 1L || !nzchar(path)) {
    stop("`path` must be a non-empty character scalar", call. = FALSE)
  }
  layers <- .ac_sol_layers(layers)
  n <- nrow(layers)
  if (is.null(cn)) cn <- .ac_cn_default(layers$ksat[[1]])
  if (is.null(rew)) rew <- .ac_rew_default(layers$fc[[1]], layers$wp[[1]])
  cn <- as.integer(cn)[[1]]
  rew <- as.integer(rew)[[1]]
  if (rew < 0L || rew > 15L) {
    stop("`rew` must be between 0 and 15 mm", call. = FALSE)
  }
  if (is.null(description) || !nzchar(description)) {
    description <- if (n == 1L && nzchar(layers$description[[1]])) {
      paste0("deep uniform '", layers$description[[1]], "' soil profile")
    } else {
      sprintf("%d-horizon soil profile", n)
    }
  }
  hdr <- c(
    as.character(description)[[1]],
    sprintf("%9s                 : AquaCrop Version (%s)",
            as.character(version)[[1]], as.character(version_note)[[1]]),
    sprintf("%8d                   : CN (Curve Number)", cn),
    sprintf("%8d                   : Readily evaporable water from top layer (mm)",
            rew),
    sprintf("%8d                   : number of soil horizons", n),
    "       -9                   : variable no longer applicable",
    "  Thickness  Sat   FC    WP     Ksat   Penetrability  Gravel  CRa       CRb           description",
    "  ---(m)-   ----(vol %)-----  (mm/day)      (%)        (%)    -----------------------------------------"
  )
  body <- sprintf(
    "%8.2f%8.1f%6.1f%6.1f%8.1f%11d%10d%13.6f%10.6f  %s",
    layers$thickness,
    layers$sat,
    layers$fc,
    layers$wp,
    layers$ksat,
    as.integer(layers$penetrability),
    as.integer(layers$gravel),
    layers$cra,
    layers$crb,
    layers$description
  )
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(c(hdr, body), path, useBytes = TRUE)
  invisible(normalizePath(path, winslash = "/", mustWork = TRUE))
}

#' @noRd
.ac_sol_layers <- function(layers) {
  if (is.null(layers)) {
    stop("`layers` is required", call. = FALSE)
  }
  layers <- as.data.frame(layers, stringsAsFactors = FALSE)
  names(layers) <- tolower(names(layers))
  need <- c("thickness", "sat", "fc", "wp", "ksat")
  miss <- setdiff(need, names(layers))
  if (length(miss)) {
    stop("`layers` is missing column(s): ", paste(miss, collapse = ", "),
         call. = FALSE)
  }
  n <- nrow(layers)
  if (n < 1L || n > 5L) {
    stop("AquaCrop allows 1 to 5 soil horizons", call. = FALSE)
  }
  for (nm in need) {
    layers[[nm]] <- as.numeric(layers[[nm]])
  }
  if (any(!is.finite(unlist(layers[need])))) {
    stop("Horizon thickness, sat, fc, wp, and ksat must be finite",
         call. = FALSE)
  }
  if (any(layers$thickness <= 0)) {
    stop("`thickness` must be positive (m)", call. = FALSE)
  }
  if (any(layers$wp > layers$fc | layers$fc > layers$sat)) {
    stop("Require wp <= fc <= sat (vol %)", call. = FALSE)
  }
  if (any(layers$ksat < 0)) {
    stop("`ksat` must be >= 0 mm/day", call. = FALSE)
  }
  if (!"penetrability" %in% names(layers)) layers$penetrability <- 100
  if (!"gravel" %in% names(layers)) layers$gravel <- 0
  if (!"description" %in% names(layers)) {
    layers$description <- paste0("horizon ", seq_len(n))
  }
  layers$penetrability <- as.numeric(layers$penetrability)
  layers$gravel <- as.numeric(layers$gravel)
  layers$description <- as.character(layers$description)
  cr <- .ac_cra_crb(layers$sat, layers$fc, layers$wp, layers$ksat)
  if (!"cra" %in% names(layers) || any(is.na(layers$cra))) {
    layers$cra <- cr$cra
  }
  if (!"crb" %in% names(layers) || any(is.na(layers$crb))) {
    layers$crb <- cr$crb
  }
  if (any(!is.finite(layers$cra) | !is.finite(layers$crb))) {
    stop("Could not compute CRa/CRb (need ksat > 0, or supply cra/crb)",
         call. = FALSE)
  }
  layers
}
