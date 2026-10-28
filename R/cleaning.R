#' Automated Quality Cleaning for Biodiversity Occurrence Coordinates
#'
#' Scans occurrence data tables and detects common spatial data anomalies
#' frequently found in biodiversity databases (GBIF, iNaturalist, museum records):
#' - Missing or non-numeric coordinates (`NA`, `NaN`)
#' - Exact zero coordinates `(0, 0)` (Null Island artifact)
#' - Coordinates outside valid ranges (`lat` outside `[-90, 90]`, `lon` outside `[-180, 180]`)
#' - Swapped latitude and longitude (e.g., `abs(lat) > 90` while `abs(lon) <= 90`)
#' - Equal coordinates (`lat == lon`) often used as placeholder entries
#'
#' @param dat A `data.frame` containing occurrence records with `lat` and `lon` columns.
#' @param drop_zero Logical: drop coordinates at `(0, 0)`. Default `TRUE`.
#' @param fix_swapped Logical: automatically detect and swap inverted latitude and
#'   longitude values. Default `TRUE`.
#' @param drop_out_of_bounds Logical: drop coordinates outside `[-90, 90]` latitude
#'   and `[-180, 180]` longitude. Default `TRUE`.
#' @param drop_equal Logical: drop records where `lat == lon` (unless both are 0,
#'   which is handled by `drop_zero`). Default `TRUE`.
#' @param verbose Logical: print summary message describing cleaned coordinates.
#'   Default `TRUE`.
#'
#' @return A named list containing:
#'   - `data`: cleaned `data.frame`.
#'   - `n_original`: original row count.
#'   - `n_cleaned`: final row count.
#'   - `n_removed`: number of rows removed.
#'   - `flags`: character vector of flags and actions taken.
#'   - `report`: human-readable character summary suitable for publication methods.
#' @export
#' @examples
#' df <- data.frame(species = "SpA",
#'                  lat = c(40.5, 0, 95.0, 35.0),
#'                  lon = c(-96.5, 0, 40.0, 35.0))
#' cl <- clean_coordinates(df)
#' cl$data
clean_coordinates <- function(dat,
                              drop_zero = TRUE,
                              fix_swapped = TRUE,
                              drop_out_of_bounds = TRUE,
                              drop_equal = TRUE,
                              verbose = TRUE) {
  if (!is.data.frame(dat)) stop("Input 'dat' must be a data frame.")
  if (!all(c("lat", "lon") %in% names(dat))) {
    stop("Input data frame must contain 'lat' and 'lon' columns.")
  }

  n_orig <- nrow(dat)
  if (n_orig == 0L) {
    return(list(data = dat, n_original = 0L, n_cleaned = 0L, n_removed = 0L,
                flags = character(0), report = "No records to clean."))
  }

  # Ensure numeric
  dat$lat <- suppressWarnings(as.numeric(dat$lat))
  dat$lon <- suppressWarnings(as.numeric(dat$lon))

  keep <- rep(TRUE, n_orig)
  flags <- character(0)
  reasons <- character(n_orig)

  # 1. Missing coordinates
  na_idx <- is.na(dat$lat) | is.na(dat$lon)
  if (any(na_idx)) {
    n_na <- sum(na_idx)
    keep[na_idx] <- FALSE
    reasons[na_idx] <- "missing_coords"
    flags <- c(flags, paste0(n_na, " record(s) with missing coordinates (NA)"))
  }

  # 2. Swap detection (lat > 90 and lon <= 90)
  if (fix_swapped) {
    swap_idx <- keep & (abs(dat$lat) > 90 & abs(dat$lon) <= 90)
    if (any(swap_idx)) {
      n_swap <- sum(swap_idx)
      tmp <- dat$lat[swap_idx]
      dat$lat[swap_idx] <- dat$lon[swap_idx]
      dat$lon[swap_idx] <- tmp
      flags <- c(flags, paste0(n_swap, " record(s) with swapped lat/lon automatically inverted"))
    }
  }

  # 3. Exact zero coordinates (0, 0)
  if (drop_zero) {
    zero_idx <- keep & (abs(dat$lat) < 1e-6 & abs(dat$lon) < 1e-6)
    if (any(zero_idx)) {
      n_zero <- sum(zero_idx)
      keep[zero_idx] <- FALSE
      reasons[zero_idx] <- "zero_coords"
      flags <- c(flags, paste0(n_zero, " record(s) at Null Island (0, 0)"))
    }
  }

  # 4. Out of bounds
  if (drop_out_of_bounds) {
    oob_idx <- keep & (abs(dat$lat) > 90 | abs(dat$lon) > 180)
    if (any(oob_idx)) {
      n_oob <- sum(oob_idx)
      keep[oob_idx] <- FALSE
      reasons[oob_idx] <- "out_of_bounds"
      flags <- c(flags, paste0(n_oob, " record(s) outside geographical bounds (lat [-90,90], lon [-180,180])"))
    }
  }

  # 5. Equal coordinates (lat == lon, common placeholder)
  if (drop_equal) {
    eq_idx <- keep & (abs(dat$lat - dat$lon) < 1e-6)
    if (any(eq_idx)) {
      n_eq <- sum(eq_idx)
      keep[eq_idx] <- FALSE
      reasons[eq_idx] <- "equal_lat_lon"
      flags <- c(flags, paste0(n_eq, " record(s) with identical lat/lon values (placeholder artifact)"))
    }
  }

  cleaned_data <- dat[keep, , drop = FALSE]
  n_clean <- nrow(cleaned_data)
  n_rem <- n_orig - n_clean

  report <- if (n_rem == 0L) {
    "All coordinates passed quality cleaning (no invalid or missing coordinates found)."
  } else {
    paste0("Coordinate quality cleaning removed ", n_rem, " problematic record(s) (",
           paste(flags, collapse = "; "), "). Retained ", n_clean, " valid record(s).")
  }

  if (verbose && n_rem > 0L) {
    message("Coordinate Quality: ", report)
  }

  list(
    data = cleaned_data,
    n_original = n_orig,
    n_cleaned = n_clean,
    n_removed = n_rem,
    flags = flags,
    report = report
  )
}
