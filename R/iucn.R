#' IUCN Red List Geographic Range Metrics (EOO & AOO)
#'
#' Computes Extent of Occurrence (EOO) and Area of Occupancy (AOO) according
#' to the IUCN Red List Guidelines (Criterion B).
#'
#' - **Extent of Occurrence (EOO)**: Defined as the area contained within the
#'   shortest continuous imaginary boundary which can be drawn to encompass all
#'   known sites of occurrence (Minimum Convex Polygon, MCP). Calculated in
#'   an equal-area projection in square kilometres (km2).
#' - **Area of Occupancy (AOO)**: Defined as the area within the EOO which is
#'   occupied by the taxon, calculated as the sum of standard 2 x 2 km
#'   grid cells (4 km2 each) intersected by occurrence points.
#' - **Preliminary Criterion B Assessment**: Evaluates thresholds for Critically
#'   Endangered (CR), Endangered (EN), Vulnerable (VU), or Least Concern (LC).
#'
#' @param pts_sf An `sf` spatial points object.
#' @param aoo_cell_km Numeric: grid cell width in kilometres for AOO calculation.
#'   Default is `2` (the standard IUCN recommendation).
#' @param return_polygon Logical: return the convex hull polygon as `sf` geometry.
#'   Default `TRUE`.
#' @param constrain_sf An optional `sf` polygon layer (e.g. landmass or water body)
#'   used to intersect and constrain the EOO Minimum Convex Polygon, excluding
#'   unsuitable habitat (e.g., marine areas for terrestrial species, or land for marine
#'   species) in accordance with IUCN Red List Guidelines (Criterion B). Default `NULL`.
#'
#' @return A named list containing:
#'   - `eoo_km2`: Extent of Occurrence in km2 (constrained if `constrain_sf` provided).
#'   - `eoo_raw_km2`: Raw Extent of Occurrence in km2 before habitat constraint.
#'   - `aoo_km2`: Area of Occupancy in km2.
#'   - `n_aoo_cells`: Number of occupied 2 x 2 km grid cells.
#'   - `n_points`: Total number of coordinate points analyzed.
#'   - `eoo_status`: Preliminary IUCN status according to EOO thresholds.
#'   - `aoo_status`: Preliminary IUCN status according to AOO thresholds.
#'   - `preliminary_status`: Most conservative preliminary IUCN status (Criterion B).
#'   - `polygon`: An `sf` polygon of the EOO (constrained if `constrain_sf` provided).
#'   - `polygon_raw`: An `sf` polygon of the raw Minimum Convex Polygon.
#'   - `is_constrained`: Logical indicating whether EOO was constrained by habitat.
#'   - `report`: Formatted string ready to paste into manuscript text.
#' @export
#' @examples
#' \dontrun{
#' res <- run_map("localities.csv")
#' iucn <- calculate_iucn_metrics(res$points)
#' cat(iucn$report)
#' }
calculate_iucn_metrics <- function(pts_sf, aoo_cell_km = 2, return_polygon = TRUE,
                                   constrain_sf = NULL) {
  if (!inherits(pts_sf, "sf")) {
    stop("Input 'pts_sf' must be an sf object.")
  }

  n_pts <- nrow(pts_sf)
  if (n_pts == 0L) {
    return(list(
      eoo_km2 = 0, eoo_raw_km2 = 0, aoo_km2 = 0, n_aoo_cells = 0L, n_points = 0L,
      eoo_status = "Data Deficient", aoo_status = "Data Deficient",
      preliminary_status = "Data Deficient", polygon = NULL, polygon_raw = NULL,
      is_constrained = FALSE,
      report = "No occurrence points available for IUCN evaluation."
    ))
  }

  orig_crs <- sf::st_crs(pts_sf)

  # Transform points to equal-area projection for geodesic accuracy
  pts_ll <- sf::st_transform(pts_sf, 4326)
  coords_ll <- sf::st_coordinates(pts_ll)
  center_lon <- mean(coords_ll[, 1], na.rm = TRUE)
  center_lat <- mean(coords_ll[, 2], na.rm = TRUE)

  equal_area_crs <- sprintf(
    "+proj=laea +lat_0=%.4f +lon_0=%.4f +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs",
    center_lat, center_lon
  )
  pts_eq <- sf::st_transform(pts_ll, equal_area_crs)

  ## 1. Extent of Occurrence (EOO) via Minimum Convex Polygon (MCP)
  hull_eq <- NULL
  eoo_km2 <- 0

  # Check distinct coordinates
  u_coords <- unique(sf::st_coordinates(pts_eq))
  if (nrow(u_coords) >= 3L) {
    union_pts <- sf::st_union(pts_eq)
    hull_eq <- sf::st_convex_hull(union_pts)
    eoo_area_m2 <- as.numeric(sf::st_area(hull_eq))
    eoo_km2 <- round(eoo_area_m2 / 1e6, 2)
  }

  hull_orig <- NULL
  if (!is.null(hull_eq) && !is.na(as.numeric(sf::st_area(hull_eq))) && as.numeric(sf::st_area(hull_eq)) > 0) {
    hull_geom <- sf::st_transform(hull_eq, orig_crs)
    hull_orig <- sf::st_sf(geometry = sf::st_sfc(hull_geom, crs = orig_crs))
  }

  hull_raw <- hull_orig
  eoo_raw_km2 <- eoo_km2
  is_constrained <- FALSE

  # Optional habitat boundary constraint (e.g. landmass for terrestrial, ocean for marine)
  if (!is.null(constrain_sf) && !is.null(hull_orig)) {
    try({
      c_geom <- sf::st_geometry(sf::st_make_valid(sf::st_transform(constrain_sf, orig_crs)))
      s2_was <- suppressMessages(suppressWarnings(sf::sf_use_s2(FALSE)))
      c_union <- suppressWarnings(sf::st_union(c_geom))
      poly_inter <- suppressWarnings(sf::st_intersection(sf::st_geometry(hull_orig), c_union))
      suppressMessages(suppressWarnings(sf::sf_use_s2(s2_was)))

      if (length(poly_inter) > 0L && !all(sf::st_is_empty(poly_inter))) {
        poly_inter_eq <- sf::st_transform(poly_inter, equal_area_crs)
        area_m2 <- as.numeric(sf::st_area(poly_inter_eq))
        if (!is.na(area_m2) && area_m2 > 0) {
          eoo_km2 <- round(area_m2 / 1e6, 2)
          hull_orig <- sf::st_sf(geometry = poly_inter, crs = orig_crs)
          is_constrained <- TRUE
        }
      }
    }, silent = TRUE)
  }

  ## 2. Area of Occupancy (AOO) via 2x2 km IUCN standard grid
  # Fast, exact integer grid index calculation (O(N) instead of materializing millions of empty polygons)
  cell_size_m <- aoo_cell_km * 1000
  coords_eq <- sf::st_coordinates(pts_eq)
  cell_indices <- unique(cbind(
    floor(coords_eq[, 1] / cell_size_m),
    floor(coords_eq[, 2] / cell_size_m)
  ))
  n_occupied <- nrow(cell_indices)
  aoo_km2 <- round(n_occupied * (aoo_cell_km^2), 2)

  ## 3. Preliminary IUCN Criterion B Status Evaluation
  # EOO Thresholds: CR < 100 km2 | EN < 5,000 km2 | VU < 20,000 km2 | LC >= 20,000 km2
  eoo_status <- if (eoo_km2 == 0) {
    "Data Deficient / Restricted (<3 distinct localities)"
  } else if (eoo_km2 < 100) {
    "Critically Endangered (CR)"
  } else if (eoo_km2 < 5000) {
    "Endangered (EN)"
  } else if (eoo_km2 < 20000) {
    "Vulnerable (VU)"
  } else {
    "Least Concern (LC)"
  }

  # AOO Thresholds: CR < 10 km2 | EN < 500 km2 | VU < 2,000 km2 | LC >= 2,000 km2
  aoo_status <- if (aoo_km2 < 10) {
    "Critically Endangered (CR)"
  } else if (aoo_km2 < 500) {
    "Endangered (EN)"
  } else if (aoo_km2 < 2000) {
    "Vulnerable (VU)"
  } else {
    "Least Concern (LC)"
  }

  # Overall most conservative status
  ranks <- c("Critically Endangered (CR)" = 1,
             "Endangered (EN)" = 2,
             "Vulnerable (VU)" = 3,
             "Least Concern (LC)" = 4)
  r_eoo <- ranks[eoo_status]
  r_aoo <- ranks[aoo_status]
  min_rank <- min(c(r_eoo, r_aoo), na.rm = TRUE)
  preliminary_status <- names(ranks)[ranks == min_rank][1]
  if (is.na(preliminary_status)) preliminary_status <- aoo_status

  eoo_desc <- if (is_constrained) {
    sprintf("%s km2 (constrained to habitat boundary; raw MCP: %s km2)",
            format(eoo_km2, big.mark = ","), format(eoo_raw_km2, big.mark = ","))
  } else {
    sprintf("%s km2", format(eoo_km2, big.mark = ","))
  }

  report <- sprintf(
    "IUCN Red List Range Evaluation (Criterion B):\n  - Extent of Occurrence (EOO): %s [%s]\n  - Area of Occupancy (AOO, 2x2 km grid): %s km2 (%d occupied cell(s)) [%s]\n  - Preliminary conservation status: %s (pending formal criteria B2a-c analysis)",
    eoo_desc, eoo_status,
    format(aoo_km2, big.mark = ","), n_occupied, aoo_status,
    preliminary_status
  )

  list(
    eoo_km2 = eoo_km2,
    eoo_raw_km2 = eoo_raw_km2,
    aoo_km2 = aoo_km2,
    n_aoo_cells = n_occupied,
    n_points = n_pts,
    eoo_status = eoo_status,
    aoo_status = aoo_status,
    preliminary_status = preliminary_status,
    polygon = if (return_polygon) hull_orig else NULL,
    polygon_raw = if (return_polygon) hull_raw else NULL,
    is_constrained = is_constrained,
    report = report
  )
}
