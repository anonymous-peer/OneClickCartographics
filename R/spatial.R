#' Read an occurrence table (CSV or Excel)
#'
#' Reads a tabular file and normalises the coordinate columns to `lat`/`lon`.
#' Latitude is taken from the first of `lat`, `latitude`, `decimalLatitude`;
#' longitude from the first of `lon`, `long`, `longitude`, `decimalLongitude`.
#' Rows with missing or out-of-range coordinates are dropped with a warning.
#'
#' @param path Character: path to a `.csv`, `.tsv`, `.txt`, `.xlsx` or `.xls` file.
#' @return A `data.frame` with `lat` and `lon` columns (numeric) plus all
#'   original columns.
#' @keywords internal
read_input <- function(path) {
  ext <- tolower(tools::file_ext(path))
  d <- if (ext %in% c("xlsx", "xls")) {
    as.data.frame(readxl::read_excel(path, sheet = 1), stringsAsFactors = FALSE)
  } else if (ext == "csv") {
    utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  } else if (ext %in% c("tsv", "txt")) {
    utils::read.delim(path, stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    stop("Unsupported input file type: ", ext, ". Use .csv, .tsv, .txt, .xlsx or .xls.")
  }
  standardize_input_df(d, path)
}

#' Standardize input data frame coordinates and columns
#'
#' @param d A data frame containing occurrence records.
#' @param source_name Character description of the data source for error messages.
#' @return A data frame with numeric `lat` and `lon` columns.
#' @keywords internal
standardize_input_df <- function(d, source_name = "input data") {
  d <- as.data.frame(d, stringsAsFactors = FALSE)
  # Latitude column: case-insensitive matching with common variants
  lat_col <- names(d)[tolower(names(d)) %in% tolower(c("lat", "latitude", "decimalLatitude"))][1]
  if (is.na(lat_col)) {
    # Broader fallback: any column containing 'lat' but not 'longi'
    lat_matches <- grep("lat", tolower(names(d)), value = TRUE)
    lat_col <- names(d)[tolower(names(d)) %in% tolower(lat_matches[!grepl("longi", lat_matches)])][1]
  }

  # Longitude column: case-insensitive matching with common variants
  lon_col <- names(d)[tolower(names(d)) %in% tolower(c("lon", "long", "longitude", "decimalLongitude"))][1]
  if (is.na(lon_col)) {
    # Broader fallback: any column containing 'lon' but not 'lat'
    lon_matches <- grep("lon", tolower(names(d)), value = TRUE)
    lon_col <- names(d)[tolower(names(d)) %in% tolower(lon_matches[!grepl("^lat", lon_matches)])][1]
  }

  # Species/grouping column: auto-detect from common variants
  species_variants <- c("species", "sp", "group", "grouping")
  sp_col <- names(d)[tolower(names(d)) %in% tolower(species_variants)][1]

  # If no species column matches variants, use first non-coordinate column
  if (is.na(sp_col) || sp_col == "NULL") {
    coord_cols <- unique(c(as.character(lat_col), as.character(lon_col)))
    sp_col <- setdiff(names(d), coord_cols)[1]
  }

  # Validation: must have both latitude and longitude
  if (is.null(lat_col) || is.na(lat_col) || is.null(lon_col) || is.na(lon_col)) {
    stop("Could not find latitude/longitude columns in ", source_name, ". ",
         "Available columns: ", paste(names(d), collapse = ", "), ".")
  }

  # Extract latitude and longitude columns
  d$lat <- suppressWarnings(as.numeric(d[[lat_col]]))
  d$lon <- suppressWarnings(as.numeric(d[[lon_col]]))

  # Set the species/grouping column if not already specified externally
  if (!is.null(sp_col) && !sp_col %in% names(d)) {
    sp_col <- NA_character_
  }
  bad <- is.na(d$lat) | is.na(d$lon) |
    d$lat < -90 | d$lat > 90 | d$lon < -180 | d$lon > 180
  if (any(bad)) {
    warning("Dropped ", sum(bad), " row(s) with missing or out-of-range coordinates.")
    d <- d[!bad, , drop = FALSE]
  }
  if (nrow(d) == 0L) stop("No valid coordinates remain after filtering.")
  d
}

#' Pick the UTM EPSG code for a longitude/latitude centroid
#'
#' @param lon Numeric longitude (degrees, WGS84).
#' @param lat Numeric latitude (degrees, WGS84).
#' @return Integer EPSG code of the local UTM zone (326xx north, 327xx south).
#' @keywords internal
auto_utm_epsg <- function(lon, lat) {
  zone <- floor((lon + 180) / 6) + 1
  zone <- min(max(zone, 1), 60)
  ifelse(lat >= 0, 32600 + zone, 32700 + zone)
}

#' WGS84 CRS with longitudes wrapped to 0-360
#'
#' Used to map data crossing the antimeridian (e.g. Pacific species ranges):
#' a PROJ `lon_wrap` transformation shifts all longitudes into `[0, 360)` so
#' the point cloud and the base layers share a continuous longitude space.
#'
#' @return An `sf` CRS object.
#' @keywords internal
wrap_crs <- function() {
  sf::st_crs("+proj=longlat +datum=WGS84 +lon_wrap=180 +no_defs")
}

#' Does the point cloud cross the antimeridian?
#'
#' @param points `sf` points in WGS84.
#' @return Logical: `TRUE` if the cloud spans the dateline (longitudes on both
#'   sides of 0 with a gap larger than 180 degrees).
#' @keywords internal
crosses_dateline <- function(points) {
  bb <- sf::st_bbox(points)
  bb[["xmin"]] < 0 && bb[["xmax"]] > 0 &&
    (bb[["xmax"]] - bb[["xmin"]]) > 180
}

#' Wrap longitudes of a WGS84 layer to 0-360
#'
#' @param x `sf` object in WGS84 (points or polygons).
#' @return The same geometries with longitudes in `[0, 360)`.
#' @keywords internal
wrap_lons <- function(x) {
  sf::st_transform(x, wrap_crs())
}

#' Great-circle span of a point cloud in kilometres
#'
#' Haversine distance between the two opposite corners of the bounding box,
#' a fast and deterministic proxy for the extent of the sample. Works on
#' wrapped (0-360) coordinates as long as the bbox is contiguous.
#'
#' @param points `sf` points (any longitude convention).
#' @return Numeric span in kilometres.
#' @keywords internal
point_span_km <- function(points) {
  bb <- sf::st_bbox(points)
  lat1 <- bb[["ymin"]] * pi / 180
  lat2 <- bb[["ymax"]] * pi / 180
  dlat <- lat2 - lat1
  dlon <- (bb[["xmax"]] - bb[["xmin"]]) * pi / 180
  a <- sin(dlat / 2)^2 + cos(lat1) * cos(lat2) * sin(dlon / 2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  6371.0088 * c
}

#' Map scale tier derived from the point span
#'
#' @param span_km Numeric span in kilometres (see [point_span_km()]).
#' @return Character: "local", "regional", "continental" or "global".
#' @keywords internal
scale_tier <- function(span_km) {
  if (span_km <= 300) "local"
  else if (span_km <= 2500) "regional"
  else if (span_km <= 8000) "continental"
  else "global"
}

#' Natural Earth scale for a map tier
#'
#' @param tier Character tier from [scale_tier()].
#' @return Integer Natural Earth scale (10, 50 or 110).
#' @keywords internal
tier_ne_scale <- function(tier) {
  switch(tier, local = 10, regional = 10, continental = 50, global = 110)
}

#' Automatic projection for a point cloud
#'
#' Picks the CRS best suited to the extent of the data:
#' UTM for local and regional spans, a Lambert Azimuthal Equal-Area projection
#' centred on the data for continental spans, and World Equidistant Cylindrical
#' (EPSG:4087) for global spans, polar data and dateline-crossing data.
#'
#' @param cent Numeric centroid `c(lon, lat)` in the (possibly wrapped) CRS.
#' @param span_km Numeric span in kilometres.
#' @param dateline Logical: data crosses the antimeridian.
#' @return Named list with `crs` (an `sf` CRS object), `epsg` (integer or
#'   `NULL` when no EPSG code exists) and `label` (human-readable description).
#' @keywords internal
auto_crs <- function(cent, span_km, dateline) {
  tier <- scale_tier(span_km)
  if (dateline) {
    list(crs = sf::st_crs(4087), epsg = 4087L,
         label = "EPSG:4087 (World Equidistant Cylindrical; dateline-crossing data)")
  } else if (abs(cent[2]) > 80) {
    epsg <- if (cent[2] > 0) 3413L else 3031L
    list(crs = sf::st_crs(epsg), epsg = epsg,
         label = paste0("EPSG:", epsg, " (polar stereographic)"))
  } else if (tier %in% c("local", "regional")) {
    zone <- auto_utm_epsg(cent[1], cent[2])
    list(crs = sf::st_crs(zone), epsg = as.integer(zone),
         label = paste0("UTM zone ", zone %% 100,
                        if (cent[2] >= 0) "N" else "S", " (EPSG:", zone, ")"))
  } else if (tier == "continental") {
    proj <- sprintf(paste0("+proj=laea +lat_0=%.4f +lon_0=%.4f ",
                           "+x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"),
                    cent[2], cent[1])
    list(crs = sf::st_crs(proj), epsg = NULL,
         label = "Lambert Azimuthal Equal-Area (centered on the data)")
  } else {
    list(crs = sf::st_crs(4087), epsg = 4087L,
         label = "EPSG:4087 (World Equidistant Cylindrical)")
  }
}

#' Expand a bounding box by a fraction of its largest dimension
#'
#' @param bbox Named numeric vector from `sf::st_bbox()`.
#' @param frac Fraction of the largest extent added on every side (default 0.03).
#' @param min_margin Minimum margin added if the bbox has zero or very small span.
#' @return Expanded named numeric vector.
#' @keywords internal
expand_bbox <- function(bbox, frac = 0.03, min_margin = NULL) {
  dx <- bbox[["xmax"]] - bbox[["xmin"]]
  dy <- bbox[["ymax"]] - bbox[["ymin"]]
  span <- max(dx, dy)

  if (is.null(min_margin)) {
    is_degrees <- max(abs(bbox)) <= 360
    min_margin <- if (is_degrees) 0.25 else 25000
  }

  m <- frac * span
  if (span < (2 * min_margin)) {
    half_diff_x <- max(m, (2 * min_margin - dx) / 2)
    half_diff_y <- max(m, (2 * min_margin - dy) / 2)
    c(xmin = bbox[["xmin"]] - half_diff_x,
      ymin = bbox[["ymin"]] - half_diff_y,
      xmax = bbox[["xmax"]] + half_diff_x,
      ymax = bbox[["ymax"]] + half_diff_y)
  } else {
    c(xmin = bbox[["xmin"]] - m, ymin = bbox[["ymin"]] - m,
      xmax = bbox[["xmax"]] + m, ymax = bbox[["ymax"]] + m)
  }
}

#' Densify a WGS84 bounding box into a polygon with intermediate points
#'
#' @param xmin,xmax,ymin,ymax Numeric coordinate limits in WGS84.
#' @param n Number of vertices along each of the four edges.
#' @return An `sfc` POLYGON in CRS 4326.
#' @keywords internal
densify_bbox_ll <- function(xmin, xmax, ymin, ymax, n = 50) {
  top <- data.frame(x = seq(xmin, xmax, length.out = n), y = ymax)
  right <- data.frame(x = xmax, y = seq(ymax, ymin, length.out = n)[-1])
  bottom <- data.frame(x = seq(xmax, xmin, length.out = n)[-1], y = ymin)
  left <- data.frame(x = xmin, y = seq(ymin, ymax, length.out = n)[-1])
  ring <- rbind(top, right, bottom, left)
  sf::st_sfc(sf::st_polygon(list(as.matrix(ring))), crs = 4326)
}

#' Densify a projected bounding box into a polygon with intermediate points
#'
#' @param bb Named numeric vector `c(xmin=..., ymin=..., xmax=..., ymax=...)`.
#' @param crs CRS object for the bounding box coordinates.
#' @param n Number of vertices along each of the four edges.
#' @return An `sfc` POLYGON in `crs`.
#' @keywords internal
densify_bbox_proj <- function(bb, crs, n = 50) {
  top <- data.frame(x = seq(bb[["xmin"]], bb[["xmax"]], length.out = n), y = bb[["ymax"]])
  right <- data.frame(x = bb[["xmax"]], y = seq(bb[["ymax"]], bb[["ymin"]], length.out = n)[-1])
  bottom <- data.frame(x = seq(bb[["xmax"]], bb[["xmin"]], length.out = n)[-1], y = bb[["ymin"]])
  left <- data.frame(x = bb[["xmin"]], y = seq(bb[["ymin"]], bb[["ymax"]], length.out = n)[-1])
  ring <- rbind(top, right, bottom, left)
  sf::st_sfc(sf::st_polygon(list(as.matrix(ring))), crs = crs)
}

#' Calculate a safe, expanded WGS84 bounding box for raster fetching
#'
#' Accurately captures curved projected parallels and adds a buffer so rasters
#' completely cover the projected frame without northern or edge cutoffs.
#'
#' @param bbox_sf Bounding box `sf` object in `crs_map`.
#' @param crs_map Map CRS.
#' @param buffer_ratio Fraction of extent added as buffer (default 0.08).
#' @return Named numeric vector `c(xmin, ymin, xmax, ymax)`.
#' @keywords internal
safe_raster_bbox_ll <- function(bbox_sf, crs_map, buffer_ratio = 0.08) {
  bb_p <- sf::st_bbox(bbox_sf)
  densified <- densify_bbox_proj(bb_p, crs = crs_map, n = 100)
  bb_ll <- sf::st_bbox(sf::st_transform(densified, 4326))
  dx <- bb_ll[["xmax"]] - bb_ll[["xmin"]]
  dy <- bb_ll[["ymax"]] - bb_ll[["ymin"]]
  buf_x <- max(0.2, dx * buffer_ratio)
  buf_y <- max(0.2, dy * buffer_ratio)
  c(xmin = max(-180, bb_ll[["xmin"]] - buf_x),
    ymin = max(-89, bb_ll[["ymin"]] - buf_y),
    xmax = min(180, bb_ll[["xmax"]] + buf_x),
    ymax = min(89, bb_ll[["ymax"]] + buf_y))
}

#' Calculate adaptive or user-specified stepping intervals and categories for grid counts
#'
#' Divides integer species richness or record counts into clean, intuitive bins
#' scaling from small numbers (e.g. 1, 2, 3) to hundreds of species/records.
#'
#' Supports:
#' - Default adaptive stepping intervals ("leave the default")
#' - User-defined number of categories (`n_categories`, `grid_categories`, or a single integer `user_steps`)
#' - User-defined observation width/step size across all categories (`step_size` / `grid_category_width`)
#' - User-defined observation intervals/breaks on each category independently (`breaks`, vector `user_steps`, or list of ranges)
#' - Quantile / equal-count binning (`method = "quantile"`)
#' - Custom category labels (`labels`)
#'
#' @param counts Integer or numeric vector of grid cell counts.
#' @param user_steps Optional numeric vector of break cutoffs, single integer number of bins, or list of range intervals.
#' @param max_bins Maximum number of bins for automatic binning (default 6).
#' @param n_categories Integer or `NULL`: exact number of categories desired.
#' @param step_size Numeric or `NULL`: fixed observation width/step size across all categories (e.g. `10` creates categories 1–10, 11–20, 21–30...).
#' @param breaks Numeric vector or list of 2-element ranges or `NULL`: cutoffs defining observations for each category independently.
#' @param method Character: classification method when `n_categories` is set (`"auto"`, `"pretty"`, `"equal_interval"`, or `"quantile"`).
#' @param labels Character vector or `NULL`: custom labels for categories.
#' @return Named list with `factor`, `labels`, `breaks`, `n_bins`, `method`.
#' @export
build_grid_bins <- function(counts,
                            user_steps = NULL,
                            max_bins = 6,
                            n_categories = NULL,
                            step_size = NULL,
                            breaks = NULL,
                            method = "auto",
                            labels = NULL) {
  if (length(counts) == 0L) return(NULL)
  min_val <- as.integer(min(counts, na.rm = TRUE))
  max_val <- as.integer(max(counts, na.rm = TRUE))

  if (max_val == min_val) {
    lbl <- if (!is.null(labels) && length(labels) >= 1L) labels[1] else as.character(min_val)
    fac <- factor(rep(lbl, length(counts)), levels = lbl)
    return(list(factor = fac, labels = lbl, breaks = c(min_val, min_val + 1L), n_bins = 1L, method = "single_value"))
  }

  # Normalize user_steps:
  # If a single integer >= 1 was provided, treat as n_categories
  if (is.null(n_categories)) {
    if (is.numeric(user_steps) && length(user_steps) == 1L && user_steps >= 1) {
      n_categories <- as.integer(user_steps)
      user_steps <- NULL
    }
  }

  # If numeric vector of length >= 2 or list provided as user_steps, treat as breaks
  if (is.null(breaks)) {
    if (is.numeric(user_steps) && length(user_steps) >= 2L) {
      breaks <- sort(unique(as.integer(user_steps)))
    } else if (is.list(user_steps)) {
      breaks <- user_steps
    }
  }

  calc_breaks <- NULL
  used_method <- "adaptive"

  # Case 1: Custom ranges list (independent categories defined as explicit intervals, e.g. list(c(1, 5), c(6, 20)))
  if (is.list(breaks)) {
    rng_list <- breaks
    n_b <- length(rng_list)
    cat_labels <- character(n_b)
    for (idx in seq_along(rng_list)) {
      rng <- rng_list[[idx]]
      if (!is.null(names(rng_list)) && nzchar(names(rng_list)[idx])) {
        cat_labels[idx] <- names(rng_list)[idx]
      } else if (length(rng) == 1L || rng[1] == rng[2]) {
        cat_labels[idx] <- as.character(rng[1])
      } else if (is.infinite(rng[2])) {
        cat_labels[idx] <- paste0(">", rng[1] - 1L)
      } else {
        cat_labels[idx] <- paste0(rng[1], "\u2013", rng[2])
      }
    }
    if (!is.null(labels) && length(labels) == n_b) {
      cat_labels <- labels
    }
    fac_vec <- rep(NA_character_, length(counts))
    for (i in seq_along(rng_list)) {
      rng <- rng_list[[i]]
      idx_match <- which(counts >= rng[1] & counts <= rng[2])
      fac_vec[idx_match] <- cat_labels[i]
    }
    # Ensure any values exceeding the highest range are safely included in the highest category
    if (any(is.na(fac_vec))) {
      unmatched_high <- which(is.na(fac_vec) & counts > rng_list[[n_b]][1])
      if (length(unmatched_high) > 0) {
        fac_vec[unmatched_high] <- cat_labels[n_b]
      }
    }
    bin_fac <- factor(fac_vec, levels = cat_labels)
    return(list(factor = bin_fac, labels = cat_labels, breaks = rng_list, n_bins = n_b, method = "custom_ranges"))
  }

  # Case 2: Fixed step_size across all categories (e.g. 10 observations per category across)
  if (!is.null(step_size) && is.numeric(step_size) && length(step_size) == 1L && step_size > 0) {
    s <- as.integer(step_size)
    if (!is.null(n_categories) && n_categories >= 1) {
      calc_breaks <- seq(min_val, by = s, length.out = n_categories + 1L)
      # Ensure max_val is covered by the top category
      if (max(calc_breaks) <= max_val) {
        calc_breaks[length(calc_breaks)] <- max_val + 1L
      }
    } else {
      calc_breaks <- seq(min_val, max_val + s, by = s)
      if (length(calc_breaks) >= 3L && calc_breaks[length(calc_breaks) - 1L] > max_val) {
        calc_breaks <- calc_breaks[-length(calc_breaks)]
      }
    }
    used_method <- "step_size"
  }

  # Case 3: Explicit numeric break cutoffs (independently defined intervals)
  if (is.null(calc_breaks) && !is.null(breaks) && is.numeric(breaks) && length(breaks) >= 2L) {
    b <- sort(unique(as.integer(breaks)))
    if (min(b) > min_val) b <- c(min_val, b)
    if (max(b) <= max_val) b <- c(b, max_val + 1L)
    calc_breaks <- b
    used_method <- "custom_breaks"
  }

  # Case 4: Number of categories (n_categories)
  if (is.null(calc_breaks) && !is.null(n_categories) && n_categories >= 1) {
    k <- as.integer(n_categories)
    if (k == 1L) {
      calc_breaks <- c(min_val, max_val + 1L)
      used_method <- "single_bin"
    } else if (method %in% c("quantile", "equal_count", "equal_frequency")) {
      probs <- seq(0, 1, length.out = k + 1L)
      q_brks <- unique(as.integer(stats::quantile(counts, probs = probs, type = 1, na.rm = TRUE)))
      if (length(q_brks) < k + 1L) {
        calc_breaks <- unique(as.integer(round(seq(min_val, max_val + 1L, length.out = k + 1L))))
      } else {
        if (max(q_brks) <= max_val) q_brks[length(q_brks)] <- max_val + 1L
        calc_breaks <- q_brks
      }
      used_method <- "quantile"
    } else if (method == "equal_interval") {
      calc_breaks <- unique(as.integer(round(seq(min_val, max_val + 1L, length.out = k + 1L))))
      used_method <- "equal_interval"
    } else {
      # "auto" or "pretty"
      p_brks <- pretty(c(min_val, max_val), n = k)
      p_brks <- p_brks[p_brks >= min_val]
      if (length(p_brks) == 0L || min(p_brks) > min_val) p_brks <- c(min_val, p_brks)
      if (max(p_brks) <= max_val) p_brks <- c(p_brks, max_val + 1L)
      calc_breaks <- unique(as.integer(p_brks))
      if (length(calc_breaks) - 1L != k) {
        calc_breaks <- unique(as.integer(round(seq(min_val, max_val + 1L, length.out = k + 1L))))
      }
      used_method <- "pretty"
    }
  }

  # Case 5: Default adaptive stepping ("leave the default")
  if (is.null(calc_breaks)) {
    used_method <- "adaptive"
    if (max_val <= 5L) {
      calc_breaks <- seq(min_val, max_val + 1L)
    } else if (max_val <= 12L) {
      calc_breaks <- unique(round(seq(min_val, max_val + 1L, length.out = 5L)))
    } else if (max_val <= 30L) {
      candidates <- c(min_val, 3L, 6L, 11L, 21L, max_val + 1L)
      calc_breaks <- sort(unique(c(candidates[candidates <= max_val], max_val + 1L)))
      if (length(calc_breaks) < 4L) calc_breaks <- unique(round(seq(min_val, max_val + 1L, length.out = 5L)))
    } else if (max_val <= 100L) {
      candidates <- c(min_val, 6L, 16L, 31L, 61L, max_val + 1L)
      calc_breaks <- sort(unique(c(candidates[candidates <= max_val], max_val + 1L)))
      if (length(calc_breaks) < 4L) calc_breaks <- unique(round(seq(min_val, max_val + 1L, length.out = 6L)))
    } else {
      p_brks <- pretty(c(min_val, max_val), n = max_bins)
      p_brks <- p_brks[p_brks >= min_val]
      if (length(p_brks) == 0L || min(p_brks) > min_val) p_brks <- c(min_val, p_brks)
      if (max(p_brks) <= max_val) p_brks <- c(p_brks, max_val + 1L)
      calc_breaks <- unique(as.integer(p_brks))
    }
  }

  calc_breaks <- unique(sort(as.integer(calc_breaks)))
  if (length(calc_breaks) < 2L) calc_breaks <- c(min_val, max_val + 1L)

  n_bins <- length(calc_breaks) - 1L
  gen_labels <- character(n_bins)
  for (i in seq_len(n_bins)) {
    low_b <- calc_breaks[i]
    high_b <- calc_breaks[i + 1L] - 1L
    if (low_b == high_b) {
      gen_labels[i] <- as.character(low_b)
    } else {
      gen_labels[i] <- paste0(low_b, "\u2013", high_b)
    }
  }

  final_labels <- if (!is.null(labels) && length(labels) == n_bins) labels else gen_labels
  bin_fac <- cut(counts, breaks = calc_breaks, right = FALSE, labels = final_labels, include.lowest = TRUE)
  list(factor = bin_fac, labels = final_labels, breaks = calc_breaks, n_bins = n_bins, method = used_method)
}