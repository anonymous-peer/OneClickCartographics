#' Cache directory for downloaded base layers
#'
#' Natural Earth layers, the ETOPO1 relief grid and the WWF ecoregions
#' shapefile are stored under the user cache directory, so repeated runs are
#' fast and work offline once downloaded.
#'
#' @return Character path to the package cache directory (created on demand).
#' @keywords internal
cache_dir <- function() {
  d <- tools::R_user_dir("OneClickCartographics", "cache")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

#' Download a file with a generous timeout
#'
#' `utils::download.file()` inherits the global `timeout` option (60 s by
#' default), which is too short for the large optional base layers. Raises
#' the timeout to 10 minutes for the download and restores it afterwards.
#'
#' @param url,path Character: source URL and destination file.
#' @return Invisibly the downloaded file path.
#' @keywords internal
download_with_timeout <- function(url, path) {
  old <- getOption("timeout")
  on.exit(options(timeout = old), add = TRUE)
  options(timeout = 600)
  utils::download.file(url, path, mode = "wb", quiet = TRUE)
  invisible(path)
}

#' Download and unzip a cached layer archive
#'
#' Downloads `url` into the package cache if not already present, then
#' unzips it into `destdir`. If the archive turns out to be corrupt (e.g. a
#' truncated download), it is deleted so the next call retries.
#'
#' @param url Character: source URL of the archive.
#' @param zip Character: cached path of the archive.
#' @param destdir Character: directory to unzip into.
#' @return Logical: `TRUE` if the archive was downloaded and unzipped
#'   successfully, `FALSE` otherwise.
#' @keywords internal
fetch_zip <- function(url, zip, destdir) {
  if (!file.exists(zip)) {
    ok <- try(download_with_timeout(url, zip), silent = TRUE)
    if (inherits(ok, "try-error")) return(FALSE)
  }
  if (!file.exists(zip) || file.info(zip)$size < 1000) return(FALSE)
  ok <- try(utils::unzip(zip, exdir = destdir), silent = TRUE)
  if (inherits(ok, "try-error")) {
    unlink(zip)
    return(FALSE)
  }
  TRUE
}

#' Fetch a Natural Earth layer, cropped to the map extent
#'
#' Wraps `rnaturalearth::ne_download()` (downloaded files are cached under the
#' package cache directory); returns `NULL` (with a message) if the layer is
#' unavailable or the download fails, so optional layers degrade gracefully.
#' The crop box is the true WGS84 envelope of the map frame (the frame edges
#' are densified before projecting, so the curved edges of the projected
#' rectangle are captured); cropping is done with planar geometry (`s2` off),
#' which is exact and avoids spherical cropping artefacts.
#'
#' @param scale Character: "10", "50" or "110".
#' @param type Character: Natural Earth layer type, e.g. "ocean", "lakes",
#'   "rivers_lake_centerlines", "admin_1_states_provinces", "populated_places",
#'   "roads".
#' @param category Character: "physical" or "cultural".
#' @param crs_map Map CRS (result of `sf::st_crs()`).
#' @param bbox_sf Bounding box as `sf` object in `crs_map`.
#' @param crop Logical: crop the layer to the bounding box (default `TRUE`);
#'   set to `FALSE` to keep the full world layer (used by `keep_medium`).
#' @return `sf` object in `crs_map`, cropped, or `NULL` on failure.
#' @keywords internal
safe_ne_fetch <- function(scale, type, category, crs_map, bbox_sf, crop = TRUE) {
  bb_margin <- safe_raster_bbox_ll(bbox_sf, crs_map, buffer_ratio = 0.05)
  bbox_ll <- sf::st_bbox(c(xmin = bb_margin[["xmin"]], ymin = bb_margin[["ymin"]],
                           xmax = bb_margin[["xmax"]], ymax = bb_margin[["ymax"]]),
                         crs = sf::st_crs(4326))
  x <- NULL
  try(x <- rnaturalearth::ne_download(scale = as.numeric(scale), type = type,
                                      category = category, returnclass = "sf",
                                      destdir = cache_dir()),
      silent = TRUE)
  if (is.null(x)) {
    message("  - layer '", type, "' download failed, skipped")
    return(NULL)
  }
  # Turn off s2 before st_make_valid to prevent spherical polygon inversion (which inverts global ocean to land)
  s2_was <- suppressMessages(suppressWarnings(sf::sf_use_s2(FALSE)))
  on.exit(suppressMessages(suppressWarnings(sf::sf_use_s2(s2_was))), add = TRUE)
  x <- tryCatch(sf::st_make_valid(x), error = function(e) x)
  if (crop) {
    y <- tryCatch(suppressWarnings(sf::st_crop(x, bbox_ll)), error = function(e) NULL)
    if (!is.null(y)) x <- y
  }
  out <- tryCatch(sf::st_transform(x, crs_map), error = function(e) NULL)
  if (is.null(out)) {
    message("  - layer '", type, "' projection failed, skipped")
    return(NULL)
  }
  out
}

#' WGS84 envelope of the map frame
#'
#' Densifies the (projected) frame rectangle and projects it to WGS84, so the
#' resulting bounding box captures the curved edges of the projected rectangle
#' (the `coord_sf` limits). The land/ocean layers are cropped to this envelope,
#' guaranteeing the map is covered by its base layers on all four sides.
#'
#' @param bbox_sf Bounding box as `sf` object in the map CRS.
#' @param crs_map Map CRS (optional, defaults to CRS of `bbox_sf`).
#' @return Numeric WGS84 bounding box (`xmin ymin xmax ymax`).
#' @keywords internal
frame_ll_bbox <- function(bbox_sf, crs_map = NULL) {
  eff_crs <- if (is.null(crs_map)) sf::st_crs(bbox_sf) else crs_map
  bb_p <- sf::st_bbox(bbox_sf)
  densified <- densify_bbox_proj(bb_p, crs = eff_crs, n = 100)
  as.numeric(sf::st_bbox(sf::st_transform(densified, 4326)))
}

#' Build the base map layers
#'
#' Fetches Natural Earth land, ocean, lakes, rivers, administrative-1
#' boundaries, populated places and roads, cropped to the map extent and
#' projected to the map CRS.
#'
#' @param scale Integer: Natural Earth scale (10, 50 or 110).
#' @param crs_map Map CRS.
#' @param bbox_sf Bounding box `sf` object in `crs_map`.
#' @param show_cities Logical: include populated places.
#' @param show_roads Logical: include roads.
#' @param admin_level Integer or `NULL`: administrative division level to
#'   draw in addition to the country borders -- 0 (countries only, default),
#'   1 (states/provinces), 2 (municipalities, scale 10 only) or `NULL`
#'   (no administrative boundaries, default).
#' @param show_water Logical: include bodies of water (ocean, lakes, rivers).
#'   Default `FALSE`.
#' @param show_ocean,show_lakes,show_rivers Logical or `NULL`: individual
#'   controls for water bodies. Inherit from `show_water` if `NULL`.
#' @return Named list of `sf` layers (some elements may be `NULL`).
#' @keywords internal
build_layers <- function(scale, crs_map, bbox_sf, show_cities, show_roads,
                         admin_level = NULL, show_water = FALSE,
                         show_ocean = NULL, show_lakes = NULL, show_rivers = NULL) {
  s <- as.numeric(scale)
  land <- safe_ne_fetch(s, "countries", "cultural", crs_map, bbox_sf)

  do_ocean <- if (!is.null(show_ocean)) isTRUE(show_ocean) else isTRUE(show_water)
  do_lakes <- if (!is.null(show_lakes)) isTRUE(show_lakes) else isTRUE(show_water)
  do_rivers <- if (!is.null(show_rivers)) isTRUE(show_rivers) else isTRUE(show_water)

  ocean <- if (do_ocean) safe_ne_fetch(s, "ocean", "physical", crs_map, bbox_sf) else NULL
  lakes <- if (do_lakes) {
    lk <- safe_ne_fetch(s, "lakes", "physical", crs_map, bbox_sf)
    lk_na <- safe_ne_fetch(10, "lakes_north_america", "physical", crs_map, bbox_sf)
    if (!is.null(lk_na) && nrow(lk_na) > 0L) {
      if (is.null(lk) || nrow(lk) == 0L) {
        lk <- lk_na
      } else {
        common_cols <- intersect(names(lk), names(lk_na))
        if (length(common_cols) > 0) {
          lk <- rbind(lk[, common_cols, drop = FALSE], lk_na[, common_cols, drop = FALSE])
        }
      }
    }
    lk
  } else NULL
  rivers <- if (do_rivers) {
    riv <- safe_ne_fetch(s, "rivers_lake_centerlines", "physical", crs_map, bbox_sf)
    riv_na <- safe_ne_fetch(10, "rivers_north_america", "physical", crs_map, bbox_sf)
    if (!is.null(riv_na) && nrow(riv_na) > 0L) {
      if (is.null(riv) || nrow(riv) == 0L) {
        riv <- riv_na
      } else {
        common_cols <- intersect(names(riv), names(riv_na))
        if (length(common_cols) > 0) {
          riv <- rbind(riv[, common_cols, drop = FALSE], riv_na[, common_cols, drop = FALSE])
        }
      }
    }
    if (is.null(riv) || nrow(riv) < 5L) {
      tryCatch({
        bb_margin <- safe_raster_bbox_ll(bbox_sf, crs_map, buffer_ratio = 0.05)
        bbox_ll <- sf::st_bbox(c(xmin = bb_margin[["xmin"]], ymin = bb_margin[["ymin"]],
                                 xmax = bb_margin[["xmax"]], ymax = bb_margin[["ymax"]]),
                               crs = sf::st_crs(4326))
        iso_candidates <- if (!is.null(land)) unique(stats::na.omit(c(land$ISO_A3, land$ADM0_A3, land$adm0_a3, land$sov_a3))) else character(0)
        iso_candidates <- iso_candidates[nchar(iso_candidates) == 3 & !iso_candidates %in% c("-99", "USA", "CAN")]
        osm_riv_list <- list()
        for (iso in iso_candidates) {
          iso_low <- tolower(iso)
          c_gpkg <- file.path(cache_dir(), paste0("osm_", iso_low, "_rivers_main.gpkg"))
          if (!file.exists(c_gpkg)) {
            c_gpkg_alt <- file.path(cache_dir(), paste0("osm_", iso_low, "_rivers.gpkg"))
            if (file.exists(c_gpkg_alt)) c_gpkg <- c_gpkg_alt
          }
          if (!file.exists(c_gpkg)) {
            s3_url <- paste0("https://production-raw-data-api.s3.amazonaws.com/ISO3/", toupper(iso), "/waterways/hotosm_", iso_low, "_waterways_osm_geojson.zip")
            tmp_z <- file.path(cache_dir(), paste0("hotosm_", iso_low, "_waterways.zip"))
            dl_ok <- tryCatch({
              utils::download.file(s3_url, tmp_z, quiet = TRUE)
              TRUE
            }, error = function(e) FALSE)
            if (dl_ok && file.exists(tmp_z)) {
              ex_files <- utils::unzip(tmp_z, list = TRUE)$Name
              gj_name <- grep("\\.geojson$", ex_files, value = TRUE)[1]
              if (!is.na(gj_name)) {
                utils::unzip(tmp_z, files = gj_name, exdir = cache_dir())
                gj_path <- file.path(cache_dir(), gj_name)
                ww_raw <- tryCatch(sf::st_read(gj_path, quiet = TRUE), error = function(e) NULL)
                unlink(gj_path)
                if (!is.null(ww_raw) && "waterway" %in% names(ww_raw)) {
                  ww_riv <- ww_raw[ww_raw$waterway == "river", ]
                  if (nrow(ww_riv) > 0) {
                    tryCatch(sf::st_write(ww_riv, c_gpkg, delete_dsn = TRUE, quiet = TRUE), error = function(e) NULL)
                  }
                }
              }
              unlink(tmp_z)
            }
          }
          if (file.exists(c_gpkg)) {
            r_sf <- tryCatch(sf::st_read(c_gpkg, quiet = TRUE), error = function(e) NULL)
            if (!is.null(r_sf) && nrow(r_sf) > 0) {
              s2_was <- suppressMessages(suppressWarnings(sf::sf_use_s2(FALSE)))
              r_crop <- tryCatch(suppressWarnings(sf::st_crop(r_sf, bbox_ll)), error = function(e) NULL)
              suppressMessages(suppressWarnings(sf::sf_use_s2(s2_was)))
              if (!is.null(r_crop) && nrow(r_crop) > 0) {
                r_trans <- tryCatch(sf::st_transform(r_crop, crs_map), error = function(e) NULL)
                if (!is.null(r_trans) && nrow(r_trans) > 0) {
                  osm_riv_list[[iso]] <- r_trans
                }
              }
            }
          }
        }
        if (length(osm_riv_list) > 0) {
          all_osm_riv <- do.call(rbind, lapply(osm_riv_list, function(x) {
            col_keep <- intersect(c("name", "waterway", "geometry"), names(x))
            x[, col_keep, drop = FALSE]
          }))
          riv <- all_osm_riv
        }
      }, error = function(e) NULL)
    }
    riv
  } else NULL
  admin1 <- NULL
  if (!is.null(admin_level) && admin_level >= 1) {
    # Use scale 10 for admin-1 whenever possible, because Natural Earth 50m only
    # includes USA and Canada, whereas 10m provides global state/province/department
    # boundaries worldwide (including Mexico, Colombia, Brazil, Argentina, etc.).
    admin1 <- safe_ne_fetch(10, "admin_1_states_provinces", "cultural", crs_map, bbox_sf)
    if (is.null(admin1) && s <= 50) {
      admin1 <- safe_ne_fetch(s, "admin_1_states_provinces", "cultural", crs_map, bbox_sf)
    }
  }
  # Level 1: states/provinces/departments (Natural Earth 10m provides global coverage)
  # Level 2 (counties/municipalities) is standardized to Level 1 for global consistency
  admin2 <- NULL
  cities <- NULL
  if (isTRUE(show_cities) || (!is.null(show_cities) && !identical(show_cities, FALSE))) {
    cities <- safe_ne_fetch(10, "populated_places", "cultural", crs_map, bbox_sf)
    if (is.null(cities) && s <= 50) {
      cities <- safe_ne_fetch(s, "populated_places", "cultural", crs_map, bbox_sf)
    }
    if (!is.null(cities)) {
      name_col <- intersect(c("NAME", "NAME_EN", "name"), names(cities))[1]
      if (!is.na(name_col)) cities$city_name <- cities[[name_col]]
      if ("POP_MAX" %in% names(cities)) {
        cities <- cities[order(cities$POP_MAX, decreasing = TRUE), ]
        max_c <- if (is.numeric(show_cities) && show_cities > 0) as.integer(show_cities) else 25L
        cities <- cities[seq_len(min(nrow(cities), max_c)), ]
      }
    }
  }
  roads <- NULL
  roads_lvl <- 0L
  if (isTRUE(show_roads)) {
    roads_lvl <- 1L
  } else if (is.numeric(show_roads)) {
    roads_lvl <- as.integer(show_roads)
  } else if (is.character(show_roads)) {
    r_low <- tolower(trimws(show_roads))
    if (r_low %in% c("major", "primary", "1", "highways")) {
      roads_lvl <- 1L
    } else if (r_low %in% c("all", "secondary", "2", "both", "full")) {
      roads_lvl <- 2L
    }
  }
  if (roads_lvl > 0L) {
    # Natural Earth global roads (10m scale)
    roads <- safe_ne_fetch(10, "roads", "cultural", crs_map, bbox_sf)

    # Supplementary North America roads if within NA bounding box
    roads_na <- safe_ne_fetch(10, "roads_north_america", "cultural", crs_map, bbox_sf)
    if (!is.null(roads_na) && nrow(roads_na) > 0L) {
      if (is.null(roads) || nrow(roads) == 0L) {
        roads <- roads_na
      } else {
        common_cols <- intersect(names(roads), names(roads_na))
        if (length(common_cols) > 0L) {
          roads <- rbind(roads[, common_cols, drop = FALSE], roads_na[, common_cols, drop = FALSE])
        }
      }
    }

    if (!is.null(roads) && nrow(roads) > 0L) {
      if ("type" %in% names(roads)) {
        # Exclude marine ferry routes
        ferry_idx <- tolower(roads$type) %in% c("ferry route", "ferry, seasonal", "ferry")
        terrestrial_roads <- roads[!ferry_idx, , drop = FALSE]

        if (roads_lvl == 1L) {
          # Level 1: Major highways, expressways, freeways, primary arteries, and national secondary highways
          l1_types <- c("Major Highway", "Beltway", "Bypass", "Expressway", "Freeway",
                        "Tollway", "Primary", "Major Road", "Secondary Highway")
          cand <- terrestrial_roads[terrestrial_roads$type %in% l1_types, , drop = FALSE]
          # Fallback: if region has no roads under those specific type names, keep all available terrestrial roads
          roads <- if (nrow(cand) > 0L) cand else terrestrial_roads
        } else {
          # Level 2 / all: All terrestrial roads (including local roads, unknown classes, and tracks)
          roads <- terrestrial_roads
        }
      }
      if (nrow(roads) == 0L) roads <- NULL
    } else {
      roads <- NULL
    }
  }
  list(land = land, ocean = ocean, lakes = lakes, rivers = rivers,
       admin1 = admin1, admin2 = admin2, cities = cities, roads = roads)
}

#' Optional terrain hillshade raster
#'
#' Downloads an elevation tile with `elevatr` (AWS Terrain Tiles) or ETOPO1 and
#' computes a hillshade with `terra`. Requires internet access on first use.
#'
#' @param bbox_sf Bounding box `sf` object in the map CRS.
#' @param crs_map Map CRS.
#' @param zoom Integer: elevatr zoom level (0-14; 6 is a good default).
#' @param span_km Numeric span of data in km.
#' @return A `data.frame` with columns `x`, `y`, `layer` for `geom_raster()`,
#'   or `NULL` on failure.
#' @keywords internal
terrain_layer <- function(bbox_sf, crs_map, zoom, span_km = 1000, scope = "both") {
  if (!requireNamespace("terra", quietly = TRUE)) {
    message("  - terra not installed, terrain skipped")
    return(NULL)
  }
  rt <- NULL
  if (requireNamespace("elevatr", quietly = TRUE)) {
    rt <- tryCatch({
      bb <- safe_raster_bbox_ll(bbox_sf, crs_map, buffer_ratio = 0.10)
      loc <- sf::st_bbox(c(xmin = bb[["xmin"]], ymin = bb[["ymin"]],
                           xmax = bb[["xmax"]], ymax = bb[["ymax"]]),
                         crs = sf::st_crs(4326))
      r <- elevatr::get_elev_raster(locations = loc, z = zoom, src = "aws",
                                    prj = sf::st_crs(crs_map)$wkt,
                                    verbose = FALSE)
      terra::rast(r)
    }, error = function(e) NULL)
  }

  if (is.null(rt)) {
    tif <- file.path(cache_dir(), "ETOPO1_Ice_g_geotiff.tif")
    if (file.exists(tif)) {
      try({
        r <- terra::rast(tif)
        bb <- safe_raster_bbox_ll(bbox_sf, crs_map, buffer_ratio = 0.10)
        rc <- terra::crop(r, terra::ext(bb[["xmin"]], bb[["xmax"]],
                                        bb[["ymin"]], bb[["ymax"]]))
        res <- max(2000, span_km * 1000 / 400)
        rt <- terra::project(rc, sf::st_crs(crs_map)$wkt, res = res)
      }, silent = TRUE)
    }
  }

  if (is.null(rt)) {
    message("  - terrain elevation source unavailable, terrain skipped")
    return(NULL)
  }

  if (scope == "land") {
    rt[rt < 0] <- NA
  } else if (scope == "sea") {
    rt[rt > 0] <- NA
  }

  tryCatch({
    tr <- terra::terrain(rt, c("slope", "aspect"), unit = "radians")
    sh <- terra::shade(tr$slope, tr$aspect, angle = 45, direction = 315)
    df <- as.data.frame(sh, xy = TRUE, na.rm = TRUE)
    names(df) <- c("x", "y", "layer")
    df
  }, error = function(e) {
    message("  - terrain shading failed, skipped (", conditionMessage(e), ")")
    NULL
  })
}

#' Optional elevation raster (altitude map)
#'
#' Fetches an elevation/depth raster and returns the values as a data frame
#' for `geom_raster()`. Supports "land" only, "sea" only, or "both"
#' (land altitude + ocean bathymetry).
#'
#' @param bbox_sf Bounding box `sf` object in the map CRS.
#' @param crs_map Map CRS.
#' @param zoom Integer: elevatr zoom level (0-14; 6 is a good default).
#' @param land `sf` land polygons in `crs_map`, or `NULL`.
#' @param scope Character: "land" (default), "sea", or "both".
#' @param span_km Numeric span of data in km.
#' @return A `data.frame` with columns `x`, `y`, `value` (elevation in
#'   metres), or `NULL` on failure.
#' @keywords internal
elevation_layer <- function(bbox_sf, crs_map, zoom, land = NULL, scope = "land", span_km = 1000) {
  if (scope %in% c("sea", "both")) {
    return(topo_layer(bbox_sf, crs_map, span_km, scope = scope))
  }

  if (requireNamespace("elevatr", quietly = TRUE) &&
      requireNamespace("terra", quietly = TRUE)) {
    elev_res <- tryCatch({
      bb <- safe_raster_bbox_ll(bbox_sf, crs_map, buffer_ratio = 0.10)
      loc <- sf::st_bbox(c(xmin = bb[["xmin"]], ymin = bb[["ymin"]],
                           xmax = bb[["xmax"]], ymax = bb[["ymax"]]),
                         crs = sf::st_crs(4326))
      r <- elevatr::get_elev_raster(locations = loc, z = zoom, src = "aws",
                                    prj = sf::st_crs(crs_map)$wkt,
                                    verbose = FALSE)
      rt <- terra::rast(r)
      if (!is.null(land)) {
        v <- terra::vect(sf::st_union(sf::st_make_valid(land)))
        rt <- terra::mask(rt, v)
      }
      rt[rt < 0] <- NA
      df <- as.data.frame(rt, xy = TRUE, na.rm = TRUE)
      names(df) <- c("x", "y", "value")
      df
    }, error = function(e) NULL)
    if (!is.null(elev_res) && nrow(elev_res) > 0) return(elev_res)
  }

  topo_layer(bbox_sf, crs_map, span_km, scope = "land")
}

#' Optional land + ocean topography raster
#'
#' Downloads the ETOPO1 global relief grid (1 arc-minute, ice surface,
#' NOAA/NGDC; cached under the package cache directory) once, then crops it
#' to the map extent, projects it to the map CRS and returns it as a data
#' frame for `geom_raster()`. Supports "both" (default), "land", or "sea".
#' Requires internet on first use.
#'
#' @param bbox_sf Bounding box `sf` object in the map CRS.
#' @param crs_map Map CRS.
#' @param span_km Numeric span of the data in km (controls the output raster
#'   resolution so global maps stay small).
#' @param scope Character: "both" (default), "land", or "sea".
#' @return A `data.frame` with columns `x`, `y`, `value` (elevation in
#'   metres; negative over the ocean), or `NULL` on failure.
#' @keywords internal
topo_layer <- function(bbox_sf, crs_map, span_km, scope = "both") {
  if (!requireNamespace("terra", quietly = TRUE)) {
    message("  - terra not installed, topography skipped")
    return(NULL)
  }
  tif <- file.path(cache_dir(), "ETOPO1_Ice_g_geotiff.tif")
  if (!file.exists(tif)) {
    zip <- file.path(cache_dir(), "ETOPO1_Ice_g_geotiff.zip")
    url <- paste0("https://www.ngdc.noaa.gov/mgg/global/relief/ETOPO1/data/",
                  "ice_surface/grid_registered/georeferenced_tiff/",
                  "ETOPO1_Ice_g_geotiff.zip")
    ok <- fetch_zip(url, zip, cache_dir())
    if (!ok) {
      message("  - ETOPO1 download failed, topography skipped")
      return(NULL)
    }
  }
  if (!file.exists(tif)) {
    message("  - ETOPO1 grid not found after download, topography skipped")
    return(NULL)
  }
  tryCatch({
    r <- terra::rast(tif)
    bb <- safe_raster_bbox_ll(bbox_sf, crs_map, buffer_ratio = 0.10)
    rc <- terra::crop(r, terra::ext(bb[["xmin"]], bb[["xmax"]],
                                    bb[["ymin"]], bb[["ymax"]]))
    res <- max(2000, span_km * 1000 / 400)
    rp <- terra::project(rc, sf::st_crs(crs_map)$wkt, res = res)

    if (scope == "land") {
      rp[rp < 0] <- NA
    } else if (scope == "sea") {
      rp[rp > 0] <- NA
    }

    df <- as.data.frame(rp, xy = TRUE, na.rm = TRUE)
    names(df) <- c("x", "y", "value")
    df
  }, error = function(e) {
    message("  - topography failed, skipped (", conditionMessage(e), ")")
    NULL
  })
}

#' Calculate clean elevation and depth scale breaks
#'
#' Computes well-spaced scale breaks for continuous elevation and bathymetry
#' colorbars. Ensures that extreme minimum and maximum values are pinned, zero
#' (sea level) is explicitly included when spanning both land and ocean, and
#' intermediate breaks avoid collisions.
#'
#' @param min_v Numeric minimum elevation/depth value.
#' @param max_v Numeric maximum elevation/depth value.
#' @param scope_val Character: "land", "sea", or "both".
#' @return Numeric vector of sorted scale breaks.
#' @keywords internal
calc_elevation_scale_breaks <- function(min_v, max_v, scope_val = "both") {
  min_extreme <- round(min_v)
  max_extreme <- round(max_v)

  if (identical(scope_val, "land") || (min_extreme >= 0)) {
    low_val <- max(0, min_extreme)
    span <- max_extreme - low_val
    if (span <= 0) return(c(low_val, max_extreme))
    cand <- pretty(c(low_val, max_extreme), n = 5)
    cand <- cand[cand > low_val & cand < max_extreme]
    min_dist <- 0.15 * span
    cand <- cand[cand >= (low_val + min_dist) & cand <= (max_extreme - min_dist)]
    if (length(cand) > 1) {
      keep_c <- cand[1]
      for (cv in cand[-1]) {
        if (cv - tail(keep_c, 1) >= min_dist) keep_c <- c(keep_c, cv)
      }
      cand <- keep_c
    }
    breaks <- sort(unique(c(low_val, cand, max_extreme)))
  } else if (identical(scope_val, "sea") || (max_extreme <= 0)) {
    high_val <- min(0, max_extreme)
    span <- high_val - min_extreme
    if (span <= 0) return(c(min_extreme, high_val))
    cand <- pretty(c(min_extreme, high_val), n = 5)
    cand <- cand[cand > min_extreme & cand < high_val]
    min_dist <- 0.15 * span
    cand <- cand[cand >= (min_extreme + min_dist) & cand <= (high_val - min_dist)]
    if (length(cand) > 1) {
      keep_c <- cand[1]
      for (cv in cand[-1]) {
        if (cv - tail(keep_c, 1) >= min_dist) keep_c <- c(keep_c, cv)
      }
      cand <- keep_c
    }
    breaks <- sort(unique(c(min_extreme, cand, high_val)))
  } else {
    total_span <- max_extreme - min_extreme

    # Sea candidates
    sea_span <- -min_extreme
    cand_sea <- pretty(c(min_extreme, 0), n = 4)
    cand_sea <- cand_sea[cand_sea > min_extreme & cand_sea < 0]
    min_dist_sea <- max(0.09 * total_span, 0.15 * sea_span)
    cand_sea <- cand_sea[cand_sea >= (min_extreme + min_dist_sea) & cand_sea <= (-min_dist_sea)]
    if (length(cand_sea) > 1) {
      keep_s <- cand_sea[1]
      for (sv in cand_sea[-1]) {
        if (sv - tail(keep_s, 1) >= min_dist_sea) keep_s <- c(keep_s, sv)
      }
      cand_sea <- keep_s
    }

    # Land candidates
    land_span <- max_extreme
    cand_land <- pretty(c(0, max_extreme), n = 4)
    cand_land <- cand_land[cand_land > 0 & cand_land < max_extreme]
    min_dist_land <- max(0.09 * total_span, 0.15 * land_span)
    cand_land <- cand_land[cand_land >= min_dist_land & cand_land <= (max_extreme - min_dist_land)]
    if (length(cand_land) > 1) {
      keep_l <- cand_land[1]
      for (lv in cand_land[-1]) {
        if (lv - tail(keep_l, 1) >= min_dist_land) keep_l <- c(keep_l, lv)
      }
      cand_land <- keep_l
    }

    breaks <- sort(unique(c(min_extreme, cand_sea, 0, cand_land, max_extreme)))
  }

  breaks
}

#' Optional WWF terrestrial ecoregions layer
#'
#' Downloads the WWF Terrestrial Ecoregions of the World (Olson et al. 2001,
#' "official_teow"; cached under the package cache directory), crops it to
#' the map extent and projects it to the map CRS, to be drawn as thin
#' outlines under the occurrence points. Requires internet on first use.
#'
#' @param bbox_sf Bounding box `sf` object in the map CRS.
#' @param crs_map Map CRS.
#' @return `sf` object in `crs_map`, cropped, or `NULL` on failure.
#' @keywords internal
ecoregions_layer <- function(bbox_sf, crs_map) {
  zip <- file.path(cache_dir(), "official_teow.zip")
  dir <- file.path(cache_dir(), "ecoregions")
  shp <- file.path(dir, "official", "wwf_terr_ecos.shp")
  if (!file.exists(shp)) {
    ok <- fetch_zip(
      "https://c402277.ssl.cf1.rackcdn.com/publications/15/files/original/official_teow.zip",
      zip, dir)
    if (!ok) {
      message("  - ecoregions download failed, skipped")
      return(NULL)
    }
  }
  if (!file.exists(shp)) {
    message("  - ecoregions shapefile not found after download, skipped")
    return(NULL)
  }
  tryCatch({
    x <- sf::st_read(shp, quiet = TRUE)
    x <- sf::st_make_valid(x)
    bbox_ll <- sf::st_bbox(sf::st_transform(
      sf::st_segmentize(bbox_sf, dfMaxLength = 50000), 4326))
    s2_was <- suppressMessages(suppressWarnings(sf::sf_use_s2(FALSE)))
    on.exit(suppressMessages(suppressWarnings(sf::sf_use_s2(s2_was))))
    y <- tryCatch(suppressWarnings(sf::st_crop(x, bbox_ll)), error = function(e) NULL)
    if (!is.null(y)) x <- y
    sf::st_transform(x, crs_map)
  }, error = function(e) {
    message("  - ecoregions layer failed, skipped (", conditionMessage(e), ")")
    NULL
  })
}

#' Internal helper: Fetch and import WDPA dataset for a single country
#'
#' Uses a multi-strategy approach:
#' 1. Checks local cache for pre-downloaded WDPA archive.
#' 2. Direct Protected Planet API (`POST /downloads`) for instant CloudFront retrieval (bypasses browser dependencies).
#' 3. Direct canonical CloudFront CDN URL fallback.
#' 4. Standard `wdpar::wdpa_fetch()` fallback.
#'
#' @param code Character: ISO3 country code (e.g. "CUB", "COL", "USA").
#' @param wdpa_dir Character: directory path to store cached WDPA datasets.
#' @return An `sf` data.frame or `NULL`.
#' @keywords internal
safe_wdpa_fetch_country <- function(code, wdpa_dir) {
  code <- toupper(trimws(code))
  if (nchar(code) != 3) {
    iso <- suppressWarnings(countrycode::countrycode(code, "country.name", "iso3c"))
    if (!is.na(iso)) code <- toupper(iso)
  }

  # 1. Check local cache
  cached_zips <- list.files(wdpa_dir, pattern = paste0(".*_", code, ".*\\.zip$"), full.names = TRUE)
  if (length(cached_zips) > 0L) {
    pa_item <- tryCatch(wdpar::wdpa_read(cached_zips[1]), error = function(e) NULL)
    if (!is.null(pa_item) && nrow(pa_item) > 0L) return(pa_item)
  }

  # 2. Strategy 1: Direct Protected Planet API (instantaneous, no chromote/browser required)
  for (fmt in c("gdb", "shp")) {
    endpoint <- "https://www.protectedplanet.net/downloads"
    body <- paste0("{\"domain\":\"general\",\"format\":\"", fmt, "\",\"token\":\"", code, "\"}")
    h <- curl::new_handle()
    curl::handle_setheaders(h, "Content-Type" = "application/json", "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)")
    curl::handle_setopt(h, post = TRUE, postfields = body, timeout = 12)
    res <- tryCatch(curl::curl_fetch_memory(endpoint, handle = h), error = function(e) NULL)
    if (!is.null(res) && res$status_code == 200) {
      json <- tryCatch(jsonlite::fromJSON(rawToChar(res$content)), error = function(e) NULL)
      if (!is.null(json) && !is.null(json$url) && nzchar(json$url) && startsWith(json$url, "http")) {
        dl_url <- json$url
        cur_ver <- tryCatch(wdpar:::wdpa_latest_version(), error = function(e) format(Sys.Date(), "%b%Y"))
        dest_file <- file.path(wdpa_dir, paste0("WDPA_", cur_ver, "_", code, if (fmt == "shp") "_shp" else "", ".zip"))
        dl_ok <- tryCatch({
          curl::curl_download(dl_url, dest_file, quiet = TRUE)
          file.exists(dest_file) && file.info(dest_file)$size > 1000
        }, error = function(e) {
          tryCatch({
            utils::download.file(dl_url, dest_file, mode = "wb", quiet = TRUE)
            file.exists(dest_file) && file.info(dest_file)$size > 1000
          }, error = function(e2) FALSE)
        })
        if (isTRUE(dl_ok)) {
          pa_item <- tryCatch(wdpar::wdpa_read(dest_file), error = function(e) NULL)
          if (!is.null(pa_item) && nrow(pa_item) > 0L) return(pa_item)
        }
      }
    }
  }

  # 3. Strategy 2: Direct canonical CloudFront CDN URL fallback
  cur_ver <- tryCatch(wdpar:::wdpa_latest_version(), error = function(e) format(Sys.Date(), "%b%Y"))
  for (fmt in c("", "_shp")) {
    dl_url <- paste0("https://d1gam3xoknrgr2.cloudfront.net/current/WDPA_WDOECM_", cur_ver, "_Public_", code, fmt, ".zip")
    dest_file <- file.path(wdpa_dir, paste0("WDPA_", cur_ver, "_", code, fmt, ".zip"))
    dl_ok <- tryCatch({
      curl::curl_download(dl_url, dest_file, quiet = TRUE)
      file.exists(dest_file) && file.info(dest_file)$size > 1000
    }, error = function(e) FALSE)
    if (isTRUE(dl_ok)) {
      pa_item <- tryCatch(wdpar::wdpa_read(dest_file), error = function(e) NULL)
      if (!is.null(pa_item) && nrow(pa_item) > 0L) return(pa_item)
    }
  }

  # 4. Strategy 3: Standard wdpar::wdpa_fetch fallback
  pa_item <- tryCatch({
    wdpar::wdpa_fetch(code, wait = FALSE, download_dir = wdpa_dir, page_wait = 5, verbose = FALSE)
  }, error = function(e) {
    tryCatch({
      wdpar::wdpa_fetch(code, wait = TRUE, download_dir = wdpa_dir, page_wait = 4, verbose = FALSE)
    }, error = function(e2) NULL)
  })
  if (!is.null(pa_item) && nrow(pa_item) > 0L) return(pa_item)

  NULL
}

#' Load and Crop Protected Areas (WDPA or User-Supplied Layer)
#'
#' Automatically retrieves and cleans protected areas from the World Database on
#' Protected Areas (WDPA) via UNEP-WCMC using direct cloud CDN downloads and
#' the `wdpar` package, or loads a user-supplied spatial vector file (Shapefile,
#' GeoPackage, GeoJSON) or `sf` object.
#'
#' @param bbox_sf Bounding box `sf` object in the map CRS.
#' @param crs_map Map CRS.
#' @param pts Occurrence points `sf` object (optional, for auto-detecting countries).
#' @param source Optional user-supplied file path, `sf` object, country ISO3 code or country name.
#' @return An `sf` polygon object in `crs_map` cropped to the bounding box, or `NULL` on failure.
#' @keywords internal
#' @export
protected_areas_layer <- function(bbox_sf, crs_map, pts = NULL, source = NULL) {
  pa_raw <- NULL

  # 1. Custom user-supplied sf object
  if (inherits(source, "sf")) {
    pa_raw <- source
  } else if (is.character(source) && length(source) == 1L && file.exists(source)) {
    # 2. Custom local file path
    pa_raw <- tryCatch(sf::st_read(source, quiet = TRUE), error = function(e) {
      message("  - failed to read protected areas file: ", source)
      NULL
    })
  }

  # 3. Automated WDPA fetch if pa_raw is still NULL
  if (is.null(pa_raw)) {
    if (!requireNamespace("wdpar", quietly = TRUE)) {
      message("  - wdpar package is required for automated protected areas retrieval, skipped")
      return(NULL)
    }

    country_codes <- NULL
    # If source is explicit character vector of country codes/names (not a file)
    if (is.character(source) && length(source) >= 1L && !file.exists(source[1])) {
      country_codes <- source
    }

    # Normalize user-supplied country codes or names
    if (!is.null(country_codes) && length(country_codes) > 0L) {
      norm_codes <- character(0)
      for (cc in country_codes) {
        cc_clean <- trimws(cc)
        if (nchar(cc_clean) == 3) {
          norm_codes <- c(norm_codes, toupper(cc_clean))
        } else {
          iso <- suppressWarnings(countrycode::countrycode(cc_clean, "country.name", "iso3c"))
          if (!is.na(iso)) norm_codes <- c(norm_codes, toupper(iso))
        }
      }
      country_codes <- unique(norm_codes)
    }

    # Auto-detect countries if not explicitly supplied
    if (is.null(country_codes) || length(country_codes) == 0L) {
      suppressMessages(suppressWarnings({
        s2_was <- sf::sf_use_s2(FALSE)
        on.exit(suppressMessages(suppressWarnings(sf::sf_use_s2(s2_was))), add = TRUE)
        # Use scale 50 (always bundled in rnaturalearthdata, never requires rnaturalearthhires)
        ne_c <- tryCatch(rnaturalearth::ne_countries(scale = 50, returnclass = "sf"), error = function(e) NULL)
        if (is.null(ne_c)) {
          ne_c <- tryCatch(rnaturalearth::ne_countries(scale = 110, returnclass = "sf"), error = function(e) NULL)
        }
        if (!is.null(ne_c)) {
          cand_codes <- character(0)
          extract_codes <- function(hits_idx) {
            out <- character(0)
            for (h in hits_idx) {
              row <- ne_c[h, ]
              iso <- NA_character_
              for (col in c("iso_a3", "adm0_a3", "gu_a3", "sov_a3")) {
                if (col %in% names(row) && !is.na(row[[col]]) && row[[col]] != "-99" && nchar(as.character(row[[col]])) == 3) {
                  iso <- as.character(row[[col]])
                  break
                }
              }
              if (is.na(iso) || iso == "-99") {
                nm <- if ("name" %in% names(row) && !is.na(row$name)) as.character(row$name) else
                      if ("admin" %in% names(row) && !is.na(row$admin)) as.character(row$admin) else NULL
                if (!is.null(nm)) {
                  iso <- suppressWarnings(countrycode::countrycode(nm, "country.name", "iso3c"))
                }
              }
              if (!is.na(iso) && nchar(iso) == 3 && iso != "-99") {
                out <- c(out, toupper(iso))
              }
            }
            unique(out)
          }

          # 1. Occurrence points intersection
          if (!is.null(pts) && nrow(pts) > 0L) {
            pts_ll <- sf::st_transform(pts, 4326)
            hits <- suppressWarnings(sf::st_intersects(pts_ll, ne_c))
            u_hits <- unique(unlist(hits))
            if (length(u_hits) > 0L) {
              cand_codes <- extract_codes(u_hits)
            }
            # Coastal / marine / island fallback if points didn't intersect land polygons directly
            if (length(cand_codes) == 0L) {
              near_hits <- suppressWarnings(tryCatch(
                unique(unlist(sf::st_is_within_distance(pts_ll, ne_c, dist = 50000))),
                error = function(e) NULL
              ))
              if (!is.null(near_hits) && length(near_hits) > 0L) {
                cand_codes <- extract_codes(near_hits)
              }
            }
          }
          # 2. Bounding box fallback if occurrence points didn't detect country
          if (length(cand_codes) == 0L) {
            bbox_ll <- sf::st_bbox(sf::st_transform(sf::st_segmentize(bbox_sf, dfMaxLength = 50000), 4326))
            bbox_poly <- sf::st_as_sfc(bbox_ll)
            hits <- suppressWarnings(sf::st_intersects(bbox_poly, ne_c)[[1]])
            if (length(hits) > 0L) {
              cand_codes <- extract_codes(hits)
            }
          }
          if (length(cand_codes) > 0L) country_codes <- cand_codes
        }
      }))
    }

    if (is.null(country_codes) || length(country_codes) == 0L) {
      message("  - could not detect country for protected areas, skipped")
      return(NULL)
    }

    wdpa_dir <- file.path(cache_dir(), "wdpa")
    dir.create(wdpa_dir, recursive = TRUE, showWarnings = FALSE)

    pa_list <- list()
    for (code in country_codes) {
      pa_item <- tryCatch({
        safe_wdpa_fetch_country(code, wdpa_dir)
      }, error = function(e) {
        message("  - WDPA fetch failed for '", code, "': ", conditionMessage(e))
        NULL
      })
      if (!is.null(pa_item) && nrow(pa_item) > 0L) {
        pa_list[[code]] <- pa_item
      }
    }

    if (length(pa_list) == 0L) {
      message("  - no protected areas data retrieved from WDPA, skipped")
      return(NULL)
    }

    if (length(pa_list) == 1L) {
      pa_raw <- pa_list[[1]]
    } else {
      common_cols <- Reduce(intersect, lapply(pa_list, names))
      pa_std <- lapply(pa_list, function(df) df[, common_cols, drop = FALSE])
      pa_raw <- do.call(rbind, pa_std)
    }
  }

  if (is.null(pa_raw) || nrow(pa_raw) == 0L) return(NULL)

  tryCatch({
    s2_was <- suppressWarnings(sf::sf_use_s2(FALSE))
    on.exit(suppressWarnings(sf::sf_use_s2(s2_was)), add = TRUE)

    pa_clean <- sf::st_make_valid(pa_raw)
    
    geom_types <- as.character(sf::st_geometry_type(pa_clean))
    poly_idx <- which(geom_types %in% c("POLYGON", "MULTIPOLYGON", "GEOMETRYCOLLECTION"))
    if (length(poly_idx) > 0L) {
      pa_clean <- pa_clean[poly_idx, , drop = FALSE]
    } else {
      return(NULL)
    }

    bbox_ll <- sf::st_bbox(sf::st_transform(sf::st_segmentize(bbox_sf, dfMaxLength = 50000), 4326))
    dx <- (bbox_ll[["xmax"]] - bbox_ll[["xmin"]]) * 0.05
    dy <- (bbox_ll[["ymax"]] - bbox_ll[["ymin"]]) * 0.05
    bbox_crop <- sf::st_bbox(c(xmin = bbox_ll[["xmin"]] - dx, ymin = bbox_ll[["ymin"]] - dy,
                               xmax = bbox_ll[["xmax"]] + dx, ymax = bbox_ll[["ymax"]] + dy),
                             crs = sf::st_crs(4326))
    
    cropped <- tryCatch(suppressWarnings(sf::st_crop(pa_clean, bbox_crop)), error = function(e) NULL)
    if (!is.null(cropped) && nrow(cropped) > 0L) {
      pa_clean <- cropped
    }

    sf::st_transform(pa_clean, crs_map)
  }, error = function(e) {
    message("  - protected areas layer failed, skipped (", conditionMessage(e), ")")
    NULL
  })
}

#' Analyze species occurrences within protected conservation areas
#'
#' Evaluates specimen representation and conservation metrics across protected
#' areas (WDPA or user-supplied). Generates per-protected-area metrics
#' (species richness, record counts, taxonomic breakdown) and an overall
#' conservation summary table (records and species within vs. outside protected areas).
#'
#' @param pa `sf` polygon object of protected areas.
#' @param pts `sf` point object of occurrences.
#' @param species_col Character: name of the column identifying species or grouping taxon.
#' @return A list with three elements:
#'   - `pa_sf`: The `pa` sf object augmented with `total_records`, `species_richness`,
#'     `species_list`, and standardized metadata columns.
#'   - `counts_df`: Data frame of per-protected-area metrics and per-species counts.
#'   - `summary_df`: Data frame summarizing conservation coverage across all PAs
#'     (pooled spp. and per-species).
#' @export
analyze_protected_areas <- function(pa, pts, species_col = "species") {
  if (is.null(pa) || nrow(pa) == 0L || is.null(pts) || nrow(pts) == 0L) {
    return(NULL)
  }

  tryCatch({
    # Ensure matching CRS for spatial intersection
    crs_pts <- sf::st_crs(pts)
    if (sf::st_crs(pa) != crs_pts) {
      pa_work <- sf::st_transform(pa, crs_pts)
    } else {
      pa_work <- pa
    }

    # Intersect points with protected areas
    s2_was <- suppressMessages(suppressWarnings(sf::sf_use_s2(FALSE)))
    on.exit(suppressMessages(suppressWarnings(sf::sf_use_s2(s2_was))), add = TRUE)

    hits <- sf::st_intersects(pa_work, pts)
    n_recs <- lengths(hits)

    # Resolve species column
    sp_vals <- if (!is.null(species_col) && species_col %in% names(pts)) {
      as.character(pts[[species_col]])
    } else if ("species" %in% names(pts)) {
      as.character(pts$species)
    } else {
      rep("Occurrence", nrow(pts))
    }

    u_species <- sort(unique(sp_vals[!is.na(sp_vals) & nzchar(sp_vals)]))

    # Calculate species richness and species lists per PA
    spp_richness <- integer(length(hits))
    spp_lists <- character(length(hits))

    for (i in seq_along(hits)) {
      idx <- hits[[i]]
      if (length(idx) > 0L) {
        s_in <- unique(sp_vals[idx])
        s_in <- s_in[!is.na(s_in) & nzchar(s_in)]
        spp_richness[i] <- length(s_in)
        spp_lists[i] <- paste(sort(s_in), collapse = ", ")
      } else {
        spp_richness[i] <- 0L
        spp_lists[i] <- ""
      }
    }

    # Standardize metadata columns
    pa_name <- if ("NAME" %in% names(pa)) pa$NAME else
               if ("NAME_ENG" %in% names(pa)) pa$NAME_ENG else
               if ("ORIG_NAME" %in% names(pa)) pa$ORIG_NAME else
               paste0("Protected Area #", seq_len(nrow(pa)))
    pa_name <- as.character(pa_name)
    pa_name[is.na(pa_name) | !nzchar(pa_name)] <- paste0("Protected Area #", which(is.na(pa_name) | !nzchar(pa_name)))

    pa_desig <- if ("DESIG_ENG" %in% names(pa)) pa$DESIG_ENG else
                if ("DESIG" %in% names(pa)) pa$DESIG else "Protected Area"
    pa_desig <- as.character(pa_desig)
    pa_desig[is.na(pa_desig)] <- "Protected Area"

    pa_iucn <- if ("IUCN_CAT" %in% names(pa)) pa$IUCN_CAT else "Not Reported"
    pa_iucn <- as.character(pa_iucn)
    pa_iucn[is.na(pa_iucn)] <- "Not Reported"

    pa_status_yr <- if ("STATUS_YR" %in% names(pa)) pa$STATUS_YR else NA
    pa_status_yr <- as.character(pa_status_yr)

    # Area in km2
    pa_rep_area <- if ("REP_AREA" %in% names(pa)) {
      suppressWarnings(as.numeric(pa$REP_AREA))
    } else {
      tryCatch({
        as.numeric(sf::st_area(sf::st_transform(pa, 3857))) / 1e6
      }, error = function(e) rep(NA_real_, nrow(pa)))
    }

    # Centroids in WGS84 for tabular reporting
    pa_ll <- sf::st_transform(pa, 4326)
    cents <- suppressWarnings(sf::st_coordinates(sf::st_centroid(pa_ll)))

    # Augment pa_sf
    pa$pa_id <- seq_len(nrow(pa))
    pa$pa_name <- pa_name
    pa$pa_desig <- pa_desig
    pa$pa_iucn <- pa_iucn
    pa$pa_status_yr <- pa_status_yr
    pa$rep_area_km2 <- pa_rep_area
    pa$total_records <- n_recs
    pa$species_richness <- spp_richness
    pa$species_list <- spp_lists

    # Build per-PA counts dataframe
    counts_df <- data.frame(
      pa_id = seq_len(nrow(pa)),
      name = pa_name,
      designation = pa_desig,
      iucn_category = pa_iucn,
      status_year = pa_status_yr,
      rep_area_km2 = round(pa_rep_area, 2),
      lon_center = round(cents[, 1], 5),
      lat_center = round(cents[, 2], 5),
      species_richness = spp_richness,
      total_records = n_recs,
      stringsAsFactors = FALSE
    )

    # Add per-species count columns
    if (length(u_species) > 0L) {
      for (sp in u_species) {
        sp_counts <- integer(length(hits))
        for (i in seq_along(hits)) {
          idx <- hits[[i]]
          if (length(idx) > 0L) {
            sp_counts[i] <- sum(sp_vals[idx] == sp, na.rm = TRUE)
          }
        }
        counts_df[[sp]] <- sp_counts
      }
    }
    counts_df$species_present <- spp_lists

    # Sort counts_df: PAs with records first, sorted by richness then total_records
    ord <- order(counts_df$total_records > 0, counts_df$species_richness, counts_df$total_records, decreasing = TRUE)
    counts_df <- counts_df[ord, , drop = FALSE]

    # Build Overall and Per-Species Conservation Summary Table
    total_pts_count <- nrow(pts)
    in_pa_indices <- unique(unlist(hits))
    tot_in_pa <- length(in_pa_indices)
    tot_outside_pa <- total_pts_count - tot_in_pa
    pct_in_pa <- round((tot_in_pa / total_pts_count) * 100, 2)

    total_pas <- nrow(pa)
    occupied_pas <- sum(n_recs > 0)
    empty_pas <- sum(n_recs == 0)
    pct_empty_pas <- round((empty_pas / total_pas) * 100, 2)

    mean_rec_occ <- if (occupied_pas > 0) round(mean(n_recs[n_recs > 0]), 2) else 0
    median_rec_occ <- if (occupied_pas > 0) round(stats::median(n_recs[n_recs > 0]), 2) else 0
    median_rec_all <- round(stats::median(n_recs), 2)

    summary_rows <- list()
    summary_rows[[1]] <- data.frame(
      taxon = "All species (spp.)",
      level = "spp",
      total_records = total_pts_count,
      records_in_pas = tot_in_pa,
      pct_records_in_pas = pct_in_pa,
      records_outside_pas = tot_outside_pa,
      total_pas = total_pas,
      occupied_pas = occupied_pas,
      empty_pas = empty_pas,
      pct_empty_pas = pct_empty_pas,
      mean_records_occupied = mean_rec_occ,
      median_records_occupied = median_rec_occ,
      median_records_all = median_rec_all,
      stringsAsFactors = FALSE
    )

    if (length(u_species) > 0L) {
      for (sp in u_species) {
        sp_all_idx <- which(sp_vals == sp)
        sp_tot <- length(sp_all_idx)
        sp_in_pa <- sum(sp_all_idx %in% in_pa_indices)
        sp_out_pa <- sp_tot - sp_in_pa
        sp_pct_in <- if (sp_tot > 0) round((sp_in_pa / sp_tot) * 100, 2) else 0

        # Number of PAs occupied by this species
        sp_pa_hits <- vapply(hits, function(h) sum(sp_vals[h] == sp, na.rm = TRUE), integer(1))
        sp_occ_pas <- sum(sp_pa_hits > 0)
        sp_empty_pas <- total_pas - sp_occ_pas
        sp_pct_empty_pas <- round((sp_empty_pas / total_pas) * 100, 2)

        sp_mean_rec <- if (sp_occ_pas > 0) round(mean(sp_pa_hits[sp_pa_hits > 0]), 2) else 0
        sp_med_occ <- if (sp_occ_pas > 0) round(stats::median(sp_pa_hits[sp_pa_hits > 0]), 2) else 0
        sp_med_all <- round(stats::median(sp_pa_hits), 2)

        summary_rows[[length(summary_rows) + 1]] <- data.frame(
          taxon = as.character(sp),
          level = "species",
          total_records = sp_tot,
          records_in_pas = sp_in_pa,
          pct_records_in_pas = sp_pct_in,
          records_outside_pas = sp_out_pa,
          total_pas = total_pas,
          occupied_pas = sp_occ_pas,
          empty_pas = sp_empty_pas,
          pct_empty_pas = sp_pct_empty_pas,
          mean_records_occupied = sp_mean_rec,
          median_records_occupied = sp_med_occ,
          median_records_all = sp_med_all,
          stringsAsFactors = FALSE
        )
      }
    }

    summary_df <- do.call(rbind, summary_rows)

    list(
      pa_sf = pa,
      counts_df = counts_df,
      summary_df = summary_df
    )
  }, error = function(e) {
    message("  - protected areas analysis failed: ", conditionMessage(e))
    NULL
  })
}