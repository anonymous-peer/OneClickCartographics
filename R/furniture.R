#' Small inset locator map
#'
#' A miniature map of the countries at 110 m scale with the map extent
#' highlighted, for use as an inset with `patchwork::inset_element()`.
#' Extents in 0-360 longitude convention (dateline-crossing data) are
#' shifted back to -180..180 so they align with the world layer.
#'
#' @param extent_sf Extent polygon in WGS84 (`sf`).
#' @return A `ggplot` object.
#' @keywords internal
make_inset <- function(extent_sf) {
  if (!requireNamespace("rnaturalearthdata", quietly = TRUE)) return(NULL)
  world <- tryCatch(rnaturalearth::ne_countries(scale = 110, returnclass = "sf"),
                    error = function(e) NULL)
  if (is.null(world)) return(NULL)
  ext_bb <- sf::st_bbox(extent_sf)
  if (ext_bb[["xmax"]] > 180) {
    ext_bb[c("xmin", "xmax")] <- ext_bb[c("xmin", "xmax")] - 360
  }
  dx <- ext_bb[["xmax"]] - ext_bb[["xmin"]]
  dy <- ext_bb[["ymax"]] - ext_bb[["ymin"]]
  span_x <- max(dx * 4, 18)
  span_y <- max(dy * 4, 18)
  cx <- (ext_bb[["xmin"]] + ext_bb[["xmax"]]) / 2
  cy <- (ext_bb[["ymin"]] + ext_bb[["ymax"]]) / 2
  xlim <- c(max(-180, cx - span_x / 2), min(180, cx + span_x / 2))
  ylim <- c(max(-85, cy - span_y / 2), min(85, cy + span_y / 2))
  ggplot2::ggplot() +
    ggplot2::geom_sf(data = world, fill = "grey88", colour = "grey55", linewidth = 0.15) +
    ggplot2::geom_sf(data = sf::st_as_sfc(sf::st_bbox(ext_bb)), fill = "red", alpha = 0.3,
                     colour = "red", linewidth = 0.45) +
    ggplot2::coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", colour = "grey40", linewidth = 0.3),
      plot.margin = ggplot2::margin(2, 2, 2, 2)
    )
}

#' Parse furniture location string into ggspatial 2-letter code
#'
#' @param pos Character position string (e.g. "bl", "bottom-left", "tr", etc.)
#' @param default Default 2-letter location code.
#' @return 2-letter code ("bl", "br", "tl", "tr") or NULL for custom.
#' @keywords internal
parse_furniture_location <- function(pos, default = "bl") {
  if (is.null(pos) || !is.character(pos) || length(pos) == 0 || !nzchar(trimws(pos[1]))) {
    return(default)
  }
  p <- tolower(trimws(pos[1]))
  p <- gsub("[_ ]+", "-", p)
  if (p %in% c("bl", "bottom-left", "bottomleft", "lower-left", "lowerleft")) {
    "bl"
  } else if (p %in% c("br", "bottom-right", "bottomright", "lower-right", "lowerright")) {
    "br"
  } else if (p %in% c("tl", "top-left", "topleft", "upper-left", "upperleft")) {
    "tl"
  } else if (p %in% c("tr", "top-right", "topright", "upper-right", "upperright")) {
    "tr"
  } else if (p %in% c("auto", "automatic")) {
    "auto"
  } else if (p == "custom") {
    NULL
  } else {
    warning("Unknown location '", pos, "'; defaulting to '", default, "'.")
    default
  }
}

#' Add scale bar and north arrow to a map
#'
#' @param p A `ggplot` map object.
#' @param map_aes Aesthetics list (entries `scalebar_colour`, `north_colour`).
#' @param scalebar Logical.
#' @param north_arrow Logical.
#' @param scalebar_pos Character: "bl" (default), "br", "tl", "tr", or full names.
#' @param north_arrow_pos Character: "tr" (default), "tl", "bl", "br", or full names.
#' @param north_arrow_scale Numeric scale factor for north arrow dimensions.
#' @return Updated `ggplot` object.
#' @keywords internal
add_furniture <- function(p, map_aes, scalebar, north_arrow,
                          scalebar_pos = NULL, north_arrow_pos = NULL,
                          north_arrow_scale = 1.0) {
  # Determine scale bar position
  scalebar_location <- parse_furniture_location(scalebar_pos, default = "bl")

  # Determine north arrow position
  north_arrow_location <- parse_furniture_location(north_arrow_pos, default = "tr")

  if (scalebar) {
    p <- p + ggspatial::annotation_scale(
      location = scalebar_location, width_hint = 0.2, style = "bar",
      text_col = map_aes$scalebar_colour, line_col = map_aes$scalebar_colour,
      text_cex = 0.7)
  }
  if (north_arrow) {
    na_scale <- if (!is.null(north_arrow_scale)) as.numeric(north_arrow_scale) else 1.0
    na_h <- if (!is.null(map_aes$north_arrow_height)) map_aes$north_arrow_height * na_scale else 1.5 * na_scale
    na_w <- if (!is.null(map_aes$north_arrow_width)) map_aes$north_arrow_width * na_scale else 1.5 * na_scale
    na_pad_x <- 0.25 * na_scale
    na_pad_y <- 0.25 * na_scale
    na_text_size <- if (!is.null(map_aes$north_arrow_text_size)) map_aes$north_arrow_text_size * na_scale else 10 * na_scale
    na_lwd <- 1.0 * na_scale

    p <- p + ggspatial::annotation_north_arrow(
      location = north_arrow_location, which_north = "true",
      height = ggplot2::unit(na_h, "cm"),
      width = ggplot2::unit(na_w, "cm"),
      pad_x = ggplot2::unit(na_pad_x, "cm"),
      pad_y = ggplot2::unit(na_pad_y, "cm"),
      style = ggspatial::north_arrow_fancy_orienteering(
        fill = c("black", "white"),
        text_col = map_aes$north_colour,
        text_size = na_text_size,
        line_width = na_lwd
      )
    )
  }
  p
}

#' Determine optimal position for inset map
#'
#' Evaluates candidate positions to select an unobstructed position that avoids
#' collisions with the scale bar and north arrow, and minimizes overlap with
#' occurrence records and land features.
#'
#' @param pts Spatial points (`sf` or data frame with coordinates).
#' @param bbox_sf Bounding box polygon or bbox of the map extent.
#' @param scalebar_pos Location of scale bar ("bl", "br", "tl", "tr").
#' @param north_arrow_pos Location of north arrow ("tr", "tl", "bl", "br").
#' @return Named list with `left`, `bottom`, `right`, `top`, and `pos_code`.
#' @keywords internal
find_optimal_inset_pos <- function(pts = NULL, bbox_sf = NULL,
                                   scalebar_pos = "bl", north_arrow_pos = "tr") {
  sb_loc <- parse_furniture_location(scalebar_pos, default = "bl")
  na_loc <- parse_furniture_location(north_arrow_pos, default = "tr")

  # Define candidate position boxes in NPC (left, bottom, right, top)
  candidates <- list(
    bl_above = list(left = 0.02, bottom = 0.12, right = 0.30, top = 0.38,
                    code = "bl"),
    tr_below = list(left = 0.70, bottom = 0.60, right = 0.98, top = 0.86,
                    code = "tr"),
    br = list(left = 0.70, bottom = 0.02, right = 0.98, top = 0.28,
              code = "br"),
    tl = list(left = 0.02, bottom = 0.70, right = 0.30, top = 0.96,
              code = "tl"),
    tr = list(left = 0.70, bottom = 0.70, right = 0.98, top = 0.96,
              code = "tr"),
    bl = list(left = 0.02, bottom = 0.02, right = 0.30, top = 0.28,
              code = "bl")
  )

  # Filter out direct collisions with other furniture
  allowed <- names(candidates)
  if (sb_loc == "bl") allowed <- setdiff(allowed, "bl")
  if (na_loc == "tr") allowed <- setdiff(allowed, "tr")
  if (sb_loc == "br") allowed <- setdiff(allowed, "br")
  if (na_loc == "tl") allowed <- setdiff(allowed, "tl")

  if (is.null(pts) || nrow(pts) == 0L || is.null(bbox_sf)) {
    pick <- if ("bl_above" %in% allowed) "bl_above" else allowed[1]
    res <- candidates[[pick]]
    res$pos_code <- paste0("auto (", res$code, ")")
    return(res)
  }

  bb <- sf::st_bbox(bbox_sf)
  xmin <- bb[["xmin"]]; xmax <- bb[["xmax"]]
  ymin <- bb[["ymin"]]; ymax <- bb[["ymax"]]
  dx <- xmax - xmin; dy <- ymax - ymin

  if (is.na(dx) || is.na(dy) || dx <= 0 || dy <= 0) {
    pick <- if ("bl_above" %in% allowed) "bl_above" else allowed[1]
    res <- candidates[[pick]]
    res$pos_code <- paste0("auto (", res$code, ")")
    return(res)
  }

  pts_in_crs <- tryCatch({
    if (inherits(pts, "sf")) sf::st_transform(pts, sf::st_crs(bbox_sf)) else NULL
  }, error = function(e) NULL)

  if (is.null(pts_in_crs) && is.data.frame(pts) && all(c("lon", "lat") %in% names(pts))) {
    pts_in_crs <- tryCatch({
      sf::st_as_sf(pts, coords = c("lon", "lat"), crs = 4326) |>
        sf::st_transform(sf::st_crs(bbox_sf))
    }, error = function(e) NULL)
  }

  if (is.null(pts_in_crs) || nrow(pts_in_crs) == 0L) {
    pick <- if ("bl_above" %in% allowed) "bl_above" else allowed[1]
    res <- candidates[[pick]]
    res$pos_code <- paste0("auto (", res$code, ")")
    return(res)
  }

  coords <- sf::st_coordinates(pts_in_crs)
  nx <- (coords[, 1] - xmin) / dx
  ny <- (coords[, 2] - ymin) / dy

  # Score each candidate by counting points inside NPC bounds and calculating min distance
  scores <- vapply(allowed, function(cand_name) {
    cand <- candidates[[cand_name]]
    in_box <- sum(nx >= cand$left & nx <= cand$right & ny >= cand$bottom & ny <= cand$top)
    cx <- (cand$left + cand$right) / 2
    cy <- (cand$bottom + cand$top) / 2
    dists <- sqrt((nx - cx)^2 + (ny - cy)^2)
    min_dist <- if (length(dists) > 0) min(dists) else 0

    # Composite score: minimize points inside, maximize distance to points
    in_box * 1e9 - min_dist
  }, numeric(1))

  best_name <- allowed[which.min(scores)]
  res <- candidates[[best_name]]
  res$pos_code <- paste0("auto (", res$code, ")")
  res
}

#' Resolve inset element bounds in NPC coordinates
#'
#' @param inset_pos Character ("auto", "br", "tl", "tr", "bl") or numeric vector:
#'   - 4-element numeric: c(left, bottom, right, top) in NPC (0-1) or native map coordinates
#'   - 2-element numeric: c(x, y) anchor coordinates in NPC (0-1) or native map coordinates
#' @param pts Spatial points or data frame.
#' @param bbox_sf Map bounding box (sf or bbox).
#' @param scalebar_pos Scale bar position.
#' @param north_arrow_pos North arrow position.
#' @param inset_width Desired width in NPC (default 0.28).
#' @param inset_height Desired height in NPC (default 0.26).
#' @return Named list: list(left = ..., bottom = ..., right = ..., top = ..., pos_code = ...)
#' @keywords internal
resolve_inset_bounds <- function(inset_pos = NULL, pts = NULL, bbox_sf = NULL,
                                 scalebar_pos = "bl", north_arrow_pos = "tr",
                                 inset_width = 0.28, inset_height = 0.26) {
  corner_npc <- list(
    tl = list(left = 0.02, bottom = 1.0 - 0.02 - inset_height,
              right = 0.02 + inset_width, top = 0.98, pos_code = "top-left (tl)"),
    tr = list(left = 1.0 - 0.02 - inset_width, bottom = 1.0 - 0.02 - inset_height,
              right = 0.98, top = 0.98, pos_code = "top-right (tr)"),
    bl = list(left = 0.02, bottom = 0.02,
              right = 0.02 + inset_width, top = 0.02 + inset_height, pos_code = "bottom-left (bl)"),
    br = list(left = 1.0 - 0.02 - inset_width, bottom = 0.02,
              right = 0.98, top = 0.02 + inset_height, pos_code = "bottom-right (br)")
  )

  # Case 1: Numeric vector supplied by user
  if (is.numeric(inset_pos)) {
    if (length(inset_pos) == 4L) {
      vals <- as.numeric(inset_pos)
      if (all(vals >= 0 & vals <= 1) && vals[3] > vals[1] && vals[4] > vals[2]) {
        return(list(left = vals[1], bottom = vals[2], right = vals[3], top = vals[4],
                    pos_code = paste0("custom NPC [", paste(round(vals, 3), collapse = ", "), "]")))
      } else if (!is.null(bbox_sf)) {
        bb <- sf::st_bbox(bbox_sf)
        dx <- bb[["xmax"]] - bb[["xmin"]]
        dy <- bb[["ymax"]] - bb[["ymin"]]
        if (dx > 0 && dy > 0) {
          l <- pmax(0.01, pmin(0.99, (vals[1] - bb[["xmin"]]) / dx))
          b <- pmax(0.01, pmin(0.99, (vals[2] - bb[["ymin"]]) / dy))
          r <- pmax(0.01, pmin(0.99, (vals[3] - bb[["xmin"]]) / dx))
          t <- pmax(0.01, pmin(0.99, (vals[4] - bb[["ymin"]]) / dy))
          if (r > l && t > b) {
            return(list(left = l, bottom = b, right = r, top = t,
                        pos_code = paste0("custom coordinates [", paste(round(vals, 2), collapse = ", "), "]")))
          }
        }
      }
    } else if (length(inset_pos) == 2L) {
      vals <- as.numeric(inset_pos)
      cx <- vals[1]; cy <- vals[2]
      if (!is.null(bbox_sf) && (cx < 0 || cx > 1 || cy < 0 || cy > 1)) {
        bb <- sf::st_bbox(bbox_sf)
        dx <- bb[["xmax"]] - bb[["xmin"]]
        dy <- bb[["ymax"]] - bb[["ymin"]]
        if (dx > 0 && dy > 0) {
          cx <- (cx - bb[["xmin"]]) / dx
          cy <- (cy - bb[["ymin"]]) / dy
        }
      }
      cx <- pmax(inset_width / 2 + 0.01, pmin(1.0 - inset_width / 2 - 0.01, cx))
      cy <- pmax(inset_height / 2 + 0.01, pmin(1.0 - inset_height / 2 - 0.01, cy))
      return(list(
        left = cx - inset_width / 2,
        bottom = cy - inset_height / 2,
        right = cx + inset_width / 2,
        top = cy + inset_height / 2,
        pos_code = paste0("custom anchor (", round(vals[1], 2), ", ", round(vals[2], 2), ")")
      ))
    }
  }

  # Case 2: Named corner string
  if (is.character(inset_pos) && length(inset_pos) > 0L) {
    p_code <- parse_furniture_location(inset_pos, default = "auto")
    if (p_code %in% names(corner_npc)) {
      return(corner_npc[[p_code]])
    }
  }

  # Case 3: "auto" or NULL -> optimal candidate selection
  opt_res <- find_optimal_inset_pos(pts, bbox_sf, scalebar_pos, north_arrow_pos)
  opt_res
}
