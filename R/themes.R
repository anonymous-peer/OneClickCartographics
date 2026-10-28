#' Default cartographic style
#'
#' A clean journal-oriented theme used by [run_map()]: no grid, subtle axis
#' text for the graticule coordinates, legend positioned as specified.
#'
#' @param legend_position Character: one of `"bottom"` (default), `"top"`,
#'   `"left"`, `"right"`, or `"none"`.
#' @param show_graticule Logical: show axis tick labels. Default `TRUE`.
#' @return A `ggplot2` theme object.
#' @keywords internal
map_theme <- function(legend_position = "bottom", show_graticule = TRUE, bg_fill = "white") {
  axis_text <- if (show_graticule) {
    ggplot2::element_text(colour = "grey30", size = 8)
  } else {
    ggplot2::element_blank()
  }
  ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = bg_fill, colour = NA),
      panel.border = ggplot2::element_rect(fill = NA, colour = "grey40", linewidth = 0.4),
      panel.grid = ggplot2::element_blank(),
      axis.text = axis_text,
      axis.ticks = if (show_graticule) {
        ggplot2::element_line(colour = "grey40", linewidth = 0.3)
      } else {
        ggplot2::element_blank()
      },
      axis.title = ggplot2::element_blank(),
      legend.position = legend_position,
      legend.title = ggplot2::element_text(face = "bold", size = 9),
      legend.text = ggplot2::element_text(size = 8),
      legend.key = ggplot2::element_rect(fill = "transparent", colour = NA),
      legend.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      legend.box.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      legend.margin = ggplot2::margin(t = -5),
      plot.margin = ggplot2::margin(8, 8, 8, 8)
    )
}

#' Default aesthetic settings for run_map()
#'
#' A named list merged with user-supplied `map_aes` in [run_map()]. Palette is
#' colour-blind safe (Okabe-Ito). All entries can be overridden.
#'
#' @return Named list of default map aesthetics.
#' @keywords internal
default_map_aes <- function() {
  list(
    palette = c("#0072B2", "#D55E00", "#009E73", "#CC79A7", "#E69F00",
                "#56B4E9", "#F0E442", "#000000"),
    land_fill = "#F5F0E6",
    map_fill = "#F5F0E6",
    land_border = "grey45",
    land_lwd = 0.25,
    country_border = "grey45",
    country_color = "grey45",
    country_colour = "grey45",
    country_lwd = 0.25,
    country_width = 0.25,
    admin_color = "grey45",
    admin_colour = "grey45",
    admin_width = 0.25,
    admin_lwd = 0.25,
    admin1_colour = "grey60",
    admin1_color = "grey60",
    admin1_lwd = 0.15,
    admin1_width = 0.15,
    state_border = "grey60",
    state_color = "grey60",
    state_colour = "grey60",
    state_lwd = 0.15,
    state_width = 0.15,
    province_color = "grey60",
    province_colour = "grey60",
    province_lwd = 0.15,
    province_width = 0.15,
    water_fill = "#A6C8E3",
    water_line = "#8FB3D9",
    water_lwd = 0.2,
    road_colour = "#78716c",
    road_lwd = 0.35,
    city_colour = "grey35",
    city_size = 0.7,
    city_label_colour = "#2b2b2b",
    city_label_size = 2.5,
    city_label_fill = "#ffffffc8",
    point_size = 2.2,
    point_alpha = 0.75,
    point_border_color = "black",
    point_border_linewidth = 0.2,
    point_border_linetype = "solid",
    graticule_colour = "grey70",
    graticule_lwd = 0.15,
    scalebar_colour = "black",
    north_colour = "black",
    north_arrow_scale = 1.0,
    terrain_low = "grey25",
    terrain_high = "grey95",
    terrain_alpha = 0.40,
    terrain_cols = c("#D8E8D5", "#E8E5C8", "#DECCA6", "#BE9D7B", "#998675", "#E0DFDF"),
    elevation_cols = c("#3E8E4A", "#8FBF4F", "#E0D35F", "#C9A86B",
                       "#9C7A4F", "#F2EFE9"),
    bathymetry_cols = c("#041B3B", "#08306B", "#1C5E9E", "#4292C6",
                        "#9ECAE1", "#DEEBF7"),
    topo_cols = c("#082B60", "#1C5E9E", "#5A9BCF", "#A8CDE0", "#DCEBD0",
                  "#F0E6B0", "#D9B97A", "#A67945", "#6E4A28", "#FFFFFF"),
    raster_alpha = 0.9,
    ecoregion_colour = "grey50",
    ecoregion_lwd = 0.12,
    protected_areas_fill = "#2e7d32",
    protected_areas_border = "#1b5e20",
    protected_areas_alpha = 0.25,
    protected_areas_lwd = 0.35,
    protected_areas_linetype = "solid",
    grid_low = "#FFFFFF",
    grid_high = "#B10026",
    grid_palette = c("#FFFFFF", "#FEE5D9", "#FCAE91", "#FB6A4A", "#DE2D26", "#A50F15"),
    grid_color = "black",
    grid_linewidth = 0.5,
    grid_linetype = "solid",
    grid_alpha = 0.85,
    shapes = c(21, 22, 23, 24, 25)
  )
}

#' Inspect or list available map aesthetic settings
#'
#' Prints or returns a reference table of all customizable aesthetic options
#' that can be passed to the `map_aes` list argument in [run_map()].
#'
#' @param print Logical: whether to pretty-print the reference table to the console (default `TRUE`).
#' @return A `data.frame` with columns `setting`, `default`, and `description` (invisibly if `print = TRUE`).
#' @export
#' @examples
#' list_map_aes()
#' \dontrun{
#' run_map("localities.csv", map_aes = list(water_fill = "#d4ebf2", land_fill = "#fdfbf7"))
#' }
list_map_aes <- function(print = TRUE) {
  defs <- default_map_aes()
  df <- data.frame(
    setting = c(
      "palette", "map_fill", "land_fill", "land_border", "land_lwd",
      "country_color", "country_width", "admin_color", "admin_width",
      "state_color", "state_width", "admin1_colour", "admin1_lwd",
      "water_fill", "water_line", "water_lwd",
      "city_colour", "city_size", "city_label_colour", "city_label_size", "city_label_fill",
      "road_colour", "road_lwd",
      "point_size", "point_alpha", "point_border_color", "point_border_linewidth", "point_border_linetype",
      "scalebar_colour", "north_colour", "north_arrow_scale",
      "terrain_alpha", "terrain_cols", "elevation_cols", "bathymetry_cols", "topo_cols", "raster_alpha",
      "ecoregion_colour", "ecoregion_lwd",
      "protected_areas_fill", "protected_areas_border", "protected_areas_alpha", "protected_areas_lwd", "protected_areas_linetype",
      "grid_color", "grid_linewidth", "grid_linetype", "grid_alpha", "grid_palette",
      "shapes"
    ),
    default = c(
      "Okabe-Ito (8 colours)", defs$map_fill, defs$land_fill, defs$land_border, as.character(defs$land_lwd),
      defs$country_color, as.character(defs$country_width), defs$admin_color, as.character(defs$admin_width),
      defs$state_color, as.character(defs$state_width), defs$admin1_colour, as.character(defs$admin1_lwd),
      defs$water_fill, defs$water_line, as.character(defs$water_lwd),
      defs$city_colour, as.character(defs$city_size), defs$city_label_colour, as.character(defs$city_label_size), defs$city_label_fill,
      defs$road_colour, as.character(defs$road_lwd),
      as.character(defs$point_size), as.character(defs$point_alpha), defs$point_border_color, as.character(defs$point_border_linewidth), defs$point_border_linetype,
      defs$scalebar_colour, defs$north_colour, as.character(defs$north_arrow_scale),
      as.character(defs$terrain_alpha), "Natural hypsometric tint", "Green-to-white gradient", "Deep blue gradient", "ETOPO1 bathy-elevation", as.character(defs$raster_alpha),
      defs$ecoregion_colour, as.character(defs$ecoregion_lwd),
      defs$protected_areas_fill, defs$protected_areas_border, as.character(defs$protected_areas_alpha), as.character(defs$protected_areas_lwd), defs$protected_areas_linetype,
      defs$grid_color, as.character(defs$grid_linewidth), defs$grid_linetype, as.character(defs$grid_alpha), "White to dark red",
      "Circles, squares, diamonds, triangles"
    ),
    description = c(
      "Categorical colour palette for species/taxa groupings",
      "Base map / landmass polygon fill colour (default #F5F0E6)",
      "Base landmass polygon fill colour (default #F5F0E6)",
      "Country boundary border line colour",
      "Country boundary stroke width",
      "Country boundary stroke colour (alias for land_border)",
      "Country boundary stroke width (alias for land_lwd)",
      "Administrative boundary line colour (country/general)",
      "Administrative boundary stroke width (country/general)",
      "State / provincial boundary line colour (alias for admin1_colour)",
      "State / provincial boundary stroke width (alias for admin1_lwd)",
      "State / provincial boundary line colour",
      "State / provincial boundary stroke width",
      "Ocean and lake water body fill colour",
      "River centerline stroke colour",
      "River centerline stroke width",
      "Populated place / city dot colour",
      "Populated place / city dot size",
      "Populated place / city text label colour",
      "Populated place / city text label size",
      "Populated place / city text label box fill colour",
      "Road network line colour",
      "Road network stroke width",
      "Occurrence point symbol size",
      "Occurrence point transparency (0 = transparent, 1 = opaque)",
      "Occurrence point outline border colour",
      "Occurrence point outline border stroke width",
      "Occurrence point outline line type ('solid', 'dashed', etc.)",
      "Scale bar line and text colour",
      "North arrow outline and text colour",
      "North arrow size multiplier (1.0 = standard, 0.75 = compact)",
      "3D hillshade shadow intensity (0 = none, 1 = maximum contrast)",
      "Natural hypsometric palette for physical terrain relief",
      "Land elevation gradient colours",
      "Ocean bathymetry / depth gradient colours",
      "Combined land elevation and sea bathymetry colour ramp",
      "Transparency of elevation/depth raster overlay",
      "WWF terrestrial ecoregion polygon border colour",
      "WWF terrestrial ecoregion polygon border line width",
      "Protected conservation areas polygon fill colour",
      "Protected conservation areas polygon border stroke colour",
      "Protected conservation areas polygon fill transparency (0 = transparent, 1 = opaque)",
      "Protected conservation areas border stroke width",
      "Protected conservation areas border line type ('solid', 'dashed', 'dotted')",
      "Density / richness grid cell outline stroke colour",
      "Density / richness grid cell outline stroke width",
      "Density / richness grid cell outline line type",
      "Density / richness grid cell fill transparency",
      "Sequential colour ramp for density / richness cells",
      "Point symbol shapes for categorical groups"
    ),
    stringsAsFactors = FALSE
  )
  if (isTRUE(print)) {
    cat("\n=== OneClickCartographics: Map Aesthetic Settings (map_aes) ===\n\n")
    print(format(df, justify = "left"), row.names = FALSE)
    cat("\nUsage example:\n  run_map(\"localities.csv\", map_aes = list(water_fill = \"#d0e7f9\", land_fill = \"#fcfaf2\"))\n\n")
    invisible(df)
  } else {
    df
  }
}
