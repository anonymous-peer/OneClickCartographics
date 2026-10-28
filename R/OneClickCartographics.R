#' Generate a publication-ready distribution map
#'
#' One-click cartographics: reads a CSV or Excel table of occurrences
#' (species/population name + latitude/longitude) and produces a map with
#' Natural Earth base layers (land, ocean, lakes/rivers, administrative
#' boundaries, cities, roads), a km scale bar, north arrow and graticule.
#'
#' The map scale adapts automatically to the extent of the points: local
#' (<= 300 km span, 1:10m layers, UTM), regional (<= 3,000 km, 1:50m, UTM),
#' continental (<= 8,000 km, 1:50m, Lambert Azimuthal Equal-Area) and global
#' (1:110m, World Equidistant Cylindrical). Data crossing the antimeridian is
#' wrapped automatically. Points are coloured by the `color_by` column
#' (default `species`); with two or more groups each group is drawn with a
#' distinct colour **and** shape combination.
#'
#' Optional layers can be requested on top of the minimalistic default map:
#' an SRTM/ETOPO elevation raster (`show_elevation`), 3D physical shaded terrain
#' (`show_terrain`), WWF terrestrial ecoregions (`show_ecoregions`), and a
#' density or species richness grid (`grid_km`).
#'
#' @param data_path Character or `data.frame`: path to a local spreadsheet
#'   (`.csv`, `.tsv`, `.txt`, `.xlsx`, or `.xls`), a `data.frame`, or a
#'   user-defined taxonomic species name (e.g. `"Panthera onca"`) to search
#'   and fetch directly from GBIF. Required.
#' @param output_dir Character or `NULL`: output folder. If `NULL`, a
#'   timestamped folder `OneClickCartographics_YYYYMMDD_HHMMSS` is created next to the
#'   input file (or in the current working directory for the bundled dataset).
#' @param color_by Character or `NULL`: name of the input column used to colour
#'   the points. If `NULL` (default), the function auto-detects a column matching
#'   "species", "sp", "group", or "grouping" (case-insensitive). When multiple
#'   grouping columns exist, the first matching column is used. Set `color_by`
#'   explicitly to override auto-detection. Default is NULL.
#' @param shape_by Character or `NULL`: optional input column mapped to point
#'   shapes. When `NULL` and `color_by` has two or more groups, the groups are
#'   automatically mapped to colour **and** shape combinations.
#' @param scale Integer or `NULL`: Natural Earth scale, one of 10, 50 or 110
#'   (smaller = more detail). `NULL` (default) selects the scale from the
#'   point span (see Details).
#' @param projection Integer EPSG code or `NULL` (auto: chosen from the point
#'   span, see Details).
#' @param auto_scale Logical: when `TRUE` (default) the Natural Earth scale,
#'   projection, city/road layers and scale bar adapt to the extent of the
#'   points; explicit `scale`/`projection` arguments always take precedence.
#' @param show_elevation Logical or character: add an elevation/altitude raster overlay.
#'   Can be `TRUE` (displays elevation with zero-anchored scale),
#'   `"land"` (terrestrial elevation only), `"sea"` (ocean bathymetry only), or `"both"`
#'   (land elevation + bathymetry). Default `FALSE`.
#' @param elevation_scope Character: scope of the elevation scale when `show_elevation`
#'   is active. One of `"both"` (default), `"land"`, or `"sea"`.
#' @param elevation_limits Numeric vector of length 2 `c(min, max)`: optional explicit limits for
#'   the elevation/bathymetry scale (e.g. `c(-8216, 1974)`). If `NULL` (default), limits are
#'   derived from the raster extrema.
#' @param water_depth Logical or `NULL`: convenience flag to toggle ocean bathymetry.
#'   When `FALSE`, sets `elevation_scope = "land"`. If `NULL` (default), inherited
#'   from `elevation_scope`.
#' @param show_altitude Alias for `show_elevation`.
#' @param show_topo Logical: convenience alias for `show_elevation = TRUE, elevation_scope = "both"`.
#' @param show_ecoregions Logical: add WWF terrestrial ecoregion boundaries
#'   (Olson et al. 2001) under occurrence points (requires internet on first use).
#' @param show_protected_areas Logical: automatically fetch, clean, and render
#'   protected conservation areas (National Parks, Reserves, Protected Landscapes)
#'   from the World Database on Protected Areas (WDPA) via UNEP-WCMC. Default `FALSE`.
#' @param protected_areas User-supplied protected areas spatial layer. Can be a
#'   path to a local vector file (Shapefile, GeoPackage, GeoJSON), an `sf` polygon
#'   object, or explicit country code(s) (e.g. `"CUB"`, `"Cuba"`). If set,
#'   overrides or directs protected areas retrieval.
#' @param protected_areas_fill Character: polygon fill color for protected areas (default `"#2e7d32"`).
#' @param protected_areas_border Character: polygon border outline color for protected areas (default `"#1b5e20"`).
#' @param protected_areas_alpha Numeric: fill opacity/transparency for protected areas (0 = fully transparent, 1 = opaque, default `0.25`).
#' @param protected_areas_lwd Numeric: polygon border outline stroke width (default `0.35`).
#' @param protected_areas_width Alias for `protected_areas_lwd`.
#' @param protected_areas_linetype Character: border line type for protected areas (`"solid"`, `"dashed"`, `"dotted"`, default `"solid"`).
#' @param grid_km Numeric or `NULL`: side length in km of a square density
#'   grid; cells are coloured by the number of records they contain. The
#'   points themselves are not modified.
#' @param grid_bbox Numeric vector or `NULL`: bounding box for the density grid
#'   in WGS84, `c(xmin, xmax, ymin, ymax)`. When `NULL` (default) the grid
#'   covers the full map extent. Only used when `grid_km` is set.
#' @param grid_csv Logical: when `TRUE` and `grid_km` is set, save a
#'   `grid_counts.csv` file to the output directory with per-species and
#'   total counts per grid cell. Default `FALSE`.
#' @param show_points Logical or `NULL`: draw the occurrence points on the
#'   map. When `NULL` (default), points are hidden if a density/richness grid
#'   is rendered (`grid_km` is specified) so the points do not cover the
#'   grid cells, and points are shown if no grid is rendered. Explicitly set
#'   `TRUE` or `FALSE` to override this default.
#' @param show_terrain Logical: add a 3D shaded relief hillshade with natural
#'   hypsometric tinting (requires internet on first use; gracefully skipped if unavailable).
#' @param terrain_zoom Integer: elevatr zoom level for the terrain tile
#'   (0-14, 6 recommended).
#' @param show_cities Logical, integer, or `NULL`: include Natural Earth
#'   populated places. Pass an integer to specify max cities to show (default 25).
#' @param city_labels Logical: display text labels for populated places when
#'   `show_cities` is active (default `TRUE`).
#' @param show_roads Logical, integer, or character: include roads.
#'   `FALSE` or `NULL` (none); `TRUE`, `1`, or `"major"` (major highways and expressways);
#'   `2` or `"all"` (major highways + secondary connecting roads).
#' @param map_aes Named list overriding default map aesthetics (see
#'   [list_map_aes()] for complete list and defaults).
#' @param map_fill,land_fill Character or NULL: fill colour for the base map / land polygons
#'   (e.g., "#E0E0E0", "grey90", "wheat"). Default `NULL` uses the aesthetic default ("#F5F0E6").
#' @param fill_color Optional alias for `map_fill`.
#' @param scalebar,north_arrow Logical: show km scale bar and north arrow.
#' @param graticule Logical: show graticule (coordinate reference lines).
#' @param graticule_lines Logical: when `TRUE` and `graticule = TRUE`, draw
#'   full graticule lines through the map (same aesthetics as before). When
#'   `FALSE` (default), only axis tick labels are shown.
#' @param inset_map Logical: add a miniature locator overview globe/inset map
#'   (requires `patchwork`).
#' @param inset_pos Position or coordinates for the inset map. Options:
#'   - `"auto"` or `NULL` (default): automatically selects an unobstructed corner
#'     ("bl", "br", "tl", "tr") that does not conflict with scale bar or north arrow,
#'     and minimizes overlap with occurrence records.
#'   - Character corner code: `"br"`, `"bottom-right"`, `"tl"`, `"top-left"`, `"tr"`, `"bl"`.
#'   - 4-element numeric vector: `c(left, bottom, right, top)` in NPC coordinates (0 to 1)
#'     or native map coordinates `c(xmin, ymin, xmax, ymax)`.
#'   - 2-element numeric vector: `c(x, y)` inside coordinates to center the inset.
#' @param formats Character vector of output formats: any combination of "pdf", "png",
#'   "svg", and "html" (interactive tooltip map).
#' @param width,height Numeric: map size in inches. When `height` is `NULL` (default),
#'   the figure height is automatically calculated to snugly fit the map panel
#'   aspect ratio, axes, and legends without excessive blank areas.
#' @param filename Character: base file name for the map outputs.
#' @param admin_level Integer, character, or `NULL`: administrative divisions drawn
#'   -- `0` or `"country"` (country borders), `1` or `"state"` / `"province"` / `"department"`
#'   (state/provincial/departmental boundaries), or `NULL` (default; no boundaries
#'   drawn unless user-specified). Note that level 1 uses 1:10m Natural Earth data worldwide
#'   with complete global coverage (including Mexican states and Colombian departments).
#'   Counties/municipalities are standardized to Level 1 for global consistency.
#' @param show_countries,show_states,show_counties Logical or `NULL`: convenience
#'   aliases to enable country, state/province, or administrative boundaries.
#' @param admin_color,admin_colour,country_color,country_colour,country_border Character or `NULL`:
#'   line colour for country / primary administrative boundary borders (default `"grey45"`).
#' @param admin_width,admin_lwd,country_width,country_lwd Numeric or `NULL`:
#'   stroke width for country / primary administrative boundary borders (default `0.25`).
#' @param state_color,state_colour,province_color,province_colour,state_border Character or `NULL`:
#'   line colour for state/province/department borders (default `"grey60"`).
#' @param state_width,state_lwd,province_width,province_lwd Numeric or `NULL`:
#'   stroke width for state/province/department borders (default `0.15`).
#' @param show_water Logical: include bodies of water (ocean, lakes, rivers).
#'   Default `FALSE` (user-specified).
#' @param show_ocean,show_lakes,show_rivers Logical or `NULL`: individual
#'   controls for ocean, lakes, and rivers. When `NULL`, inherit from `show_water`.
#' @param show_land Logical: draw the base land polygon. Default `TRUE`.
#' @param grid_color Character or `NULL`: contour line colour for each grid cell.
#'   Default `"black"` (1-pixel outline).
#' @param grid_linewidth Numeric or `NULL`: contour line width for grid cells.
#'   Default `0.5` (~1 pixel).
#' @param grid_linetype Character or numeric or `NULL`: contour line type for
#'   grid cells (e.g. `"solid"`, `"dashed"`). Default `"solid"`.
#' @param grid_steps Character, logical, numeric vector, or integer: binning scheme for
#'   grid counts. One of:
#'   - `"auto"` (default adaptive intervals e.g. 1, 2–3 up to hundreds; preserves default behavior)
#'   - `"continuous"` or `FALSE` (smooth unbinned continuous gradient)
#'   - A single integer `k >= 1` (e.g. `4`): creates `k` categories across the range
#'   - A numeric vector (e.g. `c(1, 5, 20, 50)`): cutoffs setting observations for each category independently
#'   - A list of numeric range vectors: explicit independent range intervals for each category
#' @param grid_categories Integer, list, or `NULL`: user-defined categories.
#'   - Single integer (e.g. `4`): exact number of categories desired.
#'   - List of 2-element numeric vectors (e.g. `list(c(1, 5), c(6, 20), c(21, 50))`): defines
#'     the observation range for each category independently.
#' @param grid_category_width,grid_step_size Numeric or `NULL`: fixed observation width /
#'   step size across all categories (e.g. `10` creates categories 1–10, 11–20, 21–30...).
#' @param grid_breaks Numeric vector or `NULL`: break cutoffs defining observations
#'   on each category independently (e.g. `c(1, 5, 20, 50)`).
#' @param grid_category_method Character: classification method when `grid_categories`
#'   is specified as a count. One of `"auto"` / `"pretty"`, `"equal_interval"`,
#'   or `"quantile"` (equal number of observed cells in each category across the map).
#' @param grid_category_labels Character vector or `NULL`: custom text labels
#'   for category legend items.
#' @param grid_metric Character: metric for grid cell counts. One of:
#'   - `"auto"` (default: species richness if multiple species exist, otherwise records)
#'   - `"richness"` or `"species"` (distinct species count per cell)
#'   - `"records"` or `"points"` (total occurrence records per cell)
#'   - `"both"` (generates two separate grid maps: species richness and point density)
#' @param grid_palette Character vector or `NULL`: color palette for grid cells.
#'   Default is white to red (`c("#FFFFFF", "#FEE5D9", "#FCAE91", "#FB6A4A", "#DE2D26", "#A50F15")`).
#' @param keep_medium Character: filter occurrence records by medium.
#'   - `"all"` (default): keep all occurrence points.
#'   - `"land"`: keep only terrestrial points; drop points falling in the ocean/sea.
#'   - `"water"`: keep only aquatic/marine points; drop points falling on land.
#' @param map_bounds NULL (default) or a numeric vector c(xmin, xmax, ymin,
#'   ymax) in WGS84 longitude/latitude, or an `sf` POLYGON object in WGS84 CRS.
#'   When provided, the map extent is fixed to these coordinates instead of
#'   being auto-determined from the occurrence points. Useful for focusing on a
#'   specific region.
#' @param palette Character or `NULL`: colour palette for points. Can be:
#'   - `NULL` (default): uses the Okabe-Ito colour-blind-safe palette.
#'   - A character vector of hex colour codes (e.g., `c("#FF0000", "#00FF00")`).
#'   - A palette name: `"viridis"`, `"magma"`, `"inferno"`, `"plasma"`,
#'     `"cividis"` (requires `viridis` package), or any palette from
#'     `RColorBrewer::brewer.pal.info` (e.g., `"Set1"`, `"Paired"`,
#'     `"RdYlBu"`; requires `RColorBrewer` package).
#'   - A path to a plain-text file with one hex colour per line.
#' @param legend_position Character: position of the legend. One of
#'   `"bottom"` (default), `"top"`, `"left"`, `"right"`, or `"none"`.
#' @param gis_formats Character vector or `NULL`: GIS export formats for the
#'   occurrence points. Any combination of `"shp"`, `"gpkg"`, `"geojson"`.
#'   Files are saved to the output directory as \code{<filename>_points.<ext>}.
#' @param bbox_buffer Numeric: fraction of extent added as padding around occurrence points (default 0.03).
#' @param seed Integer: random seed for reproducibility (default 12345).
#' @param verbose Logical: print progress messages. Default `TRUE`.
#' @param dpi Numeric: resolution in dots per inch for raster outputs (default `600`;
#'   use `300` for lower resolution).
#' @param point_color Character vector or `NULL`: override point colours directly.
#' @param point_shape Integer or `NULL`: override point shapes directly.
#' @param point_border_color Character or `NULL`: outline stroke colour for points.
#' @param point_border_linewidth Numeric or `NULL`: outline stroke width for points.
#' @param point_border_linetype Character, numeric, or `NULL`: outline stroke linetype for points.
#' @param journal Character or `NULL`: format figure dimensions for a target
#'   academic journal (e.g. `"zootaxa"`, `"pensoft"`, `"nature"`, `"plos"`,
#'   `"elsevier"`, `"wiley"`, `"systematic_biology"`). Automatically sets width,
#'   height, and 600 DPI (or 300 DPI if lower resolution is specified).
#' @param journal_cols Numeric: column span for journal layout (`1` = single
#'   column, `1.5` = intermediate, `2` = full width; default `2`).
#' @param clean_coords Logical: perform automated coordinate quality cleaning
#'   (drops (0,0), missing, out-of-bounds, and equal lat/lon records). Default `FALSE`.
#' @param iucn Logical: calculate IUCN Red List Criterion B metrics (EOO in km2,
#'   AOO in km2, and preliminary status). Default `FALSE`.
#' @param show_eoo Logical or `NULL`: overlay the IUCN Extent of Occurrence (Minimum
#'   Convex Polygon) on the map. Defaults to `iucn`.
#' @param iucn_medium Character: habitat medium constraint for IUCN EOO polygon.
#'   One of `"auto"` (default; auto-detects land vs marine based on occurrence locations or `keep_medium`),
#'   `"land"` (constrains EOO polygon to landmass, excluding oceans/seas),
#'   `"water"` (constrains EOO polygon to ocean/water bodies, excluding landmass), or
#'   `"all"` (unconstrained geometric Minimum Convex Polygon).
#' @param iucn_taxa Character vector or `NULL`: specific species/taxa to evaluate
#'   for IUCN Red List Criterion B (default `NULL` evaluates all taxa in data).
#' @param iucn_species Optional character vector alias for `iucn_taxa`.
#' @param show_taxa Character vector or `NULL`: specific species/taxa to display
#'   occurrence points for on the map. On IUCN maps (`iucn = TRUE`), this defaults
#'   to the species evaluated by `iucn_taxa`. Pass species names here to display
#'   points for other or additional species.
#' @param show_species,iucn_points_taxa Optional character vector aliases for `show_taxa`.
#' @param highlight_type Logical: highlight type specimens (Holotypes, Syntypes,
#'   etc.) with prominent diamond symbols. Default `FALSE`.
#' @param type_status_col Character or `NULL`: column name for specimen type status.
#' @param type_fill Character or `NULL`: fill color for highlighted type specimens.
#'   Can be `"species"` or `"same"` to match the color of the corresponding species
#'   in the legend (e.g. green diamond for green species), `"gold"` / `"#FFD700"`
#'   for gold diamond, or any custom color string. Default is `"species"`.
#' @param type_size Numeric or `NULL`: size of highlighted type symbol. If `NULL`
#'   (default), scaled to 1.6x the regular point size.
#' @param type_shape Integer: shape code for highlighted type specimens (default `23`, diamond).
#' @param facet Logical: split multiple species/groups into a multi-panel plate.
#'   Default `FALSE`.
#' @param facet_by Character or `NULL`: column name to facet by (defaults to `color_by`).
#' @param facet_cols Integer or `NULL`: number of columns in facet grid.
#' @param facet_rows Integer or `NULL`: number of rows in facet grid.
#' @param facet_layout Character or `NULL`: preset stacking layout for faceted maps:
#'   `"row"` or `"horizontal"` to stack panels in 1 row;
#'   `"col"`, `"column"`, or `"vertical"` to stack panels in 1 column.
#' @param facet_italic Logical: whether species names in facet panel headers
#'   should be formatted in italics. Default `TRUE`.
#' @param point_size Numeric or `NULL`: override point size directly.
#' @param point_alpha Numeric or `NULL`: override point transparency (0-1).
#' @param scalebar_pos Character or `NULL`: position for scale bar:
#'   `"bl"` (default, bottom-left), `"br"` (bottom-right), `"tl"` (top-left), or `"tr"` (top-right).
#' @param north_arrow_pos Character or `NULL`: position for north arrow:
#'   `"tr"` (default, top-right), `"tl"` (top-left), `"bl"` (bottom-left), or `"br"` (bottom-right).
#' @param north_arrow_scale Numeric or `NULL`: scaling factor for the north arrow
#'   (defaults to `0.75` for faceted multi-panel plates, `1.0` for single maps).
#'
#' @return A named list invisibly, containing:
#'   - `p`: the main `ggplot` map object (or primary grid map when `grid_metric = "both"`).
#'   - `p_richness`: the species richness `ggplot` object (when a grid is rendered).
#'   - `p_records`: the occurrence records `ggplot` object (when a grid is rendered).
#'   - `points`: the processed `sf` spatial points data.
#'   - `grid_sf`: the `sf` grid polygon object with counts (if `grid_km` is used).
#'   - `iucn`: IUCN evaluation list (if `iucn = TRUE`).
#'   - `cleaning`: coordinate cleaning report (if `clean_coords = TRUE`).
#'   - `crs`: the coordinate reference system string used.
#'   - `epsg`: the EPSG code (or `NULL`).
#'   - `extent`: numeric bounding box `c(xmin, ymin, xmax, ymax)` in WGS84.
#'   - `tier`: the scale tier (`"local"`, `"regional"`, `"continental"`, `"global"`).
#'   - `files`: character vector of paths to all created output files.
#'   - `methods_file`: path to the reproducibility methods summary file.
#'   - `n_records`: number of occurrence records used.
#'   - `protected_areas`: `sf` polygon layer of protected areas with metrics (if generated).
#'   - `protected_areas_counts_df`: data frame of per-protected-area occurrence counts and species richness.
#'   - `protected_areas_summary_df`: data frame of conservation summary metrics across protected areas.
#'   - `grid_counts_df`: data frame of grid centroid coordinates and counts.
#'   - `params`: named list of input parameters used to construct the map.
#' @export
#' @examples
#' \dontrun{
#' run_map("my_localities.csv")
#' # Species richness grid (points hidden by default to show cells clearly)
#' run_map("my_localities.csv", grid_km = 50, grid_metric = "richness")
#' # Generate both species richness and point density maps
#' run_map("my_localities.csv", grid_km = 50, grid_metric = "both")
#' # Add country boundaries and water bodies
#' run_map("my_localities.csv", show_countries = TRUE, show_water = TRUE)
#' # Direct taxon query from GBIF + IUCN Red List evaluation + Zootaxa preset
#' run_map("Panthera onca", iucn = TRUE, journal = "zootaxa")
#' }
#'
#' @importFrom grDevices dev.cur
#' @importFrom utils tail write.csv modifyList
#' @export
run_map <- function(data_path, output_dir = NULL,
                    color_by = NULL, shape_by = NULL,
                    scale = NULL, projection = NULL, auto_scale = TRUE,
                    show_elevation = FALSE, show_topo = FALSE,
                    show_ecoregions = FALSE,
                    show_protected_areas = FALSE, protected_areas = NULL,
                    protected_areas_fill = NULL, protected_areas_border = NULL,
                    protected_areas_alpha = NULL, protected_areas_lwd = NULL,
                    protected_areas_width = NULL, protected_areas_linetype = NULL,
                    elevation_scope = "both", elevation_limits = NULL, show_altitude = NULL,
                    water_depth = NULL,
                    grid_km = NULL, grid_bbox = NULL, grid_csv = FALSE,
                    show_points = NULL,
                    show_terrain = FALSE, terrain_zoom = 6,
                    show_cities = NULL, city_labels = TRUE, show_roads = NULL,
                    admin_level = NULL, show_countries = NULL,
                    show_states = NULL, show_counties = NULL,
                    show_water = FALSE, show_ocean = NULL,
                    show_lakes = NULL, show_rivers = NULL,
                    show_land = TRUE,
                    map_fill = NULL, land_fill = NULL, fill_color = NULL,
                    admin_color = NULL, admin_width = NULL, admin_lwd = NULL, admin_colour = NULL,
                    country_color = NULL, country_width = NULL, country_lwd = NULL, country_colour = NULL, country_border = NULL,
                    state_color = NULL, state_width = NULL, state_lwd = NULL, state_colour = NULL,
                    province_color = NULL, province_colour = NULL, province_width = NULL, province_lwd = NULL,
                    state_border = NULL,
                    grid_color = NULL, grid_linewidth = NULL,
                    grid_linetype = NULL, grid_steps = "auto",
                    grid_categories = NULL, grid_category_width = NULL,
                    grid_step_size = NULL, grid_breaks = NULL,
                    grid_category_method = "auto", grid_category_labels = NULL,
                    grid_metric = "auto", grid_palette = NULL,
                    keep_medium = "all",
                    map_aes = list(), scalebar = TRUE, north_arrow = TRUE,
                    graticule = TRUE, graticule_lines = FALSE, inset_map = FALSE,
                    inset_pos = NULL,
                    formats = "pdf", dpi = 600,
                    width = 10, height = NULL, filename = "map",
                    palette = NULL, legend_position = "bottom",
                    show_species_legend = TRUE, species_legend = NULL,
                    gis_formats = NULL,
                    seed = 12345, verbose = TRUE,
                    map_bounds = NULL, bbox_buffer = 0.03,
                    journal = NULL, journal_cols = 2, preset = NULL,
                    clean_coords = FALSE,
                    iucn = FALSE, show_eoo = NULL, iucn_medium = "auto",
                    iucn_taxa = NULL, iucn_species = NULL,
                    show_taxa = NULL, show_species = NULL, iucn_points_taxa = NULL,
                    highlight_type = FALSE, type_status_col = NULL,
                    type_fill = "species", type_size = NULL, type_shape = 23,
                    facet = FALSE, facet_by = NULL,
                    facet_cols = NULL, facet_rows = NULL,
                    facet_layout = NULL, facet_italic = TRUE,
                    point_color = NULL, point_shape = NULL, point_size = NULL,
                    point_alpha = NULL, point_border_color = NULL,
                    point_border_linewidth = NULL, point_border_linetype = NULL,
                    scalebar_pos = NULL, north_arrow_pos = NULL,
                    north_arrow_scale = NULL) {
  set.seed(seed)
  if (!is.null(species_legend)) show_species_legend <- species_legend
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  msg <- function(...) if (verbose) cat(..., "\n")
  n_records <- 0L
  data_label <- ""

  ## ---- elevation / altitude / water depth options ----
  elev_active <- FALSE
  scope_val <- tolower(elevation_scope)
  if (!is.null(water_depth)) {
    if (isFALSE(water_depth)) {
      scope_val <- "land"
    } else if (isTRUE(water_depth) && scope_val == "land") {
      scope_val <- "both"
    }
  }

  if (!is.null(show_altitude)) {
    if (is.character(show_altitude)) {
      elev_active <- TRUE
      scope_val <- tolower(show_altitude)
    } else if (isTRUE(show_altitude)) {
      elev_active <- TRUE
    }
  } else if (is.character(show_elevation)) {
    elev_active <- TRUE
    scope_val <- tolower(show_elevation)
  } else if (isTRUE(show_elevation)) {
    elev_active <- TRUE
  } else if (isTRUE(show_topo)) {
    elev_active <- TRUE
    scope_val <- "both"
  }

  if (!is.null(water_depth) && isFALSE(water_depth)) {
    scope_val <- "land"
  }

  if (!scope_val %in% c("land", "sea", "both")) {
    warning("elevation_scope must be 'land', 'sea', or 'both'. Defaulting to 'both'.")
    scope_val <- "both"
  }


  ## ---- admin level parsing ----
  if (!is.null(admin_level)) {
    if (is.character(admin_level)) {
      al_lower <- tolower(admin_level)
      admin_level <- if (al_lower %in% c("none", "null", "false")) {
        NULL
      } else if (al_lower %in% c("country", "countries", "0")) {
        0L
      } else if (al_lower %in% c("state", "states", "province", "provinces", "department", "departments", "1")) {
        1L
      } else if (al_lower %in% c("county", "counties", "municipality", "municipalities", "district", "districts", "2")) {
        1L
      } else {
        warning("Unknown admin_level '", admin_level, "'; using NULL (no boundaries).")
        NULL
      }
    } else {
      admin_level <- as.integer(admin_level)
      if (identical(admin_level, 2L)) {
        admin_level <- 1L
      } else if (!admin_level %in% 0:1) {
        admin_level <- NULL
      }
    }
  }
  if (isTRUE(show_counties)) {
    admin_level <- 1L
  } else if (isTRUE(show_states)) {
    if (is.null(admin_level) || admin_level < 1L) admin_level <- 1L
  } else if (isTRUE(show_countries)) {
    if (is.null(admin_level)) admin_level <- 0L
  }

  # Auto-enable admin_level if user supplied custom country or state border aesthetics
  has_country_custom <- !is.null(country_color) || !is.null(country_colour) || !is.null(country_border) ||
                        !is.null(country_width) || !is.null(country_lwd) ||
                        !is.null(admin_color) || !is.null(admin_colour) || !is.null(admin_width) || !is.null(admin_lwd)
  if (has_country_custom && is.null(admin_level)) {
    admin_level <- 0L
  }
  has_state_custom <- !is.null(state_color) || !is.null(state_colour) || !is.null(state_border) ||
                      !is.null(province_color) || !is.null(province_colour) ||
                      !is.null(state_width) || !is.null(state_lwd) ||
                      !is.null(province_width) || !is.null(province_lwd)
  if (has_state_custom && (is.null(admin_level) || admin_level < 1L)) {
    admin_level <- 1L
  }

  ## ---- show_points default ----
  if (is.null(show_points)) {
    show_points <- is.null(grid_km)
  }

  ## ---- journal preset resolution ----
  if (!is.null(preset)) {
    preset_clean <- tolower(trimws(preset))
    if (preset_clean %in% c("1col", "1", "single")) {
      journal_cols <- 1
    } else if (preset_clean %in% c("1.5col", "1.5")) {
      journal_cols <- 1.5
    } else if (preset_clean %in% c("2col", "2", "double")) {
      journal_cols <- 2
    } else if (is.null(journal)) {
      journal <- preset
    }
  }

  if (!is.null(journal)) {
    j_res <- resolve_journal(journal, columns = journal_cols, dpi = dpi)
    if (!is.null(j_res)) {
      width <- j_res$width
      height <- j_res$height
      dpi <- j_res$dpi
      msg("Journal Preset: ", j_res$journal_name, " (", j_res$column_spec, ", ", dpi, " DPI)")
    }
  }

  ## ---- input ----
  if (missing(data_path) || is.null(data_path)) {
    stop("Argument 'data_path' is required. Please provide a path to a local spreadsheet (CSV, TSV, Excel), a data.frame, or a taxon name for GBIF search.")
  } else if (is.data.frame(data_path)) {
    dat <- standardize_input_df(data_path, "user-supplied data.frame")
    data_label <- "user-supplied data.frame"
  } else if (is.character(data_path) && length(data_path) == 1L && !file.exists(data_path) && is_taxon_name(data_path)) {
    dat <- fetch_gbif(data_path, verbose = verbose)
    data_label <- paste0("GBIF occurrence query: '", data_path, "'")
  } else {
    if (!file.exists(data_path)) stop("File not found: ", data_path)
    dat <- read_input(data_path)
    data_label <- data_path
  }
  dat$lat <- as.numeric(dat$lat)
  dat$lon <- as.numeric(dat$lon)

  ## ---- coordinate quality cleaning ----
  clean_report <- NULL
  clean_obj <- NULL
  if (isTRUE(clean_coords)) {
    cl_res <- clean_coordinates(dat, verbose = verbose)
    dat <- cl_res$data
    clean_report <- cl_res$report
    clean_obj <- cl_res
  }

  # --- Auto-detect color_by if NULL, respecting point_color ---

  # If point_color is provided, still auto-detect color_by for legend labeling,
  # but use point_color for actual coloring
  if (is.null(color_by)) {
    # Try auto-detection from common species/group column variants
    species_variants <- c("species", "sp", "group", "grouping")
    color_by <- names(dat)[tolower(names(dat)) %in% tolower(species_variants)][1]

    # Fallback: use first column that isn't lat/lon
    if (is.na(color_by) || color_by == "NULL") {
      coord_cols <- unique(c(as.character(dat$lat), as.character(dat$lon)))
      color_by <- setdiff(names(dat), coord_cols)[1]
    }

    # If still NULL, warn and use first column
    if (is.null(color_by) || color_by == "NULL") {
      warning("Could not auto-detect grouping column. Using first column: ",
              names(dat)[1])
      color_by <- names(dat)[1]
    }
  }

  if (!color_by %in% names(dat)) {
    stop("Column '", color_by, "' not found in the input data. ",
         "Set color_by to one of: ", paste(names(dat), collapse = ", "), ".")
  }
  if (!is.null(shape_by) && !shape_by %in% names(dat)) {
    stop("Column '", shape_by, "' not found in the input data.")
  }

  # Ensure color_by and shape_by have consistent, deterministic factor ordering
  if (!is.factor(dat[[color_by]])) {
    levs_init <- sort(unique(as.character(dat[[color_by]])))
    dat[[color_by]] <- factor(as.character(dat[[color_by]]), levels = levs_init)
  }
  if (!is.null(shape_by) && !is.factor(dat[[shape_by]])) {
    shp_init <- sort(unique(as.character(dat[[shape_by]])))
    dat[[shape_by]] <- factor(as.character(dat[[shape_by]]), levels = shp_init)
  }
  n_records <- nrow(dat)
  msg("Input:", data_label, "|", n_records, "records | colour by:", color_by)

  ## ---- resolve palette ----
  palette_resolved <- resolve_palette(palette)

  ## ---- points, dateline & projection ----
  points <- sf::st_as_sf(dat, coords = c("lon", "lat"), crs = 4326)
  dateline <- crosses_dateline(points)
  if (dateline) {
    points <- wrap_lons(points)
    msg("Data crosses the antimeridian: longitudes wrapped to 0-360")
  }

  ## ---- user-defined map bounds ----
  has_map_bounds <- !is.null(map_bounds)
  if (has_map_bounds) {
    if (is.numeric(map_bounds) && length(map_bounds) == 4) {
      xmin <- map_bounds[1]; xmax <- map_bounds[2]
      ymin <- map_bounds[3]; ymax <- map_bounds[4]
      if (xmin >= xmax || ymin >= ymax) {
        stop("map_bounds must satisfy xmin < xmax and ymin < ymax: c(xmin, xmax, ymin, ymax).")
      }
      msg("Using user-defined map bounds: x [", xmin, ", ", xmax, "], y [", ymin, ", ", ymax, "]")
      cent <- c((xmin + xmax) / 2, (ymin + ymax) / 2)
      span_km <- point_span_km(sf::st_as_sf(data.frame(lon = c(xmin, xmax), lat = c(ymin, ymax)),
                                            coords = c("lon", "lat"), crs = 4326))
      tier <- scale_tier(span_km)
    } else if (inherits(map_bounds, "sf")) {
      mb_wgs <- sf::st_transform(map_bounds, 4326)
      bb_user <- sf::st_bbox(mb_wgs)
      cent <- c((bb_user["xmin"] + bb_user["xmax"]) / 2, (bb_user["ymin"] + bb_user["ymax"]) / 2)
      span_km <- point_span_km(sf::st_as_sf(data.frame(lon = c(bb_user["xmin"], bb_user["xmax"]),
                                                        lat = c(bb_user["ymin"], bb_user["ymax"])),
                                            coords = c("lon", "lat"), crs = 4326))
      tier <- scale_tier(span_km)
      msg("Using user-defined map bounds from sf geometry")
    } else {
      stop("map_bounds must be a numeric vector c(xmin, xmax, ymin, ymax) in WGS84 lon/lat degrees, or an sf object.")
    }
  } else {
    cent <- colMeans(sf::st_coordinates(points))
    span_km <- point_span_km(points)
    tier <- scale_tier(span_km)
  }

  if (is.null(projection)) {
    crs_auto <- auto_crs(cent, span_km, dateline)
    crs_map <- crs_auto$crs
    epsg <- crs_auto$epsg
    crs_label <- crs_auto$label
    msg("Projection: ", crs_label)
  } else {
    epsg <- as.integer(projection)
    crs_map <- sf::st_crs(epsg)
    crs_label <- paste0("EPSG:", epsg, " (user-supplied)")
    msg("Projection: user-supplied EPSG", epsg)
  }
  pts <- sf::st_transform(points, crs_map)

  if (has_map_bounds) {
    if (is.numeric(map_bounds) && length(map_bounds) == 4) {
      poly_dens <- densify_bbox_ll(map_bounds[1], map_bounds[2], map_bounds[3], map_bounds[4], n = 50)
      poly_proj <- sf::st_transform(poly_dens, crs_map)
      bb0 <- sf::st_bbox(poly_proj)
    } else if (inherits(map_bounds, "sf")) {
      poly_proj <- sf::st_transform(map_bounds, crs_map)
      bb0 <- sf::st_bbox(poly_proj)
    }
  } else {
    buf_frac <- if (!is.null(bbox_buffer) && is.numeric(bbox_buffer)) bbox_buffer else 0.03
    bb0 <- expand_bbox(sf::st_bbox(pts), frac = buf_frac)
  }

  ## ---- scale tier ----
  scale_auto <- NULL
  if (is.null(scale)) {
    scale <- tier_ne_scale(tier)
    scale_auto <- paste0("auto (", tier, " tier)")
  } else {
    scale <- as.integer(scale)
    scale_auto <- "user-supplied"
  }
  if (is.null(show_cities)) show_cities <- FALSE
  if (is.null(show_roads)) show_roads <- FALSE
  msg("Scale tier:", tier, "(span ~", round(span_km), "km) | NE 1:", scale, "m")

  ## ---- extent (for layer fetching) ----
  bbox_sf0 <- sf::st_as_sfc(sf::st_bbox(bb0, crs = crs_map))

  ## ---- base layers ----
  if (isTRUE(show_countries) && is.null(admin_level)) {
    admin_level <- 0
  }
  msg("Fetching Natural Earth layers (scale 1:", scale, "m)...")
  layers <- build_layers(scale, crs_map, bbox_sf0, show_cities, show_roads,
                         admin_level = admin_level, show_water = show_water,
                         show_ocean = show_ocean, show_lakes = show_lakes,
                         show_rivers = show_rivers)
  layers_used <- names(Filter(Negate(is.null), layers))

  ## ---- medium filter ----
  medium_dropped <- 0L
  if (keep_medium %in% c("land", "water")) {
    med_layer <- NULL
    try(med_layer <- rnaturalearth::ne_download(
      scale = as.numeric(scale),
      type = if (keep_medium == "water") "ocean" else "countries",
      category = if (keep_medium == "water") "physical" else "cultural",
      returnclass = "sf", destdir = cache_dir()), silent = TRUE)
    if (is.null(med_layer)) {
      message("  - medium filter requested but base layer unavailable, skipped")
    } else {
      if (dateline) med_layer <- wrap_lons(med_layer)
      s2_was <- suppressMessages(suppressWarnings(sf::sf_use_s2(FALSE)))
      poly_union <- tryCatch(suppressWarnings(sf::st_union(sf::st_make_valid(med_layer))), error = function(e) sf::st_make_valid(med_layer))
      suppressMessages(suppressWarnings(sf::sf_use_s2(s2_was)))
      hits <- lengths(sf::st_intersects(points, poly_union))
      drop <- hits == 0L
      if (any(drop)) {
        n_drop <- sum(drop)
        medium_dropped <- n_drop
        pts <- pts[!drop, , drop = FALSE]
        points <- points[!drop, , drop = FALSE]
        dat <- dat[!drop, , drop = FALSE]
        n_records <- nrow(dat)
        msg("Medium filter (keep_medium = '", keep_medium, "'): dropped ", n_drop,
            " point(s) falling outside ", keep_medium, " (", n_records, " retained).")
      } else {
        msg("Medium filter (keep_medium = '", keep_medium, "'): all ", nrow(dat),
            " point(s) fall on ", keep_medium, "; none dropped.")
      }
    }
  }

  ## ---- final extent ----
  bb <- bb0
  bbox_sf <- sf::st_as_sfc(sf::st_bbox(bb, crs = crs_map))
  extent_ll <- frame_ll_bbox(bbox_sf)

  ## ---- auto-fit figure height if not supplied ----
  dx_span <- bb[["xmax"]] - bb[["xmin"]]
  dy_span <- bb[["ymax"]] - bb[["ymin"]]
  aspect_map <- if (dx_span > 0) dy_span / dx_span else 0.75

  # Determine effective facet rows and cols
  eff_facet_cols <- facet_cols
  eff_facet_rows <- facet_rows
  if (isTRUE(facet) || !is.null(facet_by)) {
    f_var <- if (!is.null(facet_by)) facet_by else color_by
    n_groups <- if (!is.null(f_var) && f_var %in% names(dat)) length(unique(dat[[f_var]])) else 1L
    if (!is.null(facet_layout)) {
      fl <- tolower(facet_layout)
      if (fl %in% c("row", "rows", "horizontal", "h")) {
        eff_facet_rows <- 1L
        eff_facet_cols <- n_groups
      } else if (fl %in% c("col", "column", "columns", "vertical", "v", "stack")) {
        eff_facet_cols <- 1L
        eff_facet_rows <- n_groups
      }
    } else {
      if (!is.null(eff_facet_cols) && is.null(eff_facet_rows)) {
        eff_facet_rows <- ceiling(n_groups / eff_facet_cols)
      } else if (is.null(eff_facet_cols) && !is.null(eff_facet_rows)) {
        eff_facet_cols <- ceiling(n_groups / eff_facet_rows)
      } else if (is.null(eff_facet_cols) && is.null(eff_facet_rows)) {
        eff_facet_cols <- min(3L, n_groups)
        eff_facet_rows <- ceiling(n_groups / eff_facet_cols)
      }
    }
  }

  if (is.null(height)) {
    # Vertical allowance for margins, axes coordinates, ticks, and legend
    extra_h <- if (legend_position %in% c("bottom", "top")) 1.4 else 0.8
    cols_eff <- if (!is.null(eff_facet_cols)) eff_facet_cols else 1L
    rows_eff <- if (!is.null(eff_facet_rows)) eff_facet_rows else 1L
    panel_w <- max(2, (width - 0.8) / cols_eff)
    calc_h <- (panel_w * aspect_map * rows_eff) + extra_h
    height <- round(max(3.5, min(18, calc_h)), 2)
    msg("Auto-fitted figure height: ", height, " in (width: ", width, " in, aspect: ", round(aspect_map, 3), ")")
  }

  ## ---- optional overlays ----
  terrain_df <- NULL
  if (show_terrain) {
    msg("Fetching terrain hillshade (elevatr/terra) [scope: ", scope_val, "]...")
    terrain_df <- terrain_layer(bbox_sf, crs_map, terrain_zoom, span_km = span_km, scope = scope_val)
  }
  elev_df <- NULL
  if (elev_active) {
    msg("Fetching elevation/altitude [scope: ", scope_val, "]...")
    elev_df <- elevation_layer(bbox_sf, crs_map, terrain_zoom, layers$land,
                               scope = scope_val, span_km = span_km)
  } else if (show_terrain) {
    # Hypsometric base for physical terrain
    elev_df <- tryCatch(
      elevation_layer(bbox_sf, crs_map, terrain_zoom, layers$land,
                      scope = "land", span_km = span_km),
      error = function(e) NULL
    )
  }
  eco <- NULL
  if (show_ecoregions) {
    msg("Fetching WWF ecoregions...")
    eco <- ecoregions_layer(bbox_sf, crs_map)
  }
  pa <- NULL
  pa_counts_df <- NULL
  pa_summary_df <- NULL
  if (isTRUE(show_protected_areas) || !is.null(protected_areas)) {
    msg("Fetching and processing protected conservation areas (WDPA)...")
    pa_src <- if (!is.null(protected_areas) && !isFALSE(protected_areas)) protected_areas else NULL
    pa <- tryCatch(
      protected_areas_layer(bbox_sf, crs_map, pts = pts, source = pa_src),
      error = function(e) {
        message("  - protected areas layer failed, skipped (", conditionMessage(e), ")")
        NULL
      }
    )
    if (!is.null(pa) && nrow(pa) > 0L) {
      msg("Calculating protected areas species richness and occurrence counts...")
      pa_analysis <- analyze_protected_areas(pa, pts, species_col = color_by)
      if (!is.null(pa_analysis)) {
        pa <- pa_analysis$pa_sf
        pa_counts_df <- pa_analysis$counts_df
        pa_summary_df <- pa_analysis$summary_df
      }
    }
  }
  grid_sf <- NULL
  grid_counts_df <- NULL
  grid_richness_summary_df <- NULL
  grid_density_summary_df <- NULL
  grid_summary_df <- NULL
  actual_metric <- "none"
  metric_label <- NULL
  if (!is.null(grid_km)) {
    if (tier %in% c("continental", "global")) {
      message("  - grid_km is approximate at ", tier, " scale (non-metric projection)")
    }
    # Determine grid extent
    if (!is.null(grid_bbox)) {
      # grid_bbox = c(xmin, xmax, ymin, ymax) in WGS84
      grid_bbox_sf <- sf::st_sfc(
        sf::st_polygon(list(matrix(c(grid_bbox[1], grid_bbox[3],
                                      grid_bbox[2], grid_bbox[3],
                                      grid_bbox[2], grid_bbox[4],
                                      grid_bbox[1], grid_bbox[4],
                                      grid_bbox[1], grid_bbox[3]), ncol = 2, byrow = TRUE))),
        crs = 4326)
      grid_bbox_proj <- sf::st_transform(grid_bbox_sf, crs_map)
      g <- sf::st_make_grid(grid_bbox_proj, cellsize = grid_km * 1000, square = TRUE)
    } else {
      g <- sf::st_make_grid(pts, cellsize = grid_km * 1000, square = TRUE)
    }
    idx <- sf::st_intersects(pts, g)

    # Determine species/grouping column
    species_col <- if (!is.null(color_by) && color_by %in% names(dat)) {
      color_by
    } else if ("species" %in% names(dat)) {
      "species"
    } else {
      NULL
    }
    n_sp <- if (!is.null(species_col)) length(unique(dat[[species_col]])) else 1L

    m_low <- tolower(grid_metric)
    if (m_low %in% c("species", "richness")) {
      actual_metric <- "richness"
    } else if (m_low %in% c("records", "points", "count")) {
      actual_metric <- "records"
    } else if (m_low %in% c("both", "all")) {
      actual_metric <- "both"
    } else {
      actual_metric <- if (!is.null(species_col) && n_sp > 1L) "richness" else "records"
    }

    pt_cells <- unlist(idx)
    pt_rows <- rep(seq_along(idx), lengths(idx))

    # Records count per cell
    cnt_records <- tabulate(pt_cells, nbins = length(g))

    # Species richness per cell
    cnt_richness <- integer(length(g))
    if (!is.null(species_col)) {
      sp_in_cell <- tapply(dat[[species_col]][pt_rows], pt_cells, function(s) length(unique(s)))
      cnt_richness[as.integer(names(sp_in_cell))] <- as.integer(sp_in_cell)
    } else {
      cnt_richness <- cnt_records
    }

    keep <- cnt_records > 0
    grid_sf <- sf::st_sf(
      geometry = g[keep],
      count_records = cnt_records[keep],
      count_richness = cnt_richness[keep]
    )

    if (actual_metric == "richness") {
      grid_sf$count <- grid_sf$count_richness
      metric_label <- "Species per cell"
    } else if (actual_metric == "records") {
      grid_sf$count <- grid_sf$count_records
      metric_label <- "Records per cell"
    } else {
      grid_sf$count <- grid_sf$count_richness
      metric_label <- "Species Richness & Records"
    }

    # Per-species and summary grid counts
    grid_cells <- g[keep]
    species_levels <- if (!is.null(species_col)) unique(dat[[species_col]]) else "total"
    cent_sf <- sf::st_centroid(grid_cells)
    cent_ll <- sf::st_coordinates(sf::st_transform(cent_sf, 4326))
    cent_proj <- sf::st_coordinates(cent_sf)

    grid_counts_df <- data.frame(
      grid_id = seq_len(length(grid_cells)),
      lon_center = round(cent_ll[, 1], 5),
      lat_center = round(cent_ll[, 2], 5),
      x_proj = round(cent_proj[, 1], 1),
      y_proj = round(cent_proj[, 2], 1),
      species_richness = cnt_richness[keep],
      total_records = cnt_records[keep],
      stringsAsFactors = FALSE
    )

    if (!is.null(species_col) && length(species_levels) > 0L) {
      for (sp in species_levels) {
        sp_rows <- which(dat[[species_col]] == sp)
        sp_idx <- idx[sp_rows]
        sp_tab <- tabulate(unlist(sp_idx), nbins = length(g))
        grid_counts_df[[sp]] <- sp_tab[keep]
      }
    }
    grid_counts_df$total <- grid_counts_df$total_records

    # Dedicated Summary Tables: Species Richness and Occurrence Record Density
    # Quantifying strictly occupied cells (excluding empty cells across geographic bounding extent)
    rich_occ <- cnt_richness[cnt_richness > 0]
    rec_occ <- cnt_records[cnt_records > 0]

    grid_richness_summary_df <- data.frame(
      total_cells = length(rich_occ),
      min_species = if (length(rich_occ) > 0) min(rich_occ) else 0L,
      max_species = if (length(rich_occ) > 0) max(rich_occ) else 0L,
      mean_species = if (length(rich_occ) > 0) round(mean(rich_occ), 2) else 0,
      median_species = if (length(rich_occ) > 0) round(stats::median(rich_occ), 2) else 0,
      stringsAsFactors = FALSE
    )

    grid_density_summary_df <- data.frame(
      total_cells = length(rec_occ),
      min_records = if (length(rec_occ) > 0) min(rec_occ) else 0L,
      max_records = if (length(rec_occ) > 0) max(rec_occ) else 0L,
      mean_records = if (length(rec_occ) > 0) round(mean(rec_occ), 2) else 0,
      median_records = if (length(rec_occ) > 0) round(stats::median(rec_occ), 2) else 0,
      stringsAsFactors = FALSE
    )

    grid_summary_df <- grid_richness_summary_df

    if (verbose) {
      cat("\nEqual-Area Grid Species Richness Summary (", grid_km, " km cells):\n", sep = "")
      print(grid_richness_summary_df, row.names = FALSE)
      cat("\nEqual-Area Grid Record Density Summary (", grid_km, " km cells):\n", sep = "")
      print(grid_density_summary_df, row.names = FALSE)
      cat("\n")
    }
  }

  ## ---- graticule ----
  grat <- NULL
  if (graticule) {
    grat <- tryCatch(
      sf::st_graticule(sf::st_transform(bbox_sf, 4326)),
      error = function(e) NULL)
  }

  ## ---- assemble plot ----
  ma <- modifyList(default_map_aes(), map_aes)
  # Map fill color selection (preserves default #F5F0E6 when NULL)
  eff_map_fill <- if (!is.null(map_fill)) map_fill else
                  if (!is.null(land_fill)) land_fill else
                  if (!is.null(fill_color)) fill_color else
                  if (!is.null(map_aes$map_fill)) map_aes$map_fill else
                  ma$land_fill
  ma$land_fill <- eff_map_fill
  ma$map_fill <- eff_map_fill

  # Country border color customization (default "grey45")
  eff_country_col <- if (!is.null(country_color)) country_color else
                     if (!is.null(country_colour)) country_colour else
                     if (!is.null(country_border)) country_border else
                     if (!is.null(admin_color)) admin_color else
                     if (!is.null(admin_colour)) admin_colour else
                     if (!is.null(map_aes$country_color)) map_aes$country_color else
                     if (!is.null(map_aes$country_colour)) map_aes$country_colour else
                     if (!is.null(map_aes$country_border)) map_aes$country_border else
                     if (!is.null(map_aes$admin_color)) map_aes$admin_color else
                     if (!is.null(map_aes$admin_colour)) map_aes$admin_colour else
                     if (!is.null(map_aes$land_border)) map_aes$land_border else
                     ma$land_border
  ma$land_border <- eff_country_col
  ma$country_color <- eff_country_col
  ma$country_colour <- eff_country_col
  ma$country_border <- eff_country_col

  # Country border width customization (default 0.25)
  eff_country_width <- if (!is.null(country_width)) country_width else
                       if (!is.null(country_lwd)) country_lwd else
                       if (!is.null(admin_width)) admin_width else
                       if (!is.null(admin_lwd)) admin_lwd else
                       if (!is.null(map_aes$country_width)) map_aes$country_width else
                       if (!is.null(map_aes$country_lwd)) map_aes$country_lwd else
                       if (!is.null(map_aes$admin_width)) map_aes$admin_width else
                       if (!is.null(map_aes$admin_lwd)) map_aes$admin_lwd else
                       if (!is.null(map_aes$land_lwd)) map_aes$land_lwd else
                       ma$land_lwd
  ma$land_lwd <- as.numeric(eff_country_width)
  ma$country_width <- as.numeric(eff_country_width)
  ma$country_lwd <- as.numeric(eff_country_width)

  # State / Province border color customization (default "grey60")
  eff_state_col <- if (!is.null(state_color)) state_color else
                   if (!is.null(state_colour)) state_colour else
                   if (!is.null(state_border)) state_border else
                   if (!is.null(province_color)) province_color else
                   if (!is.null(province_colour)) province_colour else
                   if (!is.null(map_aes$state_color)) map_aes$state_color else
                   if (!is.null(map_aes$state_colour)) map_aes$state_colour else
                   if (!is.null(map_aes$province_color)) map_aes$province_color else
                   if (!is.null(map_aes$province_colour)) map_aes$province_colour else
                   if (!is.null(map_aes$admin1_colour)) map_aes$admin1_colour else
                   if (!is.null(map_aes$admin1_color)) map_aes$admin1_color else
                   ma$admin1_colour
  ma$admin1_colour <- eff_state_col
  ma$admin1_color <- eff_state_col
  ma$state_color <- eff_state_col
  ma$province_color <- eff_state_col

  # State / Province border width customization (default 0.15)
  eff_state_width <- if (!is.null(state_width)) state_width else
                     if (!is.null(state_lwd)) state_lwd else
                     if (!is.null(province_width)) province_width else
                     if (!is.null(province_lwd)) province_lwd else
                     if (!is.null(map_aes$state_width)) map_aes$state_width else
                     if (!is.null(map_aes$state_lwd)) map_aes$state_lwd else
                     if (!is.null(map_aes$province_width)) map_aes$province_width else
                     if (!is.null(map_aes$province_lwd)) map_aes$province_lwd else
                     if (!is.null(map_aes$admin1_lwd)) map_aes$admin1_lwd else
                     if (!is.null(map_aes$admin1_width)) map_aes$admin1_width else
                     ma$admin1_lwd
  ma$admin1_lwd <- as.numeric(eff_state_width)
  ma$admin1_width <- as.numeric(eff_state_width)
  ma$state_width <- as.numeric(eff_state_width)
  ma$province_width <- as.numeric(eff_state_width)
  if (!is.null(protected_areas_fill)) ma$protected_areas_fill <- protected_areas_fill
  if (!is.null(protected_areas_border)) ma$protected_areas_border <- protected_areas_border
  if (!is.null(protected_areas_alpha)) ma$protected_areas_alpha <- protected_areas_alpha
  if (!is.null(protected_areas_lwd)) ma$protected_areas_lwd <- protected_areas_lwd
  if (!is.null(protected_areas_width)) ma$protected_areas_lwd <- protected_areas_width
  if (!is.null(protected_areas_linetype)) ma$protected_areas_linetype <- protected_areas_linetype
  levs <- if (is.factor(dat[[color_by]])) levels(dat[[color_by]]) else sort(unique(dat[[color_by]]))
  n_grp <- length(levs)
  pal_vals <- palette_resolved[((seq_len(n_grp) - 1) %% length(palette_resolved)) + 1]
  names(pal_vals) <- levs

  # Prioritize custom point_color or custom named palette
  if (!is.null(point_color) && length(point_color) > 0) {
    if (!is.null(names(point_color)) && any(names(point_color) %in% levs)) {
      for (l in levs) {
        if (l %in% names(point_color)) pal_vals[l] <- point_color[[l]]
      }
    } else {
      pal_vals <- point_color[((seq_len(n_grp) - 1) %% length(point_color)) + 1]
      names(pal_vals) <- levs
    }
  } else if (!is.null(names(palette_resolved)) && any(names(palette_resolved) %in% levs)) {
    for (l in levs) {
      if (l %in% names(palette_resolved)) pal_vals[l] <- palette_resolved[[l]]
    }
  }

  auto_shape <- is.null(shape_by) && n_grp > 1L
  shape_col <- if (auto_shape) color_by else shape_by
  shp_vals <- NULL
  shp_levs <- NULL
  if (!is.null(shape_col)) {
    shp_levs <- if (identical(shape_col, color_by)) levs else if (is.factor(dat[[shape_col]])) levels(dat[[shape_col]]) else sort(unique(dat[[shape_col]]))
    n_shp <- length(shp_levs)
    shp_vals <- ma$shapes[((seq_len(n_shp) - 1) %/% length(palette_resolved)) %% length(ma$shapes) + 1]
    names(shp_vals) <- shp_levs
  }
  rendering <- if (auto_shape) {
    paste0("colour + shape per ", color_by, " (", n_grp, " groups)")
  } else if (!is.null(shape_by)) {
    paste0("colour by ", color_by, ", shape by ", shape_by)
  } else {
    paste0("colour only (", n_grp, " group(s))")
  }
  msg("Rendering:", rendering)

  mapping <- ggplot2::aes(fill = .data[[color_by]])
  if (!is.null(shape_col)) {
    mapping <- ggplot2::aes(fill = .data[[color_by]], shape = .data[[shape_col]])
  }
  fixed_shape <- if (is.null(shape_col)) 21 else NULL

  p <- ggplot2::ggplot()

  # Elevation/altitude raster
  if (!is.null(elev_df)) {
    vals <- elev_df$value[!is.na(elev_df$value)]
    min_v <- if (length(vals) > 0) min(vals) else 0
    max_v <- if (length(vals) > 0) max(vals) else 100

    if (!is.null(elevation_limits) && length(elevation_limits) == 2) {
      if (!is.na(elevation_limits[1])) min_v <- elevation_limits[1]
      if (!is.na(elevation_limits[2])) {
        target_max <- elevation_limits[2]
        pos_mask <- !is.na(elev_df$value) & elev_df$value > 0
        if (any(pos_mask)) {
          curr_max <- max(elev_df$value[pos_mask])
          if (curr_max > 0 && curr_max < target_max) {
            elev_df$value[pos_mask] <- elev_df$value[pos_mask] * (target_max / curr_max)
          }
        }
        max_v <- target_max
      }
    }

    min_extreme <- round(min_v)
    max_extreme <- round(max_v)
    min_lim <- min(min_v, min_extreme)
    max_lim <- max(max_v, max_extreme)

    elev_breaks <- calc_elevation_scale_breaks(min_v, max_v, scope_val)
    elev_lab_fn <- function(x) format(x, big.mark = "", scientific = FALSE, trim = TRUE)
    is_horiz <- legend_position %in% c("bottom", "top")
    cb_guide <- if (is_horiz) {
      ggplot2::guide_colorbar(
        direction = "horizontal",
        barwidth = ggplot2::unit(75, "mm"),
        barheight = ggplot2::unit(4, "mm"),
        title.position = "top",
        label.position = "bottom",
        ticks.colour = "black"
      )
    } else {
      ggplot2::guide_colorbar(
        direction = "vertical",
        barwidth = ggplot2::unit(4, "mm"),
        barheight = ggplot2::unit(65, "mm"),
        title.position = "top",
        label.position = "right",
        ticks.colour = "black"
      )
    }

    if (elev_active) {
      p <- p + ggplot2::geom_raster(
        data = elev_df,
        ggplot2::aes(x = .data$x, y = .data$y, fill = .data$value),
        alpha = ma$raster_alpha, show.legend = TRUE,
        key_glyph = ggplot2::draw_key_blank)

      if (scope_val == "land") {
        p <- p + ggplot2::scale_fill_gradientn(
          colours = ma$elevation_cols,
          name = "Elevation (m)",
          limits = c(0, max(1, max_lim)),
          breaks = elev_breaks,
          labels = elev_lab_fn,
          guide = cb_guide,
          na.value = "transparent")
      } else if (scope_val == "sea") {
        p <- p + ggplot2::scale_fill_gradientn(
          colours = ma$bathymetry_cols,
          name = "Depth (m)",
          limits = c(min(-1, min_lim), 0),
          breaks = elev_breaks,
          labels = elev_lab_fn,
          guide = cb_guide,
          na.value = "transparent")
      } else {
        # "both" - pin 0m at transition between bathymetry and elevation
        if (min_v < 0 && max_v > 0) {
          zero_pos <- (0 - min_lim) / (max_lim - min_lim)
          n_sea <- length(ma$bathymetry_cols)
          n_land <- length(ma$elevation_cols)
          sea_pos <- seq(0, zero_pos, length.out = n_sea)
          land_pos <- seq(zero_pos, 1, length.out = n_land)
          both_cols <- c(ma$bathymetry_cols, ma$elevation_cols[-1])
          both_pos <- c(sea_pos, land_pos[-1])
          p <- p + ggplot2::scale_fill_gradientn(
            colours = both_cols,
            values = both_pos,
            limits = c(min_lim, max_lim),
            breaks = elev_breaks,
            labels = elev_lab_fn,
            guide = cb_guide,
            name = "Elevation / Depth (m)",
            na.value = "transparent")
        } else if (max_v <= 0) {
          p <- p + ggplot2::scale_fill_gradientn(
            colours = ma$bathymetry_cols,
            name = "Depth (m)",
            limits = c(min(-1, min_lim), 0),
            breaks = elev_breaks,
            labels = elev_lab_fn,
            guide = cb_guide,
            na.value = "transparent")
        } else {
          p <- p + ggplot2::scale_fill_gradientn(
            colours = ma$elevation_cols,
            name = "Elevation (m)",
            limits = c(0, max(1, max_lim)),
            breaks = elev_breaks,
            labels = elev_lab_fn,
            guide = cb_guide,
            na.value = "transparent")
        }
      }
    } else {
      # Natural hypsometric base for physical terrain
      p <- p + ggplot2::geom_raster(
        data = elev_df,
        ggplot2::aes(x = .data$x, y = .data$y, fill = .data$value),
        alpha = 0.95, show.legend = FALSE,
        key_glyph = ggplot2::draw_key_blank) +
        ggplot2::scale_fill_gradientn(
          colours = ma$terrain_cols,
          na.value = "transparent")
    }
  }

  # Reset fill scale if elevation was used so points/grids can use discrete fill
  if (!is.null(elev_df)) {
    p <- p + ggnewscale::new_scale_fill()
  }

  # Base ocean polygon (drawn if no raster or land-only raster or terrain)
  if (!is.null(layers$ocean) && (is.null(elev_df) || scope_val == "land" || !elev_active)) {
    p <- p + ggplot2::geom_sf(data = layers$ocean, fill = ma$water_fill, colour = NA, show.legend = FALSE)
  }

  # Base land polygon (only when no elevation raster is drawn)
  if (isTRUE(show_land) && !is.null(layers$land) && is.null(elev_df)) {
    land_fill <- ma$land_fill
    land_alpha <- if (is.null(terrain_df)) 1 else 0.65
    land_border_col <- if (is.null(admin_level)) NA else ma$land_border
    p <- p + ggplot2::geom_sf(data = layers$land, fill = land_fill,
                              colour = land_border_col, linewidth = ma$land_lwd,
                              alpha = land_alpha, show.legend = FALSE)
  }

  # Terrain hillshade overlay (3D relief texture with highlights and shadows)
  if (!is.null(terrain_df)) {
    p <- p + ggplot2::geom_raster(
      data = terrain_df,
      ggplot2::aes(x = .data$x, y = .data$y, alpha = 1 - .data$layer),
      fill = "black", show.legend = FALSE,
      key_glyph = ggplot2::draw_key_blank) +
      ggplot2::scale_alpha_continuous(range = c(0, ma$terrain_alpha), guide = "none")
  }
  # Country borders on top of terrain/elevation relief
  if ((isTRUE(show_countries) || (!is.null(admin_level) && admin_level >= 0)) && !is.null(layers$land) && (!is.null(terrain_df) || !is.null(elev_df))) {
    p <- p + ggplot2::geom_sf(data = layers$land, fill = NA,
                              colour = ma$land_border, linewidth = ma$land_lwd, show.legend = FALSE)
  }
  if (!is.null(eco)) {
    p <- p + ggplot2::geom_sf(data = eco, fill = NA,
                              colour = ma$ecoregion_colour,
                              linewidth = ma$ecoregion_lwd, show.legend = FALSE)
  }
  if (!is.null(pa) && nrow(pa) > 0L) {
    pa_fill <- if (!is.null(ma$protected_areas_fill)) ma$protected_areas_fill else "#2e7d32"
    pa_border <- if (!is.null(ma$protected_areas_border)) ma$protected_areas_border else "#1b5e20"
    pa_alpha <- if (!is.null(ma$protected_areas_alpha)) as.numeric(ma$protected_areas_alpha) else 0.25
    pa_lwd <- if (!is.null(ma$protected_areas_lwd)) as.numeric(ma$protected_areas_lwd) else 0.35
    pa_linetype <- if (!is.null(ma$protected_areas_linetype)) ma$protected_areas_linetype else "solid"
    p <- p + ggplot2::geom_sf(
      data = pa,
      fill = pa_fill,
      colour = pa_border,
      alpha = pa_alpha,
      linewidth = pa_lwd,
      linetype = pa_linetype,
      show.legend = FALSE
    )
  }
  if (!is.null(layers$lakes)) {
    p <- p + ggplot2::geom_sf(data = layers$lakes, fill = ma$water_fill, colour = NA, show.legend = FALSE)
  }
  if (!is.null(layers$rivers)) {
    p <- p + ggplot2::geom_sf(data = layers$rivers, colour = ma$water_line,
                              linewidth = ma$water_lwd, show.legend = FALSE)
  }
  if (!is.null(layers$admin1)) {
    p <- p + ggplot2::geom_sf(data = layers$admin1, fill = NA,
                              colour = ma$admin1_colour, linewidth = ma$admin1_lwd, show.legend = FALSE)
  }
  if (!is.null(layers$roads) && nrow(layers$roads) > 0L) {
    p <- p + ggplot2::geom_sf(data = layers$roads, colour = ma$road_colour,
                              linewidth = ma$road_lwd, show.legend = FALSE)
  }
  if (!is.null(layers$cities)) {
    p <- p + ggplot2::geom_sf(data = layers$cities, colour = ma$city_colour,
                              size = ma$city_size, shape = 20, show.legend = FALSE)
    if (isTRUE(city_labels) && "city_name" %in% names(layers$cities)) {
      lbl_fill <- if (!is.null(ma$city_label_fill)) ma$city_label_fill else "#ffffffc8"
      lbl_col <- if (!is.null(ma$city_label_colour)) ma$city_label_colour else "#1f2937"
      lbl_size <- if (!is.null(ma$city_label_size)) ma$city_label_size else 2.5
      lbl_args <- list(
        data = layers$cities,
        mapping = ggplot2::aes(label = .data$city_name),
        colour = lbl_col,
        size = lbl_size,
        fill = lbl_fill,
        label.padding = grid::unit(0.18, "lines"),
        label.r = grid::unit(0.25, "lines"),
        vjust = -0.7,
        show.legend = FALSE
      )
      if (utils::packageVersion("ggplot2") >= "3.5.0") {
        lbl_args$linewidth <- 0.15
      } else {
        lbl_args$label.size <- 0.15
      }
      p <- p + do.call(ggplot2::geom_sf_label, lbl_args)
    }
  }

  # IUCN calculations & EOO convex hull layer
  iucn_metrics <- NULL
  iucn_report <- NULL
  if (isTRUE(iucn) || isTRUE(show_eoo)) {
    # Determine habitat boundary constraint (land vs water vs all)
    eff_iucn_med <- if (!is.null(iucn_medium) && iucn_medium != "auto") {
      iucn_medium
    } else if (keep_medium %in% c("land", "water")) {
      keep_medium
    } else {
      # Auto-detect whether occurrences predominantly fall on land or water
      s2_was <- suppressMessages(suppressWarnings(sf::sf_use_s2(FALSE)))
      land_u <- tryCatch(suppressWarnings(sf::st_union(sf::st_geometry(sf::st_make_valid(layers$land)))), error = function(e) NULL)
      suppressMessages(suppressWarnings(sf::sf_use_s2(s2_was)))
      if (!is.null(land_u)) {
        pts_on_land <- sum(lengths(sf::st_intersects(pts, land_u)) > 0L)
        if (pts_on_land >= nrow(pts) * 0.5) "land" else "water"
      } else {
        "all"
      }
    }

    constrain_layer <- NULL
    if (identical(eff_iucn_med, "land")) {
      constrain_layer <- layers$land
    } else if (identical(eff_iucn_med, "water")) {
      constrain_layer <- layers$ocean
      if (is.null(constrain_layer)) {
        constrain_layer <- safe_ne_fetch(scale, "ocean", "physical", crs_map, bbox_sf)
      }
    }
    eff_iucn_taxa <- if (!is.null(iucn_taxa)) iucn_taxa else iucn_species
    groups <- levs
    if (!is.null(eff_iucn_taxa)) {
      user_matches <- levs[tolower(levs) %in% tolower(eff_iucn_taxa)]
      if (length(user_matches) == 0L) {
        warning("None of the specified iucn_taxa were found in data column '", color_by, "'. Evaluating all taxa instead.")
      } else {
        groups <- user_matches
      }
    }
    iucn_metrics_list <- list()
    iucn_poly_list <- list()
    iucn_reports <- character(length(groups))
    
    for (i in seq_along(groups)) {
      grp <- groups[i]
      grp_pts <- pts[dat[[color_by]] == grp, , drop = FALSE]
      metrics <- calculate_iucn_metrics(grp_pts, constrain_sf = constrain_layer)
      
      if (length(groups) > 1) {
        sub_report <- sub("^IUCN Red List Range Evaluation \\(Criterion B\\):\n", "", metrics$report)
        if (sub_report == metrics$report) {
          metrics$report <- paste0("  [", grp, "] ", sub_report)
        } else {
          metrics$report <- paste0("  [", grp, "]\n", sub_report)
        }
      }
      
      iucn_metrics_list[[grp]] <- metrics
      iucn_reports[i] <- metrics$report
      
      if (!is.null(metrics$polygon)) {
        poly_sf <- sf::st_sf(geometry = sf::st_geometry(metrics$polygon))
        poly_sf[[color_by]] <- grp
        iucn_poly_list[[grp]] <- poly_sf
      }
    }
    
    if (length(groups) > 1) {
      iucn_report <- paste0("IUCN Red List Range Evaluation (Criterion B):\n", paste(iucn_reports, collapse = "\n"))
    } else {
      iucn_report <- iucn_reports[1]
    }
    
    if (verbose) msg(iucn_report)
    
    any_constrained <- any(vapply(iucn_metrics_list, function(x) isTRUE(x$is_constrained), logical(1)))
    iucn_metrics <- list(
      report = iucn_report,
      is_constrained = any_constrained,
      by_group = iucn_metrics_list,
      eoo_km2 = if (length(iucn_metrics_list) > 0) iucn_metrics_list[[1]]$eoo_km2 else NULL,
      aoo_km2 = if (length(iucn_metrics_list) > 0) iucn_metrics_list[[1]]$aoo_km2 else NULL
    )
    
    if (length(iucn_poly_list) > 0) {
      for (grp in names(iucn_poly_list)) {
        grp_poly <- iucn_poly_list[[grp]]
        poly_col <- if (!is.null(pal_vals[grp]) && !is.na(pal_vals[grp])) pal_vals[grp] else "#FF7F00"
        p <- p + ggplot2::geom_sf(
          data = grp_poly,
          fill = poly_col,
          colour = poly_col,
          alpha = 0.25,
          linewidth = 0.8,
          linetype = "dashed",
          show.legend = FALSE
        )
      }
    }
  }

  # Determine point aesthetics: custom parameters override defaults
  final_point_size <- if (!is.null(point_size)) point_size else ma$point_size
  final_point_alpha <- if (!is.null(point_alpha)) point_alpha else ma$point_alpha
  final_point_shape <- if (!is.null(point_shape)) point_shape else fixed_shape
  final_point_border_color <- if (!is.null(point_border_color)) point_border_color else ma$point_border_color
  final_point_border_linewidth <- if (!is.null(point_border_linewidth)) point_border_linewidth else ma$point_border_linewidth
  final_point_border_linetype <- if (!is.null(point_border_linetype)) point_border_linetype else ma$point_border_linetype

  # Determine which taxa/species points to display
  eff_show_taxa <- if (!is.null(show_taxa)) {
    show_taxa
  } else if (!is.null(show_species)) {
    show_species
  } else if (!is.null(iucn_points_taxa)) {
    iucn_points_taxa
  } else if ((isTRUE(iucn) || isTRUE(show_eoo)) && !is.null(eff_iucn_taxa)) {
    eff_iucn_taxa
  } else {
    NULL
  }

  pts_to_plot <- pts
  plot_levs <- levs
  if (!is.null(eff_show_taxa) && !is.null(color_by) && color_by %in% names(pts)) {
    user_taxa_match <- levs[tolower(levs) %in% tolower(eff_show_taxa)]
    if (length(user_taxa_match) > 0L) {
      pts_to_plot <- pts[pts[[color_by]] %in% user_taxa_match, , drop = FALSE]
      plot_levs <- user_taxa_match
    } else {
      warning("None of the specified show_taxa found in '", color_by, "'. Showing all points.")
    }
  }

  # Build points layer
  p_pts_layer <- NULL
  pts_geom_args <- list(
    data = pts_to_plot, mapping = mapping, size = final_point_size,
    alpha = final_point_alpha, colour = final_point_border_color,
    stroke = final_point_border_linewidth, linetype = final_point_border_linetype,
    key_glyph = ggplot2::draw_key_point
  )
  if (!is.null(final_point_shape)) {
    pts_geom_args$shape <- final_point_shape
  }
  p_pts_layer <- list(
    do.call(ggplot2::geom_sf, pts_geom_args)
  )

  # Highlight type specimens (Holotypes, Syntypes, Lectotypes, etc.)
  if (isTRUE(highlight_type)) {
    type_cand <- if (!is.null(type_status_col)) type_status_col else
      intersect(c("typeStatus", "type_status", "status", "type", "is_type"), names(pts_to_plot))[1]
    if (!is.na(type_cand) && type_cand %in% names(pts_to_plot)) {
      col_raw <- pts_to_plot[[type_cand]]
      type_idx <- if (is.logical(col_raw)) {
        col_raw %in% TRUE
      } else {
        grepl("holotype|syntype|lectotype|neotype|paratype|type", tolower(as.character(col_raw)))
      }
      if (any(type_idx, na.rm = TRUE)) {
        type_pts <- pts_to_plot[which(type_idx), , drop = FALSE]
        type_fills <- if (!is.null(type_fill)) {
          if (identical(type_fill, "species") || identical(type_fill, "same") || identical(type_fill, "auto")) {
            if (!is.null(color_by) && color_by %in% names(type_pts)) {
              unname(pal_vals[as.character(type_pts[[color_by]])])
            } else {
              "#FFD700"
            }
          } else if (identical(type_fill, "gold")) {
            "#FFD700"
          } else {
            type_fill
          }
        } else {
          "#FFD700"
        }
        type_fills[is.na(type_fills)] <- "#FFD700"
        final_type_size <- if (!is.null(type_size)) type_size else final_point_size * 1.6
        final_type_shape <- if (!is.null(type_shape)) type_shape else 23
        p_pts_layer <- c(p_pts_layer, list(
          ggplot2::geom_sf(data = type_pts, shape = final_type_shape, size = final_type_size,
                           fill = type_fills, colour = "black", stroke = 1.1, show.legend = FALSE)
        ))
      }
    }
  }
  fill_override <- list(
    fill = unname(pal_vals[plot_levs]),
    colour = final_point_border_color,
    stroke = final_point_border_linewidth,
    alpha = final_point_alpha
  )
  if (!is.null(shape_col) && identical(shape_col, color_by)) {
    fill_override$shape <- unname(shp_vals[plot_levs])
  }

  # Italicize scientific names in legend labels as matching expression vector
  italic_labels <- parse(text = paste0("italic('", plot_levs, "')"))
  names(italic_labels) <- plot_levs

  p_pts_layer <- c(p_pts_layer, list(
    ggplot2::scale_fill_manual(
      values = pal_vals,
      breaks = plot_levs,
      name = color_by,
      labels = italic_labels,
      guide = if (isFALSE(show_species_legend)) "none" else ggplot2::guide_legend(override.aes = fill_override)
    )
  ))
  if (!is.null(shape_col)) {
    plot_shp_levs <- if (identical(shape_col, color_by)) plot_levs else shp_levs
    p_pts_layer <- c(p_pts_layer, list(
      ggplot2::scale_shape_manual(
        values = shp_vals,
        breaks = plot_shp_levs,
        name = if (auto_shape) color_by else shape_by,
        labels = if (auto_shape) italic_labels[plot_shp_levs] else ggplot2::waiver(),
        guide = if (isFALSE(show_species_legend)) "none" else if (identical(shape_col, color_by)) "legend" else
          ggplot2::guide_legend(override.aes = list(
            shape = unname(shp_vals[plot_shp_levs]),
            colour = final_point_border_color,
            stroke = final_point_border_linewidth,
            alpha = final_point_alpha
          ))
      )
    ))
  }

  # Grid aesthetics
  final_grid_color <- if (!is.null(grid_color)) grid_color else ma$grid_color
  final_grid_linewidth <- if (!is.null(grid_linewidth)) grid_linewidth else ma$grid_linewidth
  final_grid_linetype <- if (!is.null(grid_linetype)) grid_linetype else ma$grid_linetype
  final_grid_palette <- if (!is.null(grid_palette)) grid_palette else ma$grid_palette

  # Pre-calculate inset if requested
  ins <- NULL
  if (inset_map) {
    if (requireNamespace("patchwork", quietly = TRUE)) {
      ins <- make_inset(sf::st_transform(bbox_sf, 4326))
    } else {
      message("patchwork not installed, inset skipped")
    }
  }

  # Helper to assemble a complete ggplot object
  p_base <- p
  last_bins_res <- NULL
  build_final_plot <- function(count_vec, m_title, include_points = TRUE) {
    p_curr <- p_base
    if (!is.null(grid_sf) && !is.null(count_vec)) {
      p_curr <- p_curr + ggnewscale::new_scale_fill()
      if (identical(grid_steps, "continuous") || isFALSE(grid_steps)) {
        g_data <- grid_sf
        g_data$draw_cnt <- count_vec
        p_curr <- p_curr + ggplot2::geom_sf(
          data = g_data,
          ggplot2::aes(fill = .data$draw_cnt),
          colour = final_grid_color,
          linewidth = final_grid_linewidth,
          linetype = final_grid_linetype,
          alpha = ma$grid_alpha
        ) +
        ggplot2::scale_fill_gradientn(
          colours = final_grid_palette,
          name = m_title
        )
      } else {
        eff_step_size <- if (!is.null(grid_category_width)) grid_category_width else grid_step_size
        bins_res <- build_grid_bins(
          count_vec,
          user_steps = grid_steps,
          n_categories = grid_categories,
          step_size = eff_step_size,
          breaks = grid_breaks,
          method = grid_category_method,
          labels = grid_category_labels
        )
        last_bins_res <<- bins_res
        g_data <- grid_sf
        g_data$draw_bin <- bins_res$factor
        n_b <- length(bins_res$labels)
        bin_cols <- grDevices::colorRampPalette(final_grid_palette)(n_b)
        names(bin_cols) <- bins_res$labels

        p_curr <- p_curr + ggplot2::geom_sf(
          data = g_data,
          ggplot2::aes(fill = .data$draw_bin),
          colour = final_grid_color,
          linewidth = final_grid_linewidth,
          linetype = final_grid_linetype,
          alpha = ma$grid_alpha,
          show.legend = TRUE
        ) +
        ggplot2::scale_fill_manual(
          values = bin_cols,
          breaks = bins_res$labels,
          name = m_title,
          drop = FALSE,
          guide = ggplot2::guide_legend(
            override.aes = list(
              fill = unname(bin_cols),
              colour = final_grid_color,
              alpha = ma$grid_alpha,
              linewidth = 0.3
            )
          )
        )
      }
    }

    if (!is.null(p_pts_layer) && isTRUE(include_points)) {
      for (l in p_pts_layer) p_curr <- p_curr + l
    }

    # Graticule lines as top layer
    if (graticule && graticule_lines && !is.null(grat)) {
      p_curr <- p_curr + ggplot2::geom_sf(data = grat, colour = ma$graticule_colour,
                                          linewidth = ma$graticule_lwd)
    }

    p_curr <- p_curr + ggplot2::coord_sf(crs = crs_map, xlim = c(bb[["xmin"]], bb[["xmax"]]),
                                         ylim = c(bb[["ymin"]], bb[["ymax"]]), expand = FALSE)
    eff_bg_fill <- if (isTRUE(show_ocean) || isTRUE(show_water) || !is.null(layers$ocean)) ma$water_fill else "white"
    p_curr <- p_curr + map_theme(legend_position = legend_position, show_graticule = graticule, bg_fill = eff_bg_fill)

    eff_na_scale <- if (!is.null(north_arrow_scale)) {
      as.numeric(north_arrow_scale)
    } else if (!is.null(ma$north_arrow_scale) && !identical(as.numeric(ma$north_arrow_scale), 1.0)) {
      as.numeric(ma$north_arrow_scale)
    } else if (isTRUE(facet) || !is.null(facet_by)) {
      0.75
    } else {
      1.0
    }
    p_curr <- add_furniture(p_curr, ma, scalebar, north_arrow,
                            scalebar_pos = scalebar_pos, north_arrow_pos = north_arrow_pos,
                            north_arrow_scale = eff_na_scale)

    if (inset_map && !is.null(ins)) {
      ins_bounds <- resolve_inset_bounds(
        inset_pos = inset_pos,
        pts = pts,
        bbox_sf = bbox_sf,
        scalebar_pos = scalebar_pos,
        north_arrow_pos = north_arrow_pos
      )
      p_curr <- p_curr + patchwork::inset_element(
        ins,
        left = ins_bounds$left,
        bottom = ins_bounds$bottom,
        right = ins_bounds$right,
        top = ins_bounds$top
      )
    }

    # Faceting for multi-species plates
    if (isTRUE(facet) || !is.null(facet_by)) {
      f_var <- if (!is.null(facet_by)) facet_by else color_by
      if (!is.null(f_var) && f_var %in% names(dat)) {
        p_curr <- p_curr + ggplot2::facet_wrap(
          stats::as.formula(paste0("~", f_var)),
          nrow = eff_facet_rows,
          ncol = eff_facet_cols
        )
        if (isTRUE(facet_italic)) {
          p_curr <- p_curr + ggplot2::theme(
            strip.text = ggplot2::element_text(
              face = "italic",
              size = 10,
              colour = "#2c3e50",
              margin = ggplot2::margin(b = 4, t = 4)
            ),
            strip.background = ggplot2::element_rect(
              fill = "#f8f9fa",
              colour = "grey75",
              linewidth = 0.4
            )
          )
        }
      }
    }

    p_curr
  }

  p_richness <- NULL
  p_records <- NULL

  if (is.null(grid_sf)) {
    p <- build_final_plot(NULL, NULL, include_points = isTRUE(show_points))
  } else if (actual_metric == "richness") {
    p_richness <- build_final_plot(grid_sf$count_richness, "Species per cell", include_points = isTRUE(show_points))
    p <- p_richness
  } else if (actual_metric == "records") {
    p_records <- build_final_plot(grid_sf$count_records, "Records per cell", include_points = isTRUE(show_points))
    p <- p_records
  } else { # "both"
    p_richness <- build_final_plot(grid_sf$count_richness, "Species per cell", include_points = isTRUE(show_points))
    p_records <- build_final_plot(grid_sf$count_records, "Records per cell", include_points = isTRUE(show_points))
    p <- p_richness
  }

  ## ---- output ----
  if (is.null(output_dir)) {
    output_dir <- file.path(".", paste0("OneClickCartographics_", format(Sys.time(), "%Y%m%d_%H%M%S")))
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  static_formats <- setdiff(formats, "html")
  files <- character(0)

  if (length(static_formats) > 0) {
    files <- c(files, save_map_files(p, output_dir, filename, static_formats, dpi, width, height))
    if (!is.null(grid_sf)) {
      if (actual_metric == "both") {
        f_rich <- save_map_files(p_richness, output_dir, paste0(filename, "_richness"), static_formats, dpi, width, height)
        f_rec <- save_map_files(p_records, output_dir, paste0(filename, "_records"), static_formats, dpi, width, height)
        files <- c(files, f_rich, f_rec)
      } else if (actual_metric == "richness" && !is.null(p_richness) && !identical(p, p_richness)) {
        f_rich <- save_map_files(p_richness, output_dir, paste0(filename, "_richness"), static_formats, dpi, width, height)
        files <- c(files, f_rich)
      } else if (actual_metric == "records" && !is.null(p_records) && !identical(p, p_records)) {
        f_rec <- save_map_files(p_records, output_dir, paste0(filename, "_records"), static_formats, dpi, width, height)
        files <- c(files, f_rec)
      }
    }
  }

  # Single HTML exports removed in favor of single unified report (00_map_report.html)

  ## ---- GIS export ----
  if (!is.null(gis_formats)) {
    gis_files <- save_gis_files(pts, grid_sf, output_dir, filename, gis_formats)
    files <- c(files, gis_files)
  }

  ## ---- grid counts & summary CSVs ----
  if (!is.null(grid_richness_summary_df)) {
    grid_richness_file <- file.path(output_dir, "grid_richness_summary.csv")
    write.csv(grid_richness_summary_df, grid_richness_file, row.names = FALSE)
    files <- c(files, grid_richness_file)
    msg("Grid species richness summary saved:", grid_richness_file)
  }

  if (!is.null(grid_density_summary_df)) {
    grid_density_file <- file.path(output_dir, "grid_record_density_summary.csv")
    write.csv(grid_density_summary_df, grid_density_file, row.names = FALSE)
    files <- c(files, grid_density_file)
    msg("Grid record density summary saved:", grid_density_file)
  }

  if (!is.null(grid_counts_df)) {
    # Export cell-by-cell coordinates table for GIS & spatial analysis
    grid_cells_file <- file.path(output_dir, "grid_cell_counts.csv")
    write.csv(grid_counts_df, grid_cells_file, row.names = FALSE)
    files <- c(files, grid_cells_file)
    msg("Grid cell-by-cell counts saved:", grid_cells_file)
  }

  if (!is.null(grid_sf) && !is.null(grid_counts_df)) {
    grid_cache_file <- file.path(output_dir, ".grid_cache.rds")
    tryCatch({
      saveRDS(list(points = pts, grid_sf = grid_sf, grid_counts_df = grid_counts_df,
                   grid_richness_summary_df = grid_richness_summary_df,
                   grid_density_summary_df = grid_density_summary_df,
                   grid_summary_df = grid_richness_summary_df,
                   metric_label = metric_label,
                   grid_km = grid_km,
                   point_color = pal_vals, palette = pal_vals,
                   color_by = color_by), grid_cache_file)
    }, error = function(e) NULL)
  }

  if (!is.null(iucn_metrics)) {
    iucn_cache_file <- file.path(output_dir, ".iucn_cache.rds")
    tryCatch({
      saveRDS(list(report = iucn_metrics$report, is_constrained = iucn_metrics$is_constrained,
                   medium = iucn_medium), iucn_cache_file)
    }, error = function(e) NULL)
  }

  if (!is.null(clean_obj)) {
    cleaning_cache_file <- file.path(output_dir, ".cleaning_cache.rds")
    tryCatch({
      saveRDS(list(clean_obj = clean_obj, clean_report = clean_report), cleaning_cache_file)
    }, error = function(e) NULL)
  }

  if (!is.null(pa) && !is.null(pa_counts_df)) {
    pa_counts_file <- file.path(output_dir, "protected_areas_counts.csv")
    write.csv(pa_counts_df, pa_counts_file, row.names = FALSE)
    files <- c(files, pa_counts_file)
    msg("Protected areas counts saved:", pa_counts_file)

    if (!is.null(pa_summary_df)) {
      pa_summary_file <- file.path(output_dir, "protected_areas_summary.csv")
      write.csv(pa_summary_df, pa_summary_file, row.names = FALSE)
      files <- c(files, pa_summary_file)
      msg("Protected areas summary saved:", pa_summary_file)
    }

    pa_cache_file <- file.path(output_dir, ".protected_areas_cache.rds")
    tryCatch({
      saveRDS(list(protected_areas = pa,
                   counts_df = pa_counts_df,
                   summary_df = pa_summary_df,
                   params = list(
                     protected_areas_fill = pa_fill,
                     protected_areas_border = pa_border,
                     protected_areas_alpha = pa_alpha,
                     protected_areas_lwd = pa_lwd,
                     protected_areas_linetype = pa_linetype
                   )), pa_cache_file)
    }, error = function(e) NULL)
  }

  # Save or update feature cache for consolidated reporting in output_dir
  feat_cache_file <- file.path(output_dir, ".features_cache.rds")
  prev_feats <- if (file.exists(feat_cache_file)) tryCatch(readRDS(feat_cache_file), error = function(e) list()) else list()
  al_prev <- if (!is.null(prev_feats$admin_level)) as.integer(prev_feats$admin_level) else 0L
  al_curr <- if (!is.null(admin_level)) as.integer(admin_level) else 0L
  eff_admin <- max(al_prev, al_curr)

  curr_feats <- list(
    admin_level = if (eff_admin > 0L) eff_admin else prev_feats$admin_level,
    clean_coords = isTRUE(clean_coords) || isTRUE(prev_feats$clean_coords),
    show_roads = isTRUE(show_roads) || isTRUE(prev_feats$show_roads),
    show_cities = isTRUE(show_cities) || isTRUE(prev_feats$show_cities),
    city_labels = isTRUE(city_labels) || isTRUE(prev_feats$city_labels),
    show_water = isTRUE(show_water) || isTRUE(prev_feats$show_water),
    show_ecoregions = isTRUE(show_ecoregions) || isTRUE(prev_feats$show_ecoregions),
    show_protected_areas = isTRUE(show_protected_areas) || !is.null(protected_areas) || isTRUE(prev_feats$show_protected_areas),
    show_elevation = isTRUE(show_elevation) || isTRUE(prev_feats$show_elevation),
    show_terrain = isTRUE(show_terrain) || isTRUE(prev_feats$show_terrain),
    inset_map = isTRUE(inset_map) || isTRUE(prev_feats$inset_map),
    facet = isTRUE(facet) || isTRUE(prev_feats$facet),
    facet_by = if (!is.null(facet_by)) facet_by else prev_feats$facet_by,
    highlight_type = isTRUE(highlight_type) || isTRUE(prev_feats$highlight_type),
    journal = if (!is.null(journal)) journal else prev_feats$journal,
    journal_cols = if (!is.null(journal_cols)) journal_cols else prev_feats$journal_cols,
    gis_formats = unique(c(prev_feats$gis_formats, gis_formats))
  )
  tryCatch(saveRDS(curr_feats, feat_cache_file), error = function(e) NULL)

  report_file <- file.path(output_dir, "00_map_report.html")
  furniture_txt <- paste0("km scale bar", ", north arrow, graticule")
  info <- list(
    timestamp = timestamp, r_version = R.version.string,
    package_version = as.character(utils::packageVersion("OneClickCartographics")),
    seed = seed,
    data_path = data_label, n_records = n_records, color_by = color_by,
    shape_by = shape_by,
    crs_label = crs_label, epsg_label = if (is.null(epsg)) "custom (no EPSG)" else paste0("EPSG:", epsg),
    tier = tier, span_km = span_km, ne_scale = scale, scale_auto = scale_auto,
    dateline = if (dateline) "yes (longitudes wrapped to 0-360)" else "no",
    rendering = rendering, grid_km = grid_km,
    grid_bbox = if (is.null(grid_bbox)) "full map extent" else
      paste0("c(", paste(grid_bbox, collapse = ", "), ")"),
    grid_csv = grid_csv || (actual_metric == "both"),
    grid_metric = if (!is.null(grid_km)) actual_metric else NULL,
    grid_metric_label = if (!is.null(grid_km)) {
      if (actual_metric == "both") "species richness and records (dual maps)" else metric_label
    } else NULL,
    grid_contour = if (!is.null(grid_km))
      paste0("color ", final_grid_color, ", linewidth ", final_grid_linewidth, ", linetype ", final_grid_linetype) else NULL,
    grid_bins = if (!is.null(last_bins_res)) paste(last_bins_res$labels, collapse = ", ") else NULL,
    admin_level = admin_level, keep_medium = keep_medium,
    show_water = show_water,
    show_roads = show_roads,
    show_cities = show_cities,
    city_labels = city_labels,
    show_elevation = show_elevation,
    show_terrain = show_terrain,
    clean_coords = clean_coords,
    inset_map = inset_map,
    inset_pos = inset_pos,
    inset_pos_resolved = if (inset_map) {
      resolve_inset_bounds(inset_pos, pts, bbox_sf, scalebar_pos, north_arrow_pos)$pos_code
    } else NULL,
    layers_used = layers_used,
    terrain = terrain_df, terrain_zoom = terrain_zoom,
    elevation = elev_df, topo = if (scope_val == "both") elev_df else NULL,
    elevation_scope = scope_val, water_depth = scope_val %in% c("both", "sea"),
    ecoregions = eco,
    show_protected_areas = isTRUE(show_protected_areas) || !is.null(protected_areas),
    protected_areas = pa,
    protected_areas_counts_df = pa_counts_df,
    protected_areas_summary_df = pa_summary_df,
    protected_areas_fill = if (!is.null(pa)) pa_fill else NULL,
    protected_areas_border = if (!is.null(pa)) pa_border else NULL,
    protected_areas_alpha = if (!is.null(pa)) pa_alpha else NULL,
    protected_areas_lwd = if (!is.null(pa)) pa_lwd else NULL,
    protected_areas_linetype = if (!is.null(pa)) pa_linetype else NULL,
    highlight_type = highlight_type,
    iucn = iucn,
    iucn_medium = iucn_medium,
    journal = journal,
    journal_cols = journal_cols,
    facet = facet,
    facet_by = facet_by,
    show_species_legend = show_species_legend,
    width = width,
    height = height,
    dpi = dpi,
    formats = formats,
    furniture = furniture_txt,
    gis_formats = gis_formats,
    clean_report = clean_report,
    cleaning = clean_obj,
    medium_dropped = medium_dropped,
    grid_richness_summary_df = grid_richness_summary_df,
    grid_density_summary_df = grid_density_summary_df,
    grid_record_density_summary_df = grid_density_summary_df,
    grid_summary_df = grid_richness_summary_df,
    grid_counts_df = grid_counts_df,
    iucn_report = if (!is.null(iucn_metrics)) iucn_metrics$report else NULL,
    iucn_constrained = if (!is.null(iucn_metrics)) isTRUE(iucn_metrics$is_constrained) else FALSE,
    interactive_res = list(
      points = pts,
      grid_sf = grid_sf,
      grid_counts_df = grid_counts_df,
      protected_areas = pa,
      protected_areas_counts_df = pa_counts_df,
      protected_areas_summary_df = pa_summary_df,
      params = list(grid_metric_label = metric_label,
                    point_color = pal_vals, palette = pal_vals,
                    color_by = color_by,
                    protected_areas_fill = if (!is.null(pa)) pa_fill else NULL,
                    protected_areas_border = if (!is.null(pa)) pa_border else NULL,
                    protected_areas_alpha = if (!is.null(pa)) pa_alpha else NULL,
                    protected_areas_lwd = if (!is.null(pa)) pa_lwd else NULL,
                    protected_areas_linetype = if (!is.null(pa)) pa_linetype else NULL)
    ),
    extent_proj = bb, extent_ll = extent_ll)
  generate_html_report(info, report_file)
  files <- c(files, report_file)

  msg("Saved:")
  for (f in files) msg("  ", f)
  msg("Report file:", report_file)

  # Render map in the interactive plot pane (RStudio/Positron/IDE)
  if (interactive() || dev.cur() > 1 || isTRUE(getOption("OneClickCartographics.plot", FALSE))) {
    tryCatch(print(p), error = function(e) invisible(NULL))
  }

  invisible(list(p = p,
                 p_richness = p_richness,
                 p_records = p_records,
                 points = pts,
                 grid_sf = grid_sf,
                 grid_bins = last_bins_res,
                 iucn = iucn_metrics,
                 cleaning = clean_report,
                 layers = layers,
                 protected_areas = pa,
                 protected_areas_counts_df = pa_counts_df,
                 protected_areas_summary_df = pa_summary_df,
                 crs = crs_label,
                 epsg = epsg,
                 extent = extent_ll,
                 tier = tier,
                 files = files,
                 methods_file = report_file,
                 report_file = report_file,
                 info = info,
                 n_records = n_records,
                 grid_counts_df = grid_counts_df,
                 grid_richness_summary = grid_richness_summary_df,
                 grid_density_summary = grid_density_summary_df,
                 grid_record_density_summary = grid_density_summary_df,
                 grid_summary = list(richness = grid_richness_summary_df,
                                     record_density = grid_density_summary_df),
                 grid_summary_df = grid_richness_summary_df,
                 dat = dat,
                 params = list(
                   data_path = data_label, output_dir = output_dir,
                   color_by = color_by, shape_by = shape_by,
                   palette = pal_vals, point_color = pal_vals,
                   map_fill = ma$land_fill, land_fill = ma$land_fill,
                   admin_level = admin_level, show_water = show_water,
                   show_terrain = show_terrain, show_elevation = show_elevation,
                   elevation_scope = scope_val, water_depth = scope_val %in% c("both", "sea"),
                   elevation_limits = elevation_limits,
                   show_protected_areas = show_protected_areas,
                   protected_areas_fill = if (!is.null(pa)) pa_fill else NULL,
                   protected_areas_border = if (!is.null(pa)) pa_border else NULL,
                   protected_areas_alpha = if (!is.null(pa)) pa_alpha else NULL,
                   protected_areas_lwd = if (!is.null(pa)) pa_lwd else NULL,
                   protected_areas_linetype = if (!is.null(pa)) pa_linetype else NULL,
                   grid_km = grid_km, grid_metric = actual_metric,
                   grid_steps = grid_steps,
                   grid_categories = grid_categories,
                   grid_category_width = grid_category_width,
                   grid_step_size = if (!is.null(grid_category_width)) grid_category_width else grid_step_size,
                   grid_breaks = grid_breaks,
                   grid_category_method = grid_category_method,
                   grid_category_labels = grid_category_labels,
                   show_points = show_points,
                   journal = journal, clean_coords = clean_coords,
                   iucn = iucn, show_eoo = show_eoo, iucn_medium = iucn_medium,
                   highlight_type = highlight_type, facet = facet,
                   formats = formats, dpi = dpi, width = width, height = height
                 )))
}

#' Resolve a palette specification to a character vector of colours
#'
#' @param palette Character or `NULL`: palette name, vector of hex colours,
#'   or path to a file with one colour per line.
#' @return Character vector of hex colour codes.
#' @keywords internal
resolve_palette <- function(palette = NULL) {
  # Default: Okabe-Ito
  default <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7", "#E69F00",
               "#56B4E9", "#F0E442", "#000000")

  if (is.null(palette)) return(default)

  # If it's a file path, read colours from file
  if (length(palette) == 1 && file.exists(palette)) {
    cols <- readLines(palette)
    cols <- trimws(cols)
    cols <- cols[nzchar(cols) & grepl("^#", cols)]
    if (length(cols) > 0) return(cols)
    warning("No valid hex colours found in file: ", palette, ". Using default.")
    return(default)
  }

  # If it's a vector of hex colours, use directly
  if (is.character(palette) && all(grepl("^#", palette))) {
    return(palette)
  }

  # If it's a single palette name, try viridis then RColorBrewer
  if (length(palette) == 1) {
    name <- palette

    # Try viridis
    viridis_names <- c("viridis", "magma", "inferno", "plasma", "cividis")
    if (name %in% viridis_names) {
      if (requireNamespace("viridis", quietly = TRUE)) {
        return(viridis::viridis_pal(option = name)(8))
      } else {
        warning("viridis package not installed. Using default palette.")
        return(default)
      }
    }

    # Try RColorBrewer
    if (requireNamespace("RColorBrewer", quietly = TRUE)) {
      info <- RColorBrewer::brewer.pal.info
      if (name %in% rownames(info)) {
        n <- info[name, "maxcolors"]
        return(RColorBrewer::brewer.pal(n, name))
      }
    }

    warning("Unknown palette '", name, "'. Using default.")
    return(default)
  }

  warning("Invalid palette specification. Using default.")
  default
}

#' Save GIS vector files of occurrence points and optional density grid
#'
#' @param pts `sf` object of occurrence points.
#' @param grid_sf `sf` object of density grid, or `NULL`.
#' @param output_dir Directory to save into.
#' @param filename Base file name (no extension).
#' @param formats Character vector: any of `"shp"`, `"gpkg"`, `"geojson"`.
#' @return Character vector of written file paths.
#' @keywords internal
save_gis_files <- function(pts, grid_sf, output_dir, filename, formats) {
  files <- character(0)
  ext_map <- c(shp = ".shp", gpkg = ".gpkg", geojson = ".geojson")

  for (fmt in formats) {
    ext <- ext_map[[fmt]]
    if (is.null(ext)) {
      warning("Unknown GIS format: ", fmt, ". Skipping.")
      next
    }

    # Save points
    pts_file <- file.path(output_dir, paste0(filename, "_points", ext))
    tryCatch({
      sf::st_write(pts, pts_file, delete_dsn = TRUE, quiet = TRUE)
      files <- c(files, pts_file)
    }, error = function(e) {
      message("  - Failed to write ", pts_file, ": ", conditionMessage(e))
    })

    # Save grid if present
    if (!is.null(grid_sf)) {
      grid_file <- file.path(output_dir, paste0(filename, "_grid", ext))
      tryCatch({
        sf::st_write(grid_sf, grid_file, delete_dsn = TRUE, quiet = TRUE)
        files <- c(files, grid_file)
      }, error = function(e) {
        message("  - Failed to write ", grid_file, ": ", conditionMessage(e))
      })
    }
  }

  files
}
