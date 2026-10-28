#' Construct a map from simplest baseline to multi-layered complexity
#'
#' Modular builder function for [run_map()]. Allows constructing biodiversity
#' distribution maps incrementally: starting from the simplest clean baseline
#' (landmass and occurrence points only) and adding elements such as bodies of
#' water, administrative boundaries at user-chosen levels, terrain relief, and
#' density/richness grids.
#'
#' Bodies of water (`water`) and administrative boundaries (`admin`) are
#' strictly separated and independently controlled.
#'
#' @param data Path to CSV/Excel occurrence file, or a `data.frame`.
#' @param water Logical or character vector: include bodies of water.
#'   - `FALSE` (default): clean map with no water bodies.
#'   - `TRUE`: includes all water bodies (ocean, lakes, rivers).
#'   - Character vector specifying components: e.g. `c("ocean", "lakes")`.
#' @param admin Character, integer, or `NULL`: administrative boundaries to add:
#'   - `"none"` or `NULL` (default): clean map without internal boundaries.
#'   - `"country"` or `0`: country borders only.
#'   - `"state"`, `"province"`, `"department"`, or `1`: country borders + states/provinces/departments.
#' @param admin_color,country_color,country_border Character or `NULL`: line colour for country/administrative boundary borders (default `"grey45"`).
#' @param admin_width,country_width,country_lwd Numeric or `NULL`: stroke width for country/administrative boundary borders (default `0.25`).
#' @param state_color,province_color,state_border Character or `NULL`: line colour for state/province/department borders (default `"grey60"`).
#' @param state_width,province_width,state_lwd Numeric or `NULL`: stroke width for state/province/department borders (default `0.15`).
#' @param terrain Logical: add 3D SRTM hillshade terrain relief. Default `FALSE`.
#' @param elevation Character or logical: elevation raster overlay (`"land"`,
#'   `"sea"`, or `"both"`). Default `FALSE`.
#' @param grid Character or `NULL`: add a density/richness grid:
#'   - `NULL` (default): no grid.
#'   - `"richness"`: species richness grid (species count per cell).
#'   - `"records"` or `"points"`: occurrence record density grid.
#'   - `"both"`: generates both species richness and record density maps.
#' @param grid_km Numeric: side length of grid cells in kilometres. Default `50`.
#' @param grid_categories Integer, list, or `NULL`: user-defined categories (e.g. integer `4` or list of ranges).
#' @param grid_category_width,grid_step_size Numeric or `NULL`: observation width across all categories (e.g. `10` creates categories 1–10, 11–20...).
#' @param grid_breaks Numeric vector or `NULL`: break cutoffs defining observations on each category independently (e.g. `c(1, 5, 20, 50)`).
#' @param grid_category_method Character: classification method (`"auto"`, `"equal_interval"`, `"quantile"`).
#' @param grid_category_labels Character vector or `NULL`: custom labels for category legend.
#' @param grid_steps Binning scheme for grid counts (`"auto"`, `"continuous"`, integer `k`, or numeric vector).
#' @param show_points Logical or `NULL`: overlay occurrence points on the map.
#'   When `NULL` (default), points are hidden if `grid` is specified (to prevent
#'   covering cells), and shown if no grid is requested.
#' @param clean_coords Logical: perform automated coordinate cleaning (default `FALSE`).
#' @param iucn Logical: calculate IUCN Red List Criterion B metrics (default `FALSE`).
#' @param show_eoo Logical or `NULL`: overlay IUCN Extent of Occurrence polygon. Defaults to `iucn`.
#' @param iucn_medium Habitat constraint for IUCN EOO polygon ("auto", "land", "water", "all"). Default `"auto"`.
#' @param iucn_taxa Character vector or `NULL`: specific species/taxa to evaluate for IUCN.
#' @param highlight_type Highlight type specimens with gold diamond symbols.
#' @param journal Journal preset ("zootaxa", "pensoft", "nature", "plos", etc.).
#' @param journal_cols Number of columns for journal format (1, 1.5, 2).
#' @param show_taxa Specific species/taxa to display occurrence points for.
#' @param facet Split multiple species into multi-panel plates.
#' @param facet_by Column name to facet by.
#' @param facet_cols Number of columns for facet grid.
#' @param facet_rows Number of rows for facet grid.
#' @param facet_layout Preset stacking layout for faceted maps: "row" (1 row) or "col" (1 column).
#' @param facet_italic Format species names in facet panel headers in italics (default `TRUE`).
#' @param cities Include populated places/cities (`TRUE`, `FALSE`, or max number of cities). Default `NULL`.
#' @param city_labels Logical: show text labels for cities (default `TRUE` when cities are shown).
#' @param roads Include roads (`TRUE`, `FALSE`, `1` / `"major"`, `2` / `"all"`). Default `NULL`.
#' @param ecoregions Logical: add WWF terrestrial ecoregion boundaries (default `FALSE`).
#' @param scalebar_pos Position for scale bar ("bl", "br", "tl", "tr"). Default NULL ("bl").
#' @param north_arrow_pos Position for north arrow ("tr", "tl", "bl", "br"). Default NULL ("tr").
#' @param north_arrow_scale Numeric or `NULL`: scaling factor for the north arrow (e.g. 0.75 for 75% size).
#'   Defaults to `0.75` for faceted maps and `1.0` for single-panel maps.
#' @param map_fill,land_fill Fill colour for map / land polygons (e.g. "#E0E0E0", "grey90"). Default NULL uses "#F5F0E6".
#' @param fill_color Optional alias for map_fill.
#' @param inset_map Logical: add a miniature locator overview globe/inset map (default `FALSE`).
#' @param inset_pos Position or coordinates for the inset map ("auto", corner code, or numeric coordinates).
#' @param interactive Launch interactive leaflet viewer (`TRUE`/`FALSE`).
#' @param ... Additional arguments passed to [run_map()].
#'
#' @return The generated map result list (invisibly).
#' @export
#' @examples
#' \dontrun{
#' # Simplest clean map
#' m0 <- construct_map("localities.csv")
#'
#' # Add country boundaries
#' m1 <- construct_map("localities.csv", admin = "country")
#'
#' # Add state boundaries and bodies of water
#' m2 <- construct_map("localities.csv", admin = "state", water = TRUE)
#'
#' # Add species richness grid without points obscuring cells
#' m3 <- construct_map("localities.csv", grid = "richness", grid_km = 40)
#'
#' # Generate both richness and point density grid maps
#' m4 <- construct_map("localities.csv", grid = "both", grid_km = 50)
#'
#' # Build map with terrain and richness
#' m5 <- construct_map("localities.csv", terrain = TRUE, grid = "richness")
#' }
construct_map <- function(data,
                          water = FALSE,
                          admin = "none",
                          admin_color = NULL,
                          admin_width = NULL,
                          country_color = NULL,
                          country_width = NULL,
                          state_color = NULL,
                          state_width = NULL,
                          terrain = FALSE,
                          elevation = FALSE,
                          grid = NULL,
                          grid_km = 50,
                          grid_categories = NULL,
                          grid_category_width = NULL,
                          grid_step_size = NULL,
                          grid_breaks = NULL,
                          grid_category_method = "auto",
                          grid_category_labels = NULL,
                          grid_steps = "auto",
                          show_points = NULL,
                          cities = NULL,
                          city_labels = TRUE,
                          roads = NULL,
                          ecoregions = FALSE,
                          protected_areas = FALSE,
                          protected_areas_fill = NULL,
                          protected_areas_border = NULL,
                          protected_areas_alpha = NULL,
                          protected_areas_lwd = NULL,
                          protected_areas_width = NULL,
                          protected_areas_linetype = NULL,
                          map_fill = NULL,
                          land_fill = NULL,
                          fill_color = NULL,
                          scalebar_pos = NULL,
                          north_arrow_pos = NULL,
                          inset_map = FALSE,
                          inset_pos = NULL,
                          clean_coords = FALSE,
                          iucn = FALSE,
                          show_eoo = NULL,
                          iucn_medium = "auto",
                          iucn_taxa = NULL,
                          show_taxa = NULL,
                          highlight_type = FALSE,
                          journal = NULL,
                          journal_cols = 2,
                          preset = NULL,
                          facet = FALSE,
                          facet_by = NULL,
                          facet_cols = NULL,
                          facet_rows = NULL,
                          facet_layout = NULL,
                          facet_italic = TRUE,
                          north_arrow_scale = NULL,
                          interactive = FALSE,
                          ...) {
  # Parse water
  show_water <- FALSE
  show_ocean <- NULL
  show_lakes <- NULL
  show_rivers <- NULL
  if (isTRUE(water)) {
    show_water <- TRUE
  } else if (is.character(water)) {
    show_water <- TRUE
    w_lower <- tolower(water)
    show_ocean <- "ocean" %in% w_lower
    show_lakes <- "lakes" %in% w_lower
    show_rivers <- "rivers" %in% w_lower
  }

  # Parse admin
  admin_level <- NULL
  if (!is.null(admin)) {
    if (is.numeric(admin)) {
      admin_level <- if (as.integer(admin) >= 1L) 1L else 0L
    } else {
      a_low <- tolower(admin)
      if (a_low %in% c("country", "countries", "0")) {
        admin_level <- 0L
      } else if (a_low %in% c("state", "states", "province", "provinces", "department", "departments", "1")) {
        admin_level <- 1L
      } else if (a_low %in% c("county", "counties", "municipality", "municipalities", "district", "districts", "2")) {
        admin_level <- 1L
      } else if (a_low %in% c("none", "null", "false")) {
        admin_level <- NULL
      }
    }
  }

  # Parse grid
  grid_metric <- "auto"
  use_grid_km <- NULL
  if (!is.null(grid)) {
    use_grid_km <- grid_km
    g_low <- tolower(grid)
    if (g_low %in% c("richness", "species")) {
      grid_metric <- "richness"
    } else if (g_low %in% c("records", "points", "count")) {
      grid_metric <- "records"
    } else if (g_low %in% c("both", "all")) {
      grid_metric <- "both"
    }
  }

  # Forward to run_map
  res <- run_map(
    data_path = data,
    show_water = show_water,
    show_ocean = show_ocean,
    show_lakes = show_lakes,
    show_rivers = show_rivers,
    admin_level = admin_level,
    admin_color = admin_color,
    admin_width = admin_width,
    country_color = country_color,
    country_width = country_width,
    state_color = state_color,
    state_width = state_width,
    show_terrain = terrain,
    show_elevation = elevation,
    show_cities = cities,
    city_labels = city_labels,
    show_roads = roads,
    show_ecoregions = ecoregions,
    show_protected_areas = if (is.logical(protected_areas)) protected_areas else TRUE,
    protected_areas = if (!is.logical(protected_areas)) protected_areas else NULL,
    protected_areas_fill = protected_areas_fill,
    protected_areas_border = protected_areas_border,
    protected_areas_alpha = protected_areas_alpha,
    protected_areas_lwd = protected_areas_lwd,
    protected_areas_width = protected_areas_width,
    protected_areas_linetype = protected_areas_linetype,
    map_fill = map_fill,
    land_fill = land_fill,
    fill_color = fill_color,
    scalebar_pos = scalebar_pos,
    north_arrow_pos = north_arrow_pos,
    inset_map = inset_map,
    inset_pos = inset_pos,
    grid_km = use_grid_km,
    grid_metric = grid_metric,
    grid_categories = grid_categories,
    grid_category_width = grid_category_width,
    grid_step_size = grid_step_size,
    grid_breaks = grid_breaks,
    grid_category_method = grid_category_method,
    grid_category_labels = grid_category_labels,
    grid_steps = grid_steps,
    show_points = show_points,
    clean_coords = clean_coords,
    iucn = iucn,
    show_eoo = show_eoo,
    iucn_medium = iucn_medium,
    iucn_taxa = iucn_taxa,
    show_taxa = show_taxa,
    highlight_type = highlight_type,
    journal = journal,
    journal_cols = journal_cols,
    preset = preset,
    facet = facet,
    facet_by = facet_by,
    facet_cols = facet_cols,
    facet_rows = facet_rows,
    facet_layout = facet_layout,
    facet_italic = facet_italic,
    north_arrow_scale = north_arrow_scale,
    ...
  )

  if (isTRUE(interactive)) {
    interactive_map(res)
  }

  invisible(res)
}
