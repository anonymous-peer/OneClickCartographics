#' Build HTML legend card to display outside interactive map
#' @keywords internal
build_outside_legend_html <- function(interactive_res, metric = "richness") {
  if (is.null(interactive_res)) return("")
  pts <- interactive_res$points
  params <- interactive_res$params
  grid_sf <- interactive_res$grid_sf

  species_items <- character(0)
  if (!is.null(pts) && nrow(pts) > 0L && !isFALSE(params$show_species_legend) && !isFALSE(params$species_legend)) {
    sp_col <- if ("species" %in% names(pts)) "species" else if (!is.null(params$color_by) && params$color_by %in% names(pts)) params$color_by else NULL
    if (!is.null(sp_col)) {
      u_sps <- sort(unique(as.character(pts[[sp_col]])))
      if (length(u_sps) <= 50L) {
        custom_pal <- if (!is.null(params$point_color)) params$point_color else params$palette

      species_items <- vapply(u_sps, function(sp) {
        col <- "#7f7f7f"
        if (!is.null(custom_pal) && !is.null(names(custom_pal)) && sp %in% names(custom_pal)) {
          col <- custom_pal[[sp]]
        } else if (!is.null(custom_pal) && length(custom_pal) >= which(u_sps == sp)) {
          col <- custom_pal[which(u_sps == sp)]
        }
        paste0(
          "<div style='display: flex; align-items: center; gap: 8px; margin-bottom: 7px;'>",
          "<span style='display: inline-block; width: 14px; height: 14px; border-radius: 50%; background-color: ", col, "; border: 1.5px solid #1e293b; flex-shrink: 0; box-shadow: 0 1px 2px rgba(0,0,0,0.15);'></span>",
          "<span style='font-style: italic; font-size: 0.9em; color: #1e293b;'>", htmltools::htmlEscape(sp), "</span>",
          "</div>"
        )
      }, character(1))
      }
    }
  }

  grid_legend_html <- ""
  if (!is.null(grid_sf) && nrow(grid_sf) > 0L) {
    metric_col <- if (metric %in% c("richness", "species")) "count_richness" else "count_records"
    if (!metric_col %in% names(grid_sf)) {
      metric_col <- if ("count" %in% names(grid_sf)) "count" else names(grid_sf)[1]
    }
    vals <- grid_sf[[metric_col]]
    vals_clean <- vals[!is.na(vals)]
    min_v <- if (length(vals_clean) > 0) min(vals_clean) else 0
    max_v <- if (length(vals_clean) > 0) max(vals_clean) else 0
    metric_label <- if (metric %in% c("richness", "species")) "Species Richness (per cell)" else "Occurrence Records (per cell)"

    grid_legend_html <- paste0(
      "<div style='margin-top: 14px; border-top: 1px solid #e2e8f0; padding-top: 12px;'>",
      "<div style='font-weight: 600; color: #334155; margin-bottom: 6px; font-size: 0.92em;'>", metric_label, "</div>",
      "<div style='height: 14px; width: 100%; border-radius: 3px; background: linear-gradient(to right, #FFFFFF, #FEE5D9, #FCAE91, #FB6A4A, #DE2D26, #A50F15); border: 1px solid #cbd5e1;'></div>",
      "<div style='display: flex; justify-content: space-between; font-size: 0.82em; color: #64748b; margin-top: 4px;'>",
      "<span>Min: ", min_v, "</span>",
      "<span>Max: ", max_v, "</span>",
      "</div>",
      "</div>"
    )
  }

  pa_legend_html <- ""
  pa_obj <- if (!is.null(interactive_res$protected_areas)) interactive_res$protected_areas else NULL
  if (!is.null(pa_obj) && nrow(pa_obj) > 0L) {
    pa_fill_col <- if (!is.null(interactive_res$params$protected_areas_fill)) interactive_res$params$protected_areas_fill else "#2e7d32"
    pa_border_col <- if (!is.null(interactive_res$params$protected_areas_border)) interactive_res$params$protected_areas_border else "#1b5e20"
    pa_alpha_val <- if (!is.null(interactive_res$params$protected_areas_alpha)) as.numeric(interactive_res$params$protected_areas_alpha) else 0.25
    pa_legend_html <- paste0(
      "<div style='margin-top: 14px; border-top: 1px solid #e2e8f0; padding-top: 12px;'>",
      "<div style='font-weight: 600; color: #334155; margin-bottom: 6px; font-size: 0.92em;'>Conservation Areas</div>",
      "<div style='display: flex; align-items: center; gap: 8px;'>",
      "<span style='display: inline-block; width: 18px; height: 12px; background-color: ", pa_fill_col, "; opacity: ", min(1, pa_alpha_val + 0.4), "; border: 1.5px solid ", pa_border_col, "; border-radius: 2px; flex-shrink: 0;'></span>",
      "<span style='font-size: 0.88em; color: #1e293b;'>Protected Areas (WDPA)</span>",
      "</div>",
      "</div>"
    )
  }

  paste0(
    "<div style='width: 270px; min-width: 240px; background: #ffffff; border: 1px solid #cbd5e1; border-radius: 6px; padding: 16px; box-shadow: 0 1px 4px rgba(0,0,0,0.06); display: flex; flex-direction: column; justify-content: flex-start;'>",
    "<div style='font-weight: 700; color: #1e293b; border-bottom: 2px solid #e2e8f0; padding-bottom: 6px; margin-bottom: 12px; font-size: 0.95em;'>Map Legend</div>",
    if (length(species_items) > 0) {
      paste0(
        "<div style='margin-bottom: 6px;'>",
        "<div style='font-weight: 600; color: #334155; margin-bottom: 8px; font-size: 0.92em;'>Species Occurrences</div>",
        paste(species_items, collapse = ""),
        "</div>"
      )
    } else "",
    pa_legend_html,
    grid_legend_html,
    "<div style='margin-top: auto; padding-top: 12px; border-top: 1px dashed #e2e8f0; color: #64748b; font-size: 0.82em;'>",
    "<em>Tip: Hover over cells and points for attributes. Zoom and pan freely without overlay obstructions.</em>",
    "</div>",
    "</div>"
  )
}

#' Generate HTML analysis and methods report
#'
#' Creates an HTML report containing map thumbnails, output file explanations,
#' Materials and Methods prose, cartographic specifications, IUCN metrics, grid
#' summaries, and BibTeX citations matching the Orangutan-R HTML style.
#'
#' @param info Named list of metadata assembled by [run_map()].
#' @param file Path of the output HTML file.
#' @return Invisibly the file path.
#' @keywords internal
generate_html_report <- function(info, file) {
  output_dir <- dirname(file)
  img_files <- list.files(output_dir, pattern = "\\.(png|jpeg|jpg)$", full.names = FALSE)

  # Check for cached grid data if not provided in info
  cache_grid_file <- file.path(output_dir, ".grid_cache.rds")
  if ((is.null(info$interactive_res) || is.null(info$interactive_res$grid_sf)) && file.exists(cache_grid_file)) {
    tryCatch({
      cg <- readRDS(cache_grid_file)
      info$interactive_res <- list(
        points = cg$points,
        grid_sf = cg$grid_sf,
        grid_counts_df = cg$grid_counts_df,
        params = list(grid_metric_label = cg$metric_label,
                      point_color = cg$point_color,
                      palette = cg$palette,
                      color_by = cg$color_by)
      )
      if (is.null(info$grid_summary_df)) info$grid_summary_df <- cg$grid_summary_df
      if (is.null(info$grid_counts_df)) info$grid_counts_df <- cg$grid_counts_df
      if (is.null(info$grid_km)) info$grid_km <- cg$grid_km
      if (is.null(info$grid_metric_label)) info$grid_metric_label <- cg$metric_label
    }, error = function(e) NULL)
  }

  # Check for cached IUCN data if not provided in info
  cache_iucn_file <- file.path(output_dir, ".iucn_cache.rds")
  if (is.null(info$iucn_report) && file.exists(cache_iucn_file)) {
    tryCatch({
      ci <- readRDS(cache_iucn_file)
      info$iucn_report <- ci$report
      info$iucn_constrained <- ci$is_constrained
      info$iucn_medium <- ci$medium
    }, error = function(e) NULL)
  }

  # Check for cached cleaning data if not provided in info
  cache_cleaning_file <- file.path(output_dir, ".cleaning_cache.rds")
  if (is.null(info$cleaning) && file.exists(cache_cleaning_file)) {
    tryCatch({
      cc <- readRDS(cache_cleaning_file)
      info$cleaning <- cc$clean_obj
      info$clean_report <- cc$clean_report
      info$clean_coords <- TRUE
    }, error = function(e) NULL)
  }

  # Check for cached protected areas data if not provided in info
  cache_pa_file <- file.path(output_dir, ".protected_areas_cache.rds")
  if (file.exists(cache_pa_file)) {
    tryCatch({
      cpa <- readRDS(cache_pa_file)
      if (is.null(info$protected_areas)) info$protected_areas <- cpa$protected_areas
      if (is.null(info$protected_areas_counts_df)) info$protected_areas_counts_df <- cpa$counts_df
      if (is.null(info$protected_areas_summary_df)) info$protected_areas_summary_df <- cpa$summary_df
      if (is.null(info$protected_areas_fill)) info$protected_areas_fill <- cpa$params$protected_areas_fill
      if (is.null(info$protected_areas_border)) info$protected_areas_border <- cpa$params$protected_areas_border
      if (is.null(info$protected_areas_alpha)) info$protected_areas_alpha <- cpa$params$protected_areas_alpha
      if (is.null(info$protected_areas_lwd)) info$protected_areas_lwd <- cpa$params$protected_areas_lwd
      if (is.null(info$protected_areas_linetype)) info$protected_areas_linetype <- cpa$params$protected_areas_linetype

      if (!is.null(info$interactive_res)) {
        if (is.null(info$interactive_res$protected_areas)) info$interactive_res$protected_areas <- cpa$protected_areas
        if (is.null(info$interactive_res$protected_areas_counts_df)) info$interactive_res$protected_areas_counts_df <- cpa$counts_df
        if (is.null(info$interactive_res$protected_areas_summary_df)) info$interactive_res$protected_areas_summary_df <- cpa$summary_df
        if (is.null(info$interactive_res$params$protected_areas_fill)) info$interactive_res$params$protected_areas_fill <- cpa$params$protected_areas_fill
        if (is.null(info$interactive_res$params$protected_areas_border)) info$interactive_res$params$protected_areas_border <- cpa$params$protected_areas_border
        if (is.null(info$interactive_res$params$protected_areas_alpha)) info$interactive_res$params$protected_areas_alpha <- cpa$params$protected_areas_alpha
        if (is.null(info$interactive_res$params$protected_areas_lwd)) info$interactive_res$params$protected_areas_lwd <- cpa$params$protected_areas_lwd
        if (is.null(info$interactive_res$params$protected_areas_linetype)) info$interactive_res$params$protected_areas_linetype <- cpa$params$protected_areas_linetype
      }
    }, error = function(e) NULL)
  }

  if (is.null(info$protected_areas_summary_df) && file.exists(file.path(output_dir, "protected_areas_summary.csv"))) {
    tryCatch({
      info$protected_areas_summary_df <- utils::read.csv(file.path(output_dir, "protected_areas_summary.csv"), check.names = FALSE)
    }, error = function(e) NULL)
  }
  if (is.null(info$protected_areas_counts_df) && file.exists(file.path(output_dir, "protected_areas_counts.csv"))) {
    tryCatch({
      info$protected_areas_counts_df <- utils::read.csv(file.path(output_dir, "protected_areas_counts.csv"), check.names = FALSE)
    }, error = function(e) NULL)
  }

  # Check for cached feature flags in output_dir
  cache_feat_file <- file.path(output_dir, ".features_cache.rds")
  if (file.exists(cache_feat_file)) {
    tryCatch({
      cf <- readRDS(cache_feat_file)
      if (!is.null(cf$admin_level)) {
        if (is.null(info$admin_level) || isTRUE(info$admin_level < cf$admin_level)) {
          info$admin_level <- cf$admin_level
        }
      }
      if (!isTRUE(info$clean_coords) && isTRUE(cf$clean_coords)) info$clean_coords <- TRUE
      if (!isTRUE(info$show_roads) && isTRUE(cf$show_roads)) info$show_roads <- TRUE
      if (!isTRUE(info$show_cities) && isTRUE(cf$show_cities)) info$show_cities <- TRUE
      if (!isTRUE(info$city_labels) && isTRUE(cf$city_labels)) info$city_labels <- TRUE
      if (!isTRUE(info$show_water) && isTRUE(cf$show_water)) info$show_water <- TRUE
      if (is.null(info$ecoregions) && isTRUE(cf$show_ecoregions)) info$ecoregions <- TRUE
      if (is.null(info$protected_areas) && isTRUE(cf$show_protected_areas)) info$show_protected_areas <- TRUE
      if (is.null(info$elevation) && isTRUE(cf$show_elevation)) info$elevation <- TRUE
      if (is.null(info$terrain) && isTRUE(cf$show_terrain)) info$terrain <- TRUE
      if (!isTRUE(info$inset_map) && isTRUE(cf$inset_map)) info$inset_map <- TRUE
      if (!isTRUE(info$facet) && isTRUE(cf$facet)) {
        info$facet <- TRUE
        if (is.null(info$facet_by) && !is.null(cf$facet_by)) info$facet_by <- cf$facet_by
      }
      if (!isTRUE(info$highlight_type) && isTRUE(cf$highlight_type)) info$highlight_type <- TRUE
      if (is.null(info$journal) && !is.null(cf$journal)) {
        info$journal <- cf$journal
        info$journal_cols <- cf$journal_cols
      }
      if (is.null(info$gis_formats) && !is.null(cf$gis_formats)) info$gis_formats <- cf$gis_formats
    }, error = function(e) NULL)
  }

  # Check for CSV tables if still missing
  if (is.null(info$grid_richness_summary_df) && file.exists(file.path(output_dir, "grid_richness_summary.csv"))) {
    tryCatch({
      info$grid_richness_summary_df <- utils::read.csv(file.path(output_dir, "grid_richness_summary.csv"), check.names = FALSE)
      if (is.null(info$grid_km)) info$grid_km <- 10
    }, error = function(e) NULL)
  }
  if (is.null(info$grid_density_summary_df) && file.exists(file.path(output_dir, "grid_record_density_summary.csv"))) {
    tryCatch({
      info$grid_density_summary_df <- utils::read.csv(file.path(output_dir, "grid_record_density_summary.csv"), check.names = FALSE)
      if (is.null(info$grid_km)) info$grid_km <- 10
    }, error = function(e) NULL)
  }
  if (is.null(info$grid_summary_df) && file.exists(file.path(output_dir, "grid_summary.csv"))) {
    tryCatch({
      info$grid_summary_df <- utils::read.csv(file.path(output_dir, "grid_summary.csv"), check.names = FALSE)
      if (is.null(info$grid_km)) info$grid_km <- 10
    }, error = function(e) NULL)
  }
  if (is.null(info$grid_counts_df) && file.exists(file.path(output_dir, "grid_cell_counts.csv"))) {
    tryCatch({
      info$grid_counts_df <- utils::read.csv(file.path(output_dir, "grid_cell_counts.csv"), check.names = FALSE)
      if (is.null(info$grid_km)) info$grid_km <- 10
    }, error = function(e) NULL)
  }
  
  make_table <- function(df_data) {
    rows <- vapply(seq_len(nrow(df_data)), function(i) {
      paste0("<tr><td><strong>", df_data[i, 1], "</strong></td><td>", df_data[i, 2], "</td></tr>")
    }, character(1))
    paste0("<table><thead><tr><th>Property</th><th>Specification</th></tr></thead><tbody>",
           paste(rows, collapse = ""), "</tbody></table>")
  }

  make_grid_summary_html_table <- function(df, type = "richness") {
    if (is.null(df) || nrow(df) == 0) return("")
    
    # Check if this is new 5-metric structure
    if ("min_species" %in% names(df) || type == "richness") {
      headers <- c("Occupied Grid Cells", "Min Species / Cell", "Max Species / Cell",
                   "Mean Species / Cell", "Median Species / Cell")
      th_html <- paste0("<tr>", paste(paste0("<th style='padding: 8px 12px; text-align: right; background-color: #2c3e50; color: white;'>", headers, "</th>"), collapse = ""), "</tr>")
      rows_html <- vapply(seq_len(nrow(df)), function(i) {
        row <- df[i, ]
        paste0(
          "<tr style='background-color: #ffffff; border-bottom: 1px solid #e2e8f0;'>",
          "<td style='padding: 8px 12px; text-align: right; font-weight: bold;'>", format(row$total_cells, big.mark = ","), "</td>",
          "<td style='padding: 8px 12px; text-align: right;'>", row$min_species, "</td>",
          "<td style='padding: 8px 12px; text-align: right;'>", row$max_species, "</td>",
          "<td style='padding: 8px 12px; text-align: right; color: #166534; font-weight: 600;'>", sprintf("%.2f", row$mean_species), "</td>",
          "<td style='padding: 8px 12px; text-align: right;'>", row$median_species, "</td>",
          "</tr>"
        )
      }, character(1))
      title_html <- "<h4 style='margin-top: 15px; margin-bottom: 5px; color: #2c3e50;'>Species Richness Grid Metrics</h4>"
    } else if ("min_records" %in% names(df) || type == "records") {
      headers <- c("Occupied Grid Cells", "Min Records / Cell", "Max Records / Cell",
                   "Mean Records / Cell", "Median Records / Cell")
      th_html <- paste0("<tr>", paste(paste0("<th style='padding: 8px 12px; text-align: right; background-color: #2c3e50; color: white;'>", headers, "</th>"), collapse = ""), "</tr>")
      rows_html <- vapply(seq_len(nrow(df)), function(i) {
        row <- df[i, ]
        paste0(
          "<tr style='background-color: #ffffff; border-bottom: 1px solid #e2e8f0;'>",
          "<td style='padding: 8px 12px; text-align: right; font-weight: bold;'>", format(row$total_cells, big.mark = ","), "</td>",
          "<td style='padding: 8px 12px; text-align: right;'>", row$min_records, "</td>",
          "<td style='padding: 8px 12px; text-align: right;'>", format(row$max_records, big.mark = ","), "</td>",
          "<td style='padding: 8px 12px; text-align: right; color: #166534; font-weight: 600;'>", sprintf("%.2f", row$mean_records), "</td>",
          "<td style='padding: 8px 12px; text-align: right;'>", row$median_records, "</td>",
          "</tr>"
        )
      }, character(1))
      title_html <- "<h4 style='margin-top: 15px; margin-bottom: 5px; color: #2c3e50;'>Occurrence Record Density Grid Metrics</h4>"
    } else {
      # Fallback for legacy format
      headers <- c("Taxon / Level", "Total Records", "Total Grid Cells", "Occupied Cells",
                   "Empty Cells", "Mean Records / Cell (occupied)", "Median Records (occupied)", "Median Records (all cells)")
      th_html <- paste0("<tr>", paste(paste0("<th style='padding: 8px 12px; text-align: left; background-color: #2c3e50; color: white;'>", headers, "</th>"), collapse = ""), "</tr>")
      rows_html <- vapply(seq_len(nrow(df)), function(i) {
        row <- df[i, ]
        is_spp <- identical(as.character(row$level), "spp") || grepl("spp", as.character(row$taxon), ignore.case = TRUE)
        row_bg <- if (is_spp) "background-color: #f1f5f9; font-weight: bold;" else if (i %% 2 == 0) "background-color: #fdfdfd;" else "background-color: #ffffff;"
        taxon_display <- if (is_spp) paste0("<strong>", row$taxon, "</strong>") else paste0("<em>", row$taxon, "</em>")
        empty_display <- paste0(format(row$empty_cells, big.mark = ","), " (", sprintf("%.1f%%", row$pct_empty), " empty)")
        paste0(
          "<tr style='", row_bg, " border-bottom: 1px solid #e2e8f0;'>",
          "<td style='padding: 8px 12px;'>", taxon_display, "</td>",
          "<td style='padding: 8px 12px; text-align: right;'>", format(row$total_records, big.mark = ","), "</td>",
          "<td style='padding: 8px 12px; text-align: right;'>", format(row$total_grid_cells, big.mark = ","), "</td>",
          "<td style='padding: 8px 12px; text-align: right;'>", format(row$occupied_cells, big.mark = ","), "</td>",
          "<td style='padding: 8px 12px; text-align: right;'>", empty_display, "</td>",
          "<td style='padding: 8px 12px; text-align: right; color: #166534; font-weight: 600;'>", sprintf("%.2f", row$mean_records_occupied), "</td>",
          "<td style='padding: 8px 12px; text-align: right;'>", row$median_records_occupied, "</td>",
          "<td style='padding: 8px 12px; text-align: right; color: #64748b;'>", row$median_records_all, "</td>",
          "</tr>"
        )
      }, character(1))
      title_html <- ""
    }

    paste0(
      title_html,
      "<div style='overflow-x: auto; margin: 10px 0 20px 0;'>",
      "<table style='width: 100%; border-collapse: collapse; font-size: 0.9em; box-shadow: 0 1px 3px rgba(0,0,0,0.05);'>",
      "<thead>", th_html, "</thead>",
      "<tbody>", paste(rows_html, collapse = ""), "</tbody>",
      "</table>",
      "</div>"
    )
  }

  make_grid_cells_html_table <- function(df) {
    if (is.null(df) || nrow(df) == 0) return("")

    base_cols <- c("grid_id", "lon_center", "lat_center", "x_proj", "y_proj", "species_richness", "total_records", "total")
    sp_cols <- setdiff(names(df), base_cols)

    col_headers <- c("Grid ID", "Centroid Lon (\u00b0)", "Centroid Lat (\u00b0)", "Richness", "Total Records", sp_cols)
    th_html <- paste0(
      "<tr>",
      paste(paste0("<th style='padding: 6px 10px; position: sticky; top: 0; background-color: #34495e; color: white; text-align: left; z-index: 2;'>",
                   col_headers, "</th>"), collapse = ""),
      "</tr>"
    )

    rows_html <- vapply(seq_len(nrow(df)), function(i) {
      row <- df[i, ]
      row_bg <- if (i %% 2 == 0) "background-color: #fdfdfd;" else "background-color: #ffffff;"

      sp_tds <- ""
      if (length(sp_cols) > 0) {
        vals <- vapply(sp_cols, function(col) as.character(row[[col]]), character(1))
        sp_tds <- paste(paste0("<td style='padding: 6px 10px; text-align: right;'>", vals, "</td>"), collapse = "")
      }

      paste0(
        "<tr style='", row_bg, " border-bottom: 1px solid #e2e8f0;'>",
        "<td style='padding: 6px 10px; font-weight: bold;'>", row$grid_id, "</td>",
        "<td style='padding: 6px 10px; font-family: monospace;'>", sprintf("%.4f", row$lon_center), "</td>",
        "<td style='padding: 6px 10px; font-family: monospace;'>", sprintf("%.4f", row$lat_center), "</td>",
        "<td style='padding: 6px 10px; text-align: right; font-weight: 600; color: #166534;'>", row$species_richness, "</td>",
        "<td style='padding: 6px 10px; text-align: right; font-weight: 600;'>", format(row$total_records, big.mark = ","), "</td>",
        sp_tds,
        "</tr>"
      )
    }, character(1))

    paste0(
      "<h4 style='color: #2c3e50; margin: 25px 0 8px 0;'>Spatial Cell-by-Cell Occurrence Data (<code>grid_cell_counts.csv</code> &mdash; ", nrow(df), " occupied cells)</h4>",
      "<div style='max-height: 380px; overflow-y: auto; overflow-x: auto; border: 1px solid #cbd5e1; border-radius: 4px; box-shadow: inset 0 1px 2px rgba(0,0,0,0.05); margin-bottom: 15px;'>",
      "<table style='width: 100%; border-collapse: collapse; font-size: 0.85em; margin: 0;'>",
      "<thead style='position: sticky; top: 0; z-index: 10;'>", th_html, "</thead>",
      "<tbody>", paste(rows_html, collapse = ""), "</tbody>",
      "</table>",
      "</div>"
    )
  }

  make_protected_areas_summary_html_table <- function(df) {
    if (is.null(df) || nrow(df) == 0) return("")
    headers <- c("Taxon / Level", "Total Records", "Records in PAs", "% in PAs",
                 "Outside PAs", "Total PAs", "Occupied PAs", "% PAs Occupied",
                 "Mean Recs / PA", "Median Recs (occupied)")
    th_html <- paste0("<tr>", paste(paste0("<th style='padding: 8px 12px; text-align: left;'>", headers, "</th>"), collapse = ""), "</tr>")

    rows_html <- vapply(seq_len(nrow(df)), function(i) {
      row <- df[i, ]
      is_spp <- identical(as.character(row$level), "spp") || grepl("spp", as.character(row$taxon), ignore.case = TRUE)
      row_bg <- if (is_spp) "background-color: #f1f5f9; font-weight: bold;" else if (i %% 2 == 0) "background-color: #fdfdfd;" else "background-color: #ffffff;"
      taxon_display <- if (is_spp) paste0("<strong>", row$taxon, "</strong>") else paste0("<em>", row$taxon, "</em>")

      pct_in_display <- paste0(sprintf("%.1f%%", row$pct_records_in_pas))
      pct_occ_display <- paste0(format(row$occupied_pas, big.mark = ","), " (", sprintf("%.1f%%", 100 - row$pct_empty_pas), ")")

      paste0(
        "<tr style='", row_bg, " border-bottom: 1px solid #e2e8f0;'>",
        "<td style='padding: 8px 12px;'>", taxon_display, "</td>",
        "<td style='padding: 8px 12px; text-align: right;'>", format(row$total_records, big.mark = ","), "</td>",
        "<td style='padding: 8px 12px; text-align: right; color: #166534; font-weight: 600;'>", format(row$records_in_pas, big.mark = ","), "</td>",
        "<td style='padding: 8px 12px; text-align: right; color: #166534; font-weight: 600;'>", pct_in_display, "</td>",
        "<td style='padding: 8px 12px; text-align: right; color: #64748b;'>", format(row$records_outside_pas, big.mark = ","), "</td>",
        "<td style='padding: 8px 12px; text-align: right;'>", format(row$total_pas, big.mark = ","), "</td>",
        "<td style='padding: 8px 12px; text-align: right;'>", pct_occ_display, "</td>",
        "<td style='padding: 8px 12px; text-align: right; color: #2e7d32; font-weight: 600;'>", sprintf("%.2f", row$mean_records_occupied), "</td>",
        "<td style='padding: 8px 12px; text-align: right;'>", row$median_records_occupied, "</td>",
        "</tr>"
      )
    }, character(1))

    paste0(
      "<div style='overflow-x: auto; margin: 15px 0;'>",
      "<table style='width: 100%; border-collapse: collapse; font-size: 0.9em; box-shadow: 0 1px 3px rgba(0,0,0,0.05);'>",
      "<thead style='background-color: #1b5e20; color: white;'>", th_html, "</thead>",
      "<tbody>", paste(rows_html, collapse = ""), "</tbody>",
      "</table>",
      "</div>"
    )
  }

  make_protected_areas_counts_html_table <- function(df) {
    if (is.null(df) || nrow(df) == 0) return("")

    base_cols <- c("pa_id", "name", "designation", "iucn_category", "status_year",
                   "rep_area_km2", "lon_center", "lat_center", "species_richness",
                   "total_records", "species_present")
    sp_cols <- setdiff(names(df), base_cols)

    col_headers <- c("PA Name", "Designation", "IUCN Cat.", "Status Year", "Area (km\u00b2)",
                     "Richness", "Total Records", sp_cols)
    th_html <- paste0(
      "<tr>",
      paste(paste0("<th style='padding: 6px 10px; position: sticky; top: 0; background-color: #2e7d32; color: white; text-align: left; z-index: 2;'>",
                   col_headers, "</th>"), collapse = ""),
      "</tr>"
    )

    rows_html <- vapply(seq_len(nrow(df)), function(i) {
      row <- df[i, ]
      has_recs <- as.numeric(row$total_records) > 0
      row_bg <- if (has_recs) {
        if (i %% 2 == 0) "background-color: #f7faf7;" else "background-color: #ffffff;"
      } else {
        "background-color: #fbfbfb; color: #94a3b8;"
      }

      sp_tds <- ""
      if (length(sp_cols) > 0) {
        vals <- vapply(sp_cols, function(col) as.character(row[[col]]), character(1))
        sp_tds <- paste(paste0("<td style='padding: 6px 10px; text-align: right;'>", vals, "</td>"), collapse = "")
      }

      area_str <- if (!is.na(row$rep_area_km2)) format(row$rep_area_km2, big.mark = ",") else "N/A"

      paste0(
        "<tr style='", row_bg, " border-bottom: 1px solid #e2e8f0;'>",
        "<td style='padding: 6px 10px; font-weight: 600;'>", htmltools::htmlEscape(row$name), "</td>",
        "<td style='padding: 6px 10px;'>", htmltools::htmlEscape(row$designation), "</td>",
        "<td style='padding: 6px 10px;'>", htmltools::htmlEscape(row$iucn_category), "</td>",
        "<td style='padding: 6px 10px; font-family: monospace;'>", row$status_year, "</td>",
        "<td style='padding: 6px 10px; text-align: right;'>", area_str, "</td>",
        "<td style='padding: 6px 10px; text-align: right; font-weight: 600; color: #166534;'>", row$species_richness, "</td>",
        "<td style='padding: 6px 10px; text-align: right; font-weight: 600;'>", format(row$total_records, big.mark = ","), "</td>",
        sp_tds,
        "</tr>"
      )
    }, character(1))

    paste0(
      "<h4 style='color: #2c3e50; margin: 25px 0 8px 0;'>Protected Area Occurrence Breakdown (<code>protected_areas_counts.csv</code> &mdash; ", nrow(df), " areas)</h4>",
      "<div style='max-height: 380px; overflow-y: auto; overflow-x: auto; border: 1px solid #cbd5e1; border-radius: 4px; box-shadow: inset 0 1px 2px rgba(0,0,0,0.05); margin-bottom: 15px;'>",
      "<table style='width: 100%; border-collapse: collapse; font-size: 0.85em; margin: 0;'>",
      "<thead style='position: sticky; top: 0; z-index: 10;'>", th_html, "</thead>",
      "<tbody>", paste(rows_html, collapse = ""), "</tbody>",
      "</table>",
      "</div>"
    )
  }
  
  has_elev <- isTRUE(info$show_elevation) || !is.null(info$elevation)
  has_terr <- isTRUE(info$show_terrain) || !is.null(info$terrain)
  has_topo <- !is.null(info$topo)
  has_depth <- isTRUE(info$water_depth)

  scope_desc <- if (!has_elev && !has_terr && !has_topo && !has_depth) {
    "Disabled (Default)"
  } else if (identical(info$elevation_scope, "land")) {
    "Land elevation only (Water depth / Bathymetry excluded)"
  } else if (identical(info$elevation_scope, "sea")) {
    "Water depth / Bathymetry only"
  } else {
    "Land elevation + Ocean depth (Both)"
  }

  cleaning_spec <- if (!is.null(info$cleaning)) {
    if (info$cleaning$n_removed > 0L) {
      paste0("Cleaned (", info$cleaning$n_removed, " record(s) removed; ", info$cleaning$n_cleaned, " retained)")
    } else {
      "Cleaned (all records valid; none removed)"
    }
  } else if (!is.null(info$clean_report)) {
    paste0("Cleaned (", info$clean_report, ")")
  } else if (isTRUE(info$clean_coords)) {
    "Enabled (all records valid; none removed)"
  } else {
    "Disabled (Default)"
  }

  spec_df <- data.frame(
    Property = c(
      "Generation Timestamp", "OneClickCartographics Version", "R Execution Environment",
      "Random Seed", "Input Data Source", "Occurrence Records Analyzed",
      "Coordinate Quality Cleaning",
      "Coloring Attribute", "Shape Mapping Attribute", "Projection & CRS",
      "Scale Tier & Geographic Span", "Natural Earth Scale", "Administrative Boundaries",
      "Populated Places & Cities", "Transportation Infrastructure (Roads)",
      "Hydrological Features", "Medium Filter", "Elevation & Bathymetry Scope",
      "Terrain Hillshade Overlay", "WWF Ecoregions Layer", "Protected Conservation Areas (WDPA)",
      "Taxonomic Type Highlighting",
      "IUCN Criterion B Evaluation", "Equal-Area Occupancy Grid", "Locator Overview Inset Globe",
      "Multi-Species Plate Faceting", "Publisher Journal Preset", "Figure Dimensions & Resolution",
      "Export Formats", "GIS Spatial Vector Formats",
      "Species Rendering Mode", "Cartographic Furniture"
    ),
    Specification = c(
      info$timestamp,
      paste0("v", info$package_version),
      info$r_version,
      if (!is.null(info$seed)) as.character(info$seed) else "12345 (default)",
      info$data_path,
      paste0(info$n_records, " occurrence(s)"),
      cleaning_spec,
      info$color_by,
      if (!is.null(info$shape_by)) info$shape_by else "Automatic (auto-mapped)",
      info$crs_label,
      paste0(info$tier, " (span ~", round(info$span_km), " km)"),
      if (is.null(info$scale_auto)) paste0("1:", info$ne_scale, "m") else paste0("1:", info$ne_scale, "m (", info$scale_auto, ")"),
      if (is.null(info$admin_level)) "None" else if (info$admin_level == 0) "Country borders" else "Country & Province/State/Department borders",
      if (isTRUE(info$show_cities)) paste0("Enabled", if (isTRUE(info$city_labels)) " (with place name labels)" else "") else "Disabled (Default)",
      if (isTRUE(info$show_roads)) "Enabled (Major highways & secondary roads)" else "Disabled (Default)",
      if (isTRUE(info$show_water)) "Enabled (Oceans, Lakes & Rivers)" else "Disabled (Default)",
      if (!is.null(info$medium_dropped) && info$medium_dropped > 0L) paste0(info$keep_medium, " (", info$medium_dropped, " point(s) outside medium dropped)") else info$keep_medium,
      scope_desc,
      if (!is.null(info$terrain) || isTRUE(info$show_terrain)) paste0("Enabled (elevatr zoom level ", if (!is.null(info$terrain_zoom)) info$terrain_zoom else "auto", ")") else "Disabled",
      if (!is.null(info$ecoregions) || isTRUE(info$show_ecoregions)) "Enabled (WWF TEOW outlines)" else "Disabled",
      if (!is.null(info$protected_areas) || isTRUE(info$show_protected_areas)) {
        pa_spec <- "Enabled (World Database on Protected Areas [WDPA]"
        if (!is.null(info$protected_areas_fill)) {
          pa_spec <- paste0(pa_spec, "; fill: ", info$protected_areas_fill,
                            " [alpha ", if (!is.null(info$protected_areas_alpha)) info$protected_areas_alpha else "0.25",
                            "], border: ", if (!is.null(info$protected_areas_border)) info$protected_areas_border else "#1b5e20",
                            " [width ", if (!is.null(info$protected_areas_lwd)) info$protected_areas_lwd else "0.35",
                            ", ", if (!is.null(info$protected_areas_linetype)) info$protected_areas_linetype else "solid", "]")
        }
        paste0(pa_spec, ")")
      } else "Disabled (Default)",
      if (isTRUE(info$highlight_type)) "Enabled (Diamond markers for Holotypes/Syntypes)" else "Disabled",
      if (!is.null(info$iucn_report)) paste0("Enabled (Habitat constraint: ", info$iucn_medium, ")") else "Disabled",
      if (!is.null(info$grid_km)) paste0(info$grid_km, " km cells (", if (!is.null(info$grid_metric_label)) info$grid_metric_label else "records", ")") else "Disabled",
      if (isTRUE(info$inset_map)) paste0("Enabled", if (!is.null(info$inset_pos_resolved)) paste0(" (Position: ", info$inset_pos_resolved, ")") else "") else "Disabled (Default)",
      if (isTRUE(info$facet)) paste0("Enabled (Facet by: ", if (!is.null(info$facet_by)) info$facet_by else info$color_by, ")") else "Disabled",
      if (!is.null(info$journal)) paste0(toupper(info$journal), " (", info$journal_cols, "-column preset)") else "Custom (Default)",
      paste0(if (!is.null(info$width)) info$width else 10, " x ", if (!is.null(info$height)) info$height else 8, " inches @ ", if (!is.null(info$dpi)) info$dpi else 600, " DPI"),
      paste(if (!is.null(info$formats)) info$formats else c("pdf", "png"), collapse = ", "),
      if (!is.null(info$gis_formats) && length(info$gis_formats) > 0L) paste(toupper(info$gis_formats), collapse = ", ") else "None (Default)",
      info$rendering,
      info$furniture
    ),
    stringsAsFactors = FALSE
  )

  methods_paragraph <- paste0(
    "Distribution maps were generated using the R package OneClickCartographics (Anonymous Authors, 2026) in R v",
    info$r_version, ". Geographic occurrences were mapped using a dynamically centered ",
    info$crs_label, " projection (WGS84 datum). ",
    if (!is.null(info$clean_report)) paste0(info$clean_report, " ") else "",
    "Cartographic base boundaries were obtained from Natural Earth (1:", info$ne_scale, "m scale). ",
    if (isTRUE(info$show_water)) "Hydrological features including oceans, major lakes, and river networks were incorporated. " else "",
    if (!is.null(info$protected_areas) || isTRUE(info$show_protected_areas)) "Protected conservation areas were mapped from the World Database on Protected Areas (WDPA; UNEP-WCMC, 2026). " else "",
    if (!is.null(info$grid_km)) paste0("Spatial occupancy was aggregated into equal-area cells of ", info$grid_km, " km side length. ") else "",
    if (!is.null(info$iucn_report)) {
      if (isTRUE(info$iucn_constrained)) {
        "Extent of Occurrence (EOO, Minimum Convex Polygon constrained to habitat boundaries to exclude unsuitable areas) and Area of Occupancy (AOO, 2x2 km grid) were evaluated following IUCN Red List Guidelines (IUCN, 2024). "
      } else {
        "Extent of Occurrence (EOO, Minimum Convex Polygon) and Area of Occupancy (AOO, 2x2 km grid) were evaluated following IUCN Red List Guidelines (IUCN, 2024). "
      }
    } else "",
    "Scale bars and directional indicators were calculated and rendered dynamically."
  )

  thumb_html <- character(0)
  format_map_title <- function(pf) {
    base <- tools::file_path_sans_ext(basename(pf))
    clean_name <- gsub("^[0-9]+[_-]", "", base) # strip leading 01_, 02_ if present
    clean_name <- gsub("[_-]", " ", clean_name)
    words <- strsplit(clean_name, "\\s+")[[1]]
    cap_words <- paste0(toupper(substring(words, 1, 1)), substring(words, 2))
    title_str <- paste(cap_words, collapse = " ")
    title_str <- gsub("\\bIucn\\b", "IUCN", title_str, ignore.case = TRUE)
    title_str <- gsub("\\bEoo\\b", "EOO", title_str, ignore.case = TRUE)
    title_str <- gsub("\\bAoo\\b", "AOO", title_str, ignore.case = TRUE)
    title_str <- gsub("\\bDem\\b", "DEM", title_str, ignore.case = TRUE)
    title_str <- gsub("\\bGis\\b", "GIS", title_str, ignore.case = TRUE)
    title_str <- gsub("\\bWdpa\\b", "WDPA", title_str, ignore.case = TRUE)
    title_str <- gsub("\\bWwf\\b", "WWF", title_str, ignore.case = TRUE)
    title_str <- gsub("\\bKm\\b", "km", title_str, ignore.case = TRUE)
    title_str <- gsub("\\bQc\\b", "QC", title_str, ignore.case = TRUE)
    if (!nzchar(title_str) || tolower(title_str) == "map") {
      "Distribution Map"
    } else {
      paste0("Distribution Map: ", title_str)
    }
  }

  if (length(img_files) > 0) {
    sorted_imgs <- sort(img_files)
    thumb_cards <- vapply(sorted_imgs, function(pf) {
      title_txt <- format_map_title(pf)
      paste0(
        "<div style='margin: 25px 0;'>",
        "<h3 style='color: #2c3e50; margin-bottom: 8px;'>", title_txt, "</h3>",
        "<div class='plot-container'><img src='", pf, "' alt='", pf, "'></div>",
        "<p style='text-align: center; color: #7f8c8d; font-size: 0.9em; margin-top: 5px;'>File: <code>", pf, "</code></p>",
        "</div>"
      )
    }, character(1))
    thumb_html <- paste(thumb_cards, collapse = "\n")
  } else {
    pdf_files <- list.files(output_dir, pattern = "\\.pdf$", full.names = FALSE)
    if (length(pdf_files) > 0) {
      sorted_pdfs <- sort(pdf_files)
      thumb_cards <- vapply(sorted_pdfs, function(pf) {
        title_txt <- format_map_title(pf)
        paste0(
          "<div style='margin: 25px 0;'>",
          "<h3 style='color: #2c3e50; margin-bottom: 8px;'>", title_txt, "</h3>",
          "<div class='plot-container'>",
          "<object data='", pf, "' type='application/pdf' width='100%' height='650px' style='border: 1px solid #ddd; border-radius: 4px; box-shadow: 0 4px 12px rgba(0,0,0,0.08);'>",
          "<p>Your browser does not support inline PDF viewing. <a href='", pf, "' target='_blank'>Click here to open or download ", pf, "</a>.</p>",
          "</object>",
          "</div>",
          "<p style='text-align: center; color: #7f8c8d; font-size: 0.9em; margin-top: 5px;'>Vector PDF Map: <a href='", pf, "' target='_blank'><code>", pf, "</code></a></p>",
          "</div>"
        )
      }, character(1))
      thumb_html <- paste(thumb_cards, collapse = "\n")
    } else {
      thumb_html <- "<div class='warning'>No map file was found in the output directory.</div>"
    }
  }

  # Build consolidated interactive Leaflet maps (embedded via self-contained iframes, no standalone files)
  interactive_html <- ""
  if (!is.null(info$interactive_res) &&
      requireNamespace("leaflet", quietly = TRUE) &&
      requireNamespace("htmlwidgets", quietly = TRUE)) {
    tryCatch({
      # 1. Species Richness interactive map (outside legend)
      m_rich <- interactive_map(info$interactive_res, metric = "richness", legend_inside = FALSE, legend_position = "outside")
      tmp_r <- tempfile(fileext = ".html")
      htmlwidgets::saveWidget(m_rich, file = tmp_r, selfcontained = TRUE)
      txt_r <- paste(readLines(tmp_r, warn = FALSE), collapse = "\n")
      unlink(tmp_r)
      esc_r <- gsub("&", "&amp;", txt_r, fixed = TRUE)
      esc_r <- gsub("\"", "&quot;", esc_r, fixed = TRUE)
      leg_r <- build_outside_legend_html(info$interactive_res, metric = "richness")

      # 2. Occurrence density / records interactive map (outside legend)
      m_rec <- interactive_map(info$interactive_res, metric = "records", legend_inside = FALSE, legend_position = "outside")
      tmp_rec <- tempfile(fileext = ".html")
      htmlwidgets::saveWidget(m_rec, file = tmp_rec, selfcontained = TRUE)
      txt_rec <- paste(readLines(tmp_rec, warn = FALSE), collapse = "\n")
      unlink(tmp_rec)
      esc_rec <- gsub("&", "&amp;", txt_rec, fixed = TRUE)
      esc_rec <- gsub("\"", "&quot;", esc_rec, fixed = TRUE)
      leg_rec <- build_outside_legend_html(info$interactive_res, metric = "records")

      interactive_html <- paste0(
        "<div class='section'>\n",
        "<h2>2. Interactive Biodiversity Web Maps</h2>\n",
        "<div class='interpretation'>Explore occurrence points and grid metrics interactively with full zoom, hover tooltips, and layer toggles. Legends are rendered outside the interactive map canvas to ensure an unobstructed field of view.</div>\n",
        "<h3 style='color: #2c3e50; margin: 20px 0 8px 0;'>2.1 Interactive Species Richness Map</h3>\n",
        "<p style='color: #555; font-size: 0.9em; margin-bottom: 10px;'>Hover over any 10 km grid cell to view cell ID, species richness, and the complete species list. Click individual occurrence points to inspect specimen details.</p>\n",
        "<div style='display: flex; flex-direction: row; gap: 16px; align-items: stretch; margin-bottom: 25px; flex-wrap: wrap;'>\n",
        "  <div style='flex: 1; min-width: 320px;'>\n",
        "    <iframe srcdoc=\"", esc_r, "\" style=\"width: 100%; height: 580px; border: 1px solid #cbd5e1; border-radius: 6px; box-shadow: 0 2px 8px rgba(0,0,0,0.08); display: block;\"></iframe>\n",
        "  </div>\n",
        "  ", leg_r, "\n",
        "</div>\n",
        "<h3 style='color: #2c3e50; margin: 25px 0 8px 0;'>2.2 Interactive Occurrence Density & Records Map</h3>\n",
        "<p style='color: #555; font-size: 0.9em; margin-bottom: 10px;'>Hover over any 10 km grid cell to view total specimen occurrence records and the breakdown of specimen counts per species.</p>\n",
        "<div style='display: flex; flex-direction: row; gap: 16px; align-items: stretch; margin-bottom: 15px; flex-wrap: wrap;'>\n",
        "  <div style='flex: 1; min-width: 320px;'>\n",
        "    <iframe srcdoc=\"", esc_rec, "\" style=\"width: 100%; height: 580px; border: 1px solid #cbd5e1; border-radius: 6px; box-shadow: 0 2px 8px rgba(0,0,0,0.08); display: block;\"></iframe>\n",
        "  </div>\n",
        "  ", leg_rec, "\n",
        "</div>\n",
        "</div>\n"
      )
    }, error = function(e) {
      interactive_html <<- ""
    })
  }

  html <- c(
    "<!DOCTYPE html>",
    "<html>",
    "<head>",
    "<meta charset='utf-8'>",
    "<title>OneClickCartographics Analysis Report</title>",
    "<style>",
    "body { font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif; line-height: 1.6; color: #333; max-width: 1100px; margin: 0 auto; padding: 20px; }",
    "h1 { color: #2c3e50; border-bottom: 2px solid #eee; padding-bottom: 10px; }",
    "h2 { color: #34495e; margin-top: 30px; }",
    "h3 { color: #7f8c8d; }",
    ".section { background: #f9f9f9; padding: 20px; border-radius: 5px; margin-bottom: 30px; border: 1px solid #eaeded; }",
    ".plot-container { text-align: center; margin: 20px 0; }",
    ".plot-container img { max-width: 100%; max-height: 650px; height: auto; border: 1px solid #ddd; border-radius: 4px; padding: 5px; background: #fff; box-shadow: 0 4px 12px rgba(0,0,0,0.08); }",
    ".interpretation { background: #e8f4f8; padding: 15px; border-left: 5px solid #2980b9; margin-bottom: 20px; border-radius: 0 4px 4px 0; }",
    ".warning { background: #fdf5e6; padding: 15px; border-left: 5px solid #e67e22; margin-bottom: 20px; border-radius: 0 4px 4px 0; }",
    ".success { background: #eafaf1; padding: 15px; border-left: 5px solid #27ae60; margin-bottom: 20px; border-radius: 0 4px 4px 0; }",
    "table { border-collapse: collapse; width: 100%; margin: 15px 0; font-size: 0.9em; }",
    "th { background: #34495e; color: #fff; padding: 8px 12px; text-align: left; }",
    "td { padding: 8px 12px; border-bottom: 1px solid #ddd; }",
    "tr:nth-child(even) { background: #f2f2f2; }",
    "tr:hover { background: #ddeeff; }",
    "pre { font-family: 'Consolas', 'Courier New', monospace; background: #272822; color: #f8f8f2; padding: 15px; border-radius: 5px; overflow-x: auto; }",
    "blockquote { background: #ffffff; border-left: 5px solid #34495e; padding: 15px; margin: 15px 0; font-style: italic; border-radius: 0 4px 4px 0; }",
    "</style>",
    "</head>",
    "<body>",
    "<h1>OneClickCartographics Distribution Analysis & Methods Report</h1>",
    "<div class='interpretation'><strong>Cite this work as:</strong> Anonymous Authors (2026). OneClickCartographics: Automated Publication-Grade Cartography for Biodiversity Science. R package version 1.0.0. <a href='https://github.com/anonymous-peer/OneClickCartographics' target='_blank'>https://github.com/anonymous-peer/OneClickCartographics</a></div>",
    "<h3>GitHub repository</h3>",
    "<div class='interpretation'>The full source code, documentation, and issue tracker are available at <a href='https://github.com/anonymous-peer/OneClickCartographics' target='_blank'>https://github.com/anonymous-peer/OneClickCartographics</a>.</div>",
    
    # Section 1: Rendered Map Output
    "<div class='section'>",
    "<h2>1. Rendered Map Output</h2>",
    "<div class='interpretation'>Below are the publication-grade vector maps generated during this analysis run.</div>",
    thumb_html,
    "</div>",

    # Section 2: Interactive Biodiversity Web Maps
    if (nzchar(interactive_html)) interactive_html else "",

    # Section 3: Materials and Methods Paragraph
    "<div class='section'>",
    "<h2>3. Materials and Methods Paragraph</h2>",
    "<div class='interpretation'>Copy and paste the text below directly into your manuscript's Materials & Methods section:</div>",
    paste0("<blockquote>", methods_paragraph, "</blockquote>"),
    "</div>",

    # Section 4: Data & Cartographic Specifications
    "<div class='section'>",
    "<h2>4. Data & Cartographic Specifications</h2>",
    make_table(spec_df),
    if (!is.null(info$cleaning) && length(info$cleaning$flags) > 0) {
      paste0(
        "<div class='warning' style='margin-top: 15px;'>",
        "<h4 style='margin: 0 0 6px 0; color: #b45309;'>Coordinate Quality Cleaning & Dropped Records Audit</h4>",
        "<p style='margin: 0 0 8px 0;'><strong>Action Summary:</strong> ", info$cleaning$report, "</p>",
        "<ul style='margin: 0; padding-left: 20px;'>",
        paste0("<li style='margin-bottom: 4px;'>", info$cleaning$flags, "</li>", collapse = ""),
        "</ul>",
        "</div>"
      )
    } else if (!is.null(info$cleaning)) {
      "<div class='success' style='margin-top: 15px;'><strong>Coordinate Quality:</strong> All coordinates passed automated quality checks (no invalid, out-of-bounds, or placeholder coordinates detected).</div>"
    } else if (!is.null(info$clean_report)) {
      paste0("<div class='interpretation' style='margin-top: 15px;'><strong>Coordinate Quality:</strong> ", info$clean_report, "</div>")
    } else "",
    "</div>",

    # Section 5: IUCN Conservation Assessment (if available)
    if (!is.null(info$iucn_report)) c(
      "<div class='section'>",
      "<h2>5. IUCN Red List Conservation Assessment (Criterion B)</h2>",
      "<div class='interpretation'>Extent of Occurrence (EOO) and Area of Occupancy (AOO) evaluated following IUCN Red List Guidelines (IUCN, 2024).</div>",
      paste0("<pre style='background:#f8f9fa; color:#2c3e50; border:1px solid #dcdfe6; padding:15px; border-radius:4px;'>", info$iucn_report, "</pre>"),
      "</div>"
    ) else "",


    # Section 6: Protected Areas Conservation & Gap Analysis (if available)
    if (!is.null(info$protected_areas_summary_df) || !is.null(info$protected_areas_counts_df)) {
      pa_summary_tbl <- if (!is.null(info$protected_areas_summary_df)) make_protected_areas_summary_html_table(info$protected_areas_summary_df) else ""
      pa_counts_tbl <- if (!is.null(info$protected_areas_counts_df)) make_protected_areas_counts_html_table(info$protected_areas_counts_df) else ""
      c(
        "<div class='section'>",
        "<h2>6. Protected Areas Conservation & Gap Analysis</h2>",
        "<div class='interpretation'>Spatial intersection and conservation representation analysis between occurrence records and the World Database on Protected Areas (WDPA). Quantifies the proportion of specimens and species represented within formal protected reserves vs. unprotected habitats (conservation gap analysis), followed by a breakdown of species richness and record density per protected area. All generated CSV tables (<code>protected_areas_summary.csv</code> and <code>protected_areas_counts.csv</code>) are presented below.</div>",
        pa_summary_tbl,
        pa_counts_tbl,
        "<p class='success' style='margin-top: 15px;'>Protected areas summary metrics exported to <code>protected_areas_summary.csv</code>. Per-reserve species richness and record counts exported to <code>protected_areas_counts.csv</code>.</p>",
        "</div>"
      )
    } else "",

    # Section 7: Density & Species Richness Grid Summary (if available)
    if (!is.null(info$grid_km)) {
      grid_rich_tbl <- if (!is.null(info$grid_richness_summary_df)) make_grid_summary_html_table(info$grid_richness_summary_df, type = "richness") else ""
      grid_dens_tbl <- if (!is.null(info$grid_density_summary_df)) make_grid_summary_html_table(info$grid_density_summary_df, type = "records") else ""
      if (!nzchar(grid_rich_tbl) && !nzchar(grid_dens_tbl) && !is.null(info$grid_summary_df)) {
        grid_rich_tbl <- make_grid_summary_html_table(info$grid_summary_df)
      }
      grid_cells_tbl <- if (!is.null(info$grid_counts_df)) make_grid_cells_html_table(info$grid_counts_df) else ""
      c(
        "<div class='section'>",
        "<h2>7. Occupancy Grid Analysis & Summary Tables</h2>",
        paste0("<div class='interpretation'>Spatial occurrence data aggregated into equal-area cells of size <strong>", info$grid_km, " km</strong>. Summary metrics are calculated strictly across occupied cells (excluding empty cells where counts are 0 across the geographic bounding extent). Separate summary tables are provided for species richness and occurrence record density, with all generated CSV tables (<code>grid_richness_summary.csv</code>, <code>grid_record_density_summary.csv</code>, and <code>grid_cell_counts.csv</code>) represented below.</div>"),
        grid_rich_tbl,
        grid_dens_tbl,
        grid_cells_tbl,
        if (!is.null(info$grid_contour)) paste0("<p style='margin-top: 15px;'><strong>Grid Contour Line:</strong> <code>", info$grid_contour, "</code></p>") else "",
        if (!is.null(info$grid_bins)) paste0("<p style='margin-top: 10px;'><strong>Grid Legend Categories:</strong> <code>", info$grid_bins, "</code></p>") else "",
        "<p class='success' style='margin-top: 15px;'>Grid summary metrics exported to <code>grid_richness_summary.csv</code> and <code>grid_record_density_summary.csv</code>. Cell-by-cell spatial coordinates and counts exported to <code>grid_cell_counts.csv</code>.</p>",
        "</div>"
      )
    } else "",

    # Section 8: Citations & BibTeX Entry
    "<div class='section'>",
    "<h2>8. Software & Data References</h2>",
    "<div class='interpretation'>Please cite the underlying base layers and spatial tools used to construct this map:</div>",
    "<ul>",
    "<li><strong>Base Boundaries:</strong> Natural Earth (public domain), <a href='https://www.naturalearthdata.com/' target='_blank'>https://www.naturalearthdata.com/</a></li>",
    "<li><strong>Occurrence Data:</strong> GBIF.org occurrence download</li>",
    if (!is.null(info$elevation) || !is.null(info$terrain)) "<li><strong>Elevation / Terrain:</strong> AWS Terrain Tiles (SRTM) via <code>elevatr</code></li>" else "",
    if (!is.null(info$topo)) "<li><strong>Topography:</strong> ETOPO1 Global Relief Model, NOAA NCEI</li>" else "",
    if (!is.null(info$ecoregions)) "<li><strong>Ecoregions:</strong> Olson et al. (2001), <em>Terrestrial Ecoregions of the World</em>, BioScience 51(11):933-938</li>" else "",
    if (!is.null(info$protected_areas) || isTRUE(info$show_protected_areas)) "<li><strong>Protected Areas:</strong> UNEP-WCMC and IUCN (2026), <em>Protected Planet: The World Database on Protected Areas (WDPA)</em>, Cambridge, UK: UNEP-WCMC and IUCN. Available at: <a href='https://www.protectedplanet.net/' target='_blank'>https://www.protectedplanet.net/</a></li>" else "",
    "</ul>",
    "<h3>BibTeX Entry:</h3>",
    "<pre>@manual{Anonymous2026,\n  title  = {OneClickCartographics: Automated Publication-Grade Cartography for Biodiversity Science},\n  author = {Anonymous Authors},\n  year   = {2026},\n  note   = {R package version 1.0.0},\n  url    = {https://github.com/anonymous-peer/OneClickCartographics}\n}</pre>",
    "</div>",

    "</body>",
    "</html>"
  )

  html <- html[nzchar(html)]
  writeLines(html, file)
  invisible(file)
}

#' Alias for backwards compatibility
#' @keywords internal
write_methods_file <- generate_html_report

#' Save the map to disk in one or more formats
#'
#' @param p The map (`ggplot` or `patchwork` object).
#' @param output_dir Directory to save into.
#' @param filename Base file name (no extension).
#' @param formats Character vector of formats ("pdf", "png", "svg").
#' @param dpi Numeric resolution for raster formats.
#' @param width,height Numeric size in inches.
#' @return Character vector of written file paths.
#' @keywords internal
save_map_files <- function(p, output_dir, filename, formats, dpi, width, height) {
  if (is.null(width) || !is.numeric(width) || width <= 0) width <- 10
  if (is.null(height) || !is.numeric(height) || height <= 0) height <- round(width * 0.75, 2)
  files <- character(0)
  fmts <- tolower(formats)
  fmts <- setdiff(fmts, c("html", "htm"))
  if (length(fmts) == 0) fmts <- "pdf"

  for (fmt in fmts) {
    ext <- switch(fmt,
                  "tif" = "tiff",
                  "jpg" = "jpeg",
                  fmt)
    f <- file.path(output_dir, paste0(filename, ".", ext))
    if (ext %in% c("tiff", "tif")) {
      ggplot2::ggsave(f, plot = p, width = width, height = height,
                      dpi = dpi, units = "in", compression = "lzw")
    } else {
      ggplot2::ggsave(f, plot = p, width = width, height = height,
                      dpi = dpi, units = "in")
    }
    files <- c(files, f)
  }

  files
}

#' Save a map to disk
#'
#' Saves a `ggplot` map object, or a result list from [run_map()] or [construct_map()],
#' to one or more output formats (PDF, PNG, TIFF, JPEG, SVG).
#'
#' @param map A `ggplot` map object, or a result list returned by [run_map()] or [construct_map()].
#' @param filename Character: output filename (without extension). Default is `"map"`.
#' @param output_dir Character: directory to save into. Default is `"."`.
#' @param formats Character vector of formats: `"pdf"` (default), `"png"`, `"tiff"`, `"jpeg"`, `"svg"`.
#' @param dpi Numeric: resolution in DPI (default 600).
#' @param width Numeric: width in inches (default 10).
#' @param height Numeric or `NULL`: height in inches. When `NULL`, calculated to fit snug aspect ratio.
#' @return Character vector of saved file paths (invisibly).
#' @export
#' @examples
#' \dontrun{
#' res <- run_map("localities.csv", formats = NULL)
#' # Customize or inspect the ggplot object
#' custom_p <- res$p + ggplot2::ggtitle("My Custom Biodiversity Map")
#' # Save with any desired format and filename
#' save_map(custom_p, filename = "my_custom_map", formats = "pdf")
#' }
save_map <- function(map, filename = "map", output_dir = ".", formats = "pdf",
                     dpi = 600, width = 10, height = NULL) {
  p_obj <- if (is.list(map) && !inherits(map, "ggplot")) {
    if (!is.null(map$p)) map$p else map[[1]]
  } else {
    map
  }
  if (!inherits(p_obj, "ggplot")) {
    stop("Input 'map' must be a ggplot object or a result list containing a ggplot map.")
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  invisible(save_map_files(p_obj, output_dir, filename, formats, dpi, width, height))
}

#' Save one or more maps to disk
#'
#' Saves a `ggplot` map, a list of `ggplot` maps, or a result object to disk in the
#' requested formats (PDF, PNG, TIFF, JPEG, SVG).
#'
#' @param maps A `ggplot` object, a list or named list of `ggplot` objects, or a result list from [run_map()].
#' @param output_dir Character: directory to save maps into. Default is `"."`.
#' @param filename_prefix Character: base name prefix for saved files (default `"map"`).
#' @param formats Character vector of formats (default `"pdf"`).
#' @param dpi Numeric: resolution in DPI (default 600).
#' @param width Numeric: width in inches (default 10).
#' @param height Numeric or `NULL`: height in inches.
#' @param select Character vector or `NULL`: optional subset of map names or types
#'   to save (e.g. `c("terrain", "richness")` or `c("main", "records")`). Default is `NULL` (save all).
#' @param create_report Logical: whether to generate a consolidated HTML report. Default is `FALSE`.
#' @return Character vector of saved file paths (invisibly).
#' @export
#' @examples
#' \dontrun{
#' p1 <- run_map("localities.csv", formats = NULL)$p
#' p2 <- run_map("localities.csv", admin_level = 1, formats = NULL)$p
#' save_maps(list(base = p1, admin = p2), output_dir = "my_maps", formats = "pdf")
#' }
save_maps <- function(maps, output_dir = ".", filename_prefix = "map",
                      formats = "pdf", dpi = 600, width = 10, height = NULL,
                      create_report = FALSE, select = NULL) {
  if (is.list(maps) && !is.null(maps$params)) {
    if (missing(width) && !is.null(maps$params$width)) width <- maps$params$width
    if (missing(height) && !is.null(maps$params$height)) height <- maps$params$height
    if (missing(dpi) && !is.null(maps$params$dpi)) dpi <- maps$params$dpi
  }

  if (inherits(maps, "ggplot")) {
    maps <- list(main = maps)
  } else if (is.list(maps) && !is.null(maps$p)) {
    res_list <- list(main = maps$p, terrain = maps$p, base = maps$p)
    if (!is.null(maps$p_richness)) res_list$richness <- maps$p_richness
    if (!is.null(maps$p_records)) {
      res_list$records <- maps$p_records
      res_list$density <- maps$p_records
    }
    maps <- res_list
  } else if (!is.list(maps)) {
    stop("Input 'maps' must be a ggplot object, a list of ggplot objects, or a run_map result.")
  }

  if (!is.null(select) && length(select) > 0L) {
    sel_lower <- tolower(trimws(select))
    # Match by names or position
    matched_keys <- character(0)
    for (s in sel_lower) {
      m <- names(maps)[tolower(names(maps)) == s]
      if (length(m) > 0L) matched_keys <- c(matched_keys, m[1])
    }
    if (length(matched_keys) > 0L) {
      maps <- maps[unique(matched_keys)]
    }
  }

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  saved_files <- character(0)
  map_names <- names(maps)

  for (i in seq_along(maps)) {
    m_obj <- maps[[i]]
    if (is.null(m_obj)) next
    if (is.list(m_obj) && !inherits(m_obj, "ggplot") && !is.null(m_obj$p)) {
      m_obj <- m_obj$p
    }
    if (!inherits(m_obj, "ggplot")) next

    suffix <- if (!is.null(map_names) && nzchar(map_names[i])) map_names[i] else as.character(i)
    fname <- if (length(maps) == 1 && (is.null(map_names) || !nzchar(map_names[1]) || map_names[1] == "main")) {
      filename_prefix
    } else {
      paste0(filename_prefix, "_", suffix)
    }

    w_files <- save_map_files(m_obj, output_dir, fname, formats, dpi, width, height)
    saved_files <- c(saved_files, w_files)
  }

  if (create_report) {
    report_file <- file.path(output_dir, "00_map_report.html")
    tryCatch({
      info <- list(timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                   r_version = R.version.string,
                   package_version = as.character(utils::packageVersion("OneClickCartographics")),
                   formats = formats, dpi = dpi, width = width, height = height)
      generate_html_report(info, report_file)
      saved_files <- unique(c(saved_files, report_file))
    }, error = function(e) NULL)
  }

  message("Saved ", length(saved_files), " file(s) to ", output_dir)
  invisible(saved_files)
}