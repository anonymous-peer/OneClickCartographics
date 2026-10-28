#' Generate an interactive web map with hover tooltips and layer controls
#'
#' Converts a [run_map()] or [construct_map()] result into an interactive
#' Leaflet map widget. Hovering over grid cells or points reveals detailed
#' tooltips (cell ID, species richness, record count, species names, and
#' coordinates). Layer controls allow toggling grid cells and points on and off.
#'
#' @param res Result list returned by [run_map()] or [construct_map()].
#' @param output_file Character or `NULL`: if provided, saves a self-contained
#'   interactive HTML file to this path using `htmlwidgets::saveWidget()`.
#' @param metric Character: `"richness"` or `"records"`. Which metric to display
#'   on the grid cells. Defaults to `"richness"` if available.
#' @param tile_provider Character: Leaflet tile provider (e.g.
#'   `"CartoDB.Positron"`, `"CartoDB.DarkMatter"`, `"OpenStreetMap"`,
#'   `"Esri.WorldImagery"`). Default is `"CartoDB.Positron"`.
#' @param legend_inside Logical: include floating legends inside the interactive
#'   map canvas (default `FALSE` so legends do not obscure the map view).
#' @param legend_position Character: `"outside"` (default) or `"inside"`.
#' @param ... Additional arguments passed to [run_map()] when `res` is a file path or data frame.
#' @return An interactive `leaflet` map widget (invisibly if `output_file` is set).
#' @export
#' @examples
#' \dontrun{
#' res <- run_map("localities.csv", grid_km = 50)
#' interactive_map(res)
#' interactive_map(res, output_file = "interactive_map.html")
#' }
interactive_map <- function(res, output_file = NULL, metric = "auto",
                            tile_provider = "CartoDB.Positron",
                            legend_inside = FALSE, legend_position = "outside",
                            show_protected_areas = NULL,
                            protected_areas = NULL,
                            protected_areas_fill = NULL,
                            protected_areas_border = NULL,
                            protected_areas_alpha = NULL,
                            protected_areas_lwd = NULL,
                            protected_areas_width = NULL,
                            protected_areas_linetype = NULL, ...) {
  if (!requireNamespace("leaflet", quietly = TRUE)) {
    stop("Package 'leaflet' is required for interactive maps. Install it with install.packages('leaflet').")
  }

  if (is.character(res) || is.data.frame(res)) {
    res <- run_map(res, ...)
  }

  pts <- res$points
  grid_sf <- res$grid_sf

  # Create base leaflet map
  m <- leaflet::leaflet()
  m <- leaflet::addProviderTiles(m, tile_provider, group = "Base Map")

  overlay_groups <- character(0)

  # Resolve protected areas layer
  pa_sf <- if (!is.null(protected_areas)) {
    protected_areas
  } else if (!is.null(res$protected_areas)) {
    res$protected_areas
  } else {
    NULL
  }
  if (isFALSE(show_protected_areas)) pa_sf <- NULL

  # Add Protected Areas layer if available
  if (!is.null(pa_sf) && nrow(pa_sf) > 0L) {
    pa_ll <- tryCatch(sf::st_transform(pa_sf, 4326), error = function(e) NULL)

    if (!is.null(pa_ll) && nrow(pa_ll) > 0L) {
      pa_fill <- if (!is.null(protected_areas_fill)) protected_areas_fill else
                 if (!is.null(res$params$protected_areas_fill)) res$params$protected_areas_fill else "#2e7d32"
      pa_border <- if (!is.null(protected_areas_border)) protected_areas_border else
                   if (!is.null(res$params$protected_areas_border)) res$params$protected_areas_border else "#1b5e20"
      pa_alpha <- if (!is.null(protected_areas_alpha)) as.numeric(protected_areas_alpha) else
                  if (!is.null(res$params$protected_areas_alpha)) as.numeric(res$params$protected_areas_alpha) else 0.25
      pa_lwd <- if (!is.null(protected_areas_lwd)) as.numeric(protected_areas_lwd) else
                if (!is.null(protected_areas_width)) as.numeric(protected_areas_width) else
                if (!is.null(res$params$protected_areas_lwd)) as.numeric(res$params$protected_areas_lwd) else
                if (!is.null(res$params$protected_areas_width)) as.numeric(res$params$protected_areas_width) else 1.2
      pa_linetype <- if (!is.null(protected_areas_linetype)) protected_areas_linetype else
                     if (!is.null(res$params$protected_areas_linetype)) res$params$protected_areas_linetype else "solid"

      pa_dash <- if (identical(pa_linetype, "dashed")) "6, 6" else
                 if (identical(pa_linetype, "dotted")) "2, 5" else
                 if (identical(pa_linetype, "dotdash")) "2, 4, 6, 4" else NULL

      pa_tooltips <- vapply(seq_len(nrow(pa_ll)), function(i) {
        name_val <- if ("pa_name" %in% names(pa_ll)) pa_ll$pa_name[i] else
                    if ("NAME" %in% names(pa_ll)) pa_ll$NAME[i] else
                    if ("NAME_ENG" %in% names(pa_ll)) pa_ll$NAME_ENG[i] else paste0("Protected Area #", i)
        desig_val <- if ("pa_desig" %in% names(pa_ll)) pa_ll$pa_desig[i] else
                     if ("DESIG_ENG" %in% names(pa_ll)) pa_ll$DESIG_ENG[i] else
                     if ("DESIG" %in% names(pa_ll)) pa_ll$DESIG[i] else "Protected Area"
        iucn_val <- if ("pa_iucn" %in% names(pa_ll)) pa_ll$pa_iucn[i] else
                    if ("IUCN_CAT" %in% names(pa_ll)) pa_ll$IUCN_CAT[i] else "Not Reported"
        recs_val <- if ("total_records" %in% names(pa_ll)) pa_ll$total_records[i] else 0L
        rich_val <- if ("species_richness" %in% names(pa_ll)) pa_ll$species_richness[i] else 0L

        paste0(
          "<div style='font-family: sans-serif; min-width: 160px;'>",
          "<strong style='color:#1b5e20;'>", htmltools::htmlEscape(name_val), "</strong><br/>",
          "<b>Designation:</b> ", htmltools::htmlEscape(desig_val), "<br/>",
          "<b>IUCN Category:</b> ", htmltools::htmlEscape(iucn_val), "<br/>",
          "<b>Species Richness:</b> <span style='color:#2e7d32; font-weight:bold;'>", rich_val, "</span><br/>",
          "<b>Total Records:</b> <span style='font-weight:bold;'>", recs_val, "</span>",
          "</div>"
        )
      }, character(1))

      pa_popups <- vapply(seq_len(nrow(pa_ll)), function(i) {
        name_val <- if ("pa_name" %in% names(pa_ll)) pa_ll$pa_name[i] else
                    if ("NAME" %in% names(pa_ll)) pa_ll$NAME[i] else
                    if ("NAME_ENG" %in% names(pa_ll)) pa_ll$NAME_ENG[i] else paste0("Protected Area #", i)
        desig_val <- if ("pa_desig" %in% names(pa_ll)) pa_ll$pa_desig[i] else
                     if ("DESIG_ENG" %in% names(pa_ll)) pa_ll$DESIG_ENG[i] else
                     if ("DESIG" %in% names(pa_ll)) pa_ll$DESIG[i] else "Protected Area"
        iucn_val <- if ("pa_iucn" %in% names(pa_ll)) pa_ll$pa_iucn[i] else
                    if ("IUCN_CAT" %in% names(pa_ll)) pa_ll$IUCN_CAT[i] else "Not Reported"
        status_yr <- if ("pa_status_yr" %in% names(pa_ll)) pa_ll$pa_status_yr[i] else
                     if ("STATUS_YR" %in% names(pa_ll)) pa_ll$STATUS_YR[i] else "Unknown"
        area_val <- if ("rep_area_km2" %in% names(pa_ll)) pa_ll$rep_area_km2[i] else
                    if ("REP_AREA" %in% names(pa_ll)) pa_ll$REP_AREA[i] else NA
        area_txt <- if (!is.na(area_val) && is.numeric(area_val)) paste0(round(area_val, 1), " km&sup2;") else "Not Reported"
        recs_val <- if ("total_records" %in% names(pa_ll)) pa_ll$total_records[i] else 0L
        rich_val <- if ("species_richness" %in% names(pa_ll)) pa_ll$species_richness[i] else 0L
        spp_list <- if ("species_list" %in% names(pa_ll)) pa_ll$species_list[i] else ""

        txt <- paste0(
          "<div style='font-family: sans-serif; max-width: 290px; padding: 2px;'>",
          "<h4 style='margin:0 0 6px 0; color:#1b5e20; border-bottom: 2px solid #a5d6a7; padding-bottom:3px;'>",
          htmltools::htmlEscape(name_val), "</h4>",
          "<p style='margin:2px 0; font-size:12px;'><b>Designation:</b> ", htmltools::htmlEscape(desig_val), "</p>",
          "<p style='margin:2px 0; font-size:12px;'><b>IUCN Category:</b> ", htmltools::htmlEscape(iucn_val), "</p>",
          "<p style='margin:2px 0; font-size:12px;'><b>Status Year:</b> ", status_yr, "</p>",
          "<p style='margin:2px 0; font-size:12px;'><b>Reported Area:</b> ", area_txt, "</p>",
          "<div style='margin-top:6px; padding:6px; background:#e8f5e9; border-radius:4px; font-size:12px;'>",
          "<b>Conservation Metrics:</b><br/>",
          "&bull; <b>Total Records:</b> ", recs_val, "<br/>",
          "&bull; <b>Species Richness:</b> ", rich_val, " species",
          "</div>"
        )
        if (nzchar(spp_list)) {
          spps <- unlist(strsplit(spp_list, ", ", fixed = TRUE))
          txt <- paste0(txt, "<p style='margin:6px 0 2px 0; font-size:11px;'><b>Species Present (", length(spps), "):</b></p>",
                        "<ul style='margin:0 0 0 16px; padding:0; font-size:11px;'>")
          for (sp in utils::head(spps, 8L)) {
            txt <- paste0(txt, "<li><i>", htmltools::htmlEscape(sp), "</i></li>")
          }
          if (length(spps) > 8L) {
            txt <- paste0(txt, "<li><i>... and ", length(spps) - 8L, " more</i></li>")
          }
          txt <- paste0(txt, "</ul>")
        }
        paste0(txt, "</div>")
      }, character(1))

      m <- leaflet::addPolygons(
        m,
        data = pa_ll,
        fillColor = pa_fill,
        fillOpacity = pa_alpha,
        color = pa_border,
        weight = pa_lwd,
        dashArray = pa_dash,
        opacity = 0.9,
        highlightOptions = leaflet::highlightOptions(
          weight = pa_lwd + 1.5,
          color = "#004d40",
          fillOpacity = min(1, pa_alpha + 0.25),
          bringToFront = FALSE
        ),
        label = lapply(pa_tooltips, htmltools::HTML),
        popup = lapply(pa_popups, htmltools::HTML),
        labelOptions = leaflet::labelOptions(
          style = list("font-weight" = "normal", padding = "4px 8px"),
          textsize = "13px",
          direction = "auto"
        ),
        group = "Protected Areas"
      )

      overlay_groups <- c(overlay_groups, "Protected Areas")
    }
  }

  # Add grid cells layer if available
  if (!is.null(grid_sf) && nrow(grid_sf) > 0L) {
    grid_ll <- sf::st_transform(grid_sf, 4326)

    # Determine metric column
    metric_col <- "count"
    legend_title <- "Grid Count"
    if (metric == "auto") {
      if ("count_richness" %in% names(grid_ll)) {
        metric_col <- "count_richness"
        legend_title <- "Species Richness"
      } else if ("count" %in% names(grid_ll)) {
        metric_col <- "count"
        legend_title <- if (!is.null(res$params$grid_metric_label)) res$params$grid_metric_label else "Records"
      }
    } else if (metric %in% c("richness", "species") && "count_richness" %in% names(grid_ll)) {
      metric_col <- "count_richness"
      legend_title <- "Species Richness"
    } else if (metric %in% c("records", "points", "count") && "count_records" %in% names(grid_ll)) {
      metric_col <- "count_records"
      legend_title <- "Records"
    }

    vals <- grid_ll[[metric_col]]
    pal <- leaflet::colorNumeric(
      palette = c("#FFFFFF", "#FEE5D9", "#FCAE91", "#FB6A4A", "#DE2D26", "#A50F15"),
      domain = vals
    )

    # Construct HTML tooltips
    tooltips <- vapply(seq_len(nrow(grid_ll)), function(i) {
      cent <- sf::st_coordinates(sf::st_centroid(grid_ll$geometry[i]))
      txt <- paste0(
        "<div style='font-family: sans-serif; min-width: 140px;'>",
        "<strong style='color:#A50F15;'>Grid Cell #", i, "</strong><br/>",
        "<b>Latitude:</b> ", round(cent[2], 4), "&deg;<br/>",
        "<b>Longitude:</b> ", round(cent[1], 4), "&deg;<br/>"
      )
      if ("count_richness" %in% names(grid_ll)) {
        txt <- paste0(txt, "<b>Species Richness:</b> ", grid_ll$count_richness[i], "<br/>")
      }
      if ("count_records" %in% names(grid_ll)) {
        txt <- paste0(txt, "<b>Occurrence Records:</b> ", grid_ll$count_records[i], "<br/>")
      } else if ("count" %in% names(grid_ll)) {
        txt <- paste0(txt, "<b>Count:</b> ", grid_ll$count[i], "<br/>")
      }

      # If per-species counts exist in grid_counts_df
      if (!is.null(res$grid_counts_df) && nrow(res$grid_counts_df) >= i) {
        row_data <- res$grid_counts_df[i, , drop = FALSE]
        ignore_cols <- c("grid_id", "lon_center", "lat_center", "species_richness", "total_records", "total")
        sp_cols <- setdiff(names(row_data), ignore_cols)
        present_sps <- sp_cols[which(row_data[1, sp_cols] > 0)]
        if (length(present_sps) > 0L) {
          disp_sps <- utils::head(present_sps, 8L)
          txt <- paste0(txt, "<hr style='margin:4px 0;'/><b>Species present (", length(present_sps), "):</b><br/>",
                        "<ul style='margin:2px 0 0 16px; padding:0; font-size:11px;'>")
          for (sp in disp_sps) {
            txt <- paste0(txt, "<li><i>", sp, "</i> (", row_data[[sp]], ")</li>")
          }
          if (length(present_sps) > 8L) {
            txt <- paste0(txt, "<li><i>... and ", length(present_sps) - 8L, " more</i></li>")
          }
          txt <- paste0(txt, "</ul>")
        }
      }
      paste0(txt, "</div>")
    }, character(1))

    m <- leaflet::addPolygons(
      m,
      data = grid_ll,
      fillColor = ~pal(vals),
      fillOpacity = 0.75,
      color = "black",
      weight = 1,
      opacity = 1,
      highlightOptions = leaflet::highlightOptions(
        weight = 2.5,
        color = "#000000",
        fillOpacity = 0.9,
        bringToFront = TRUE
      ),
      label = lapply(tooltips, htmltools::HTML),
      labelOptions = leaflet::labelOptions(
        style = list("font-weight" = "normal", padding = "4px 8px"),
        textsize = "13px",
        direction = "auto"
      ),
      group = "Grid Cells"
    )

    if (isTRUE(legend_inside) || identical(legend_position, "inside")) {
      m <- leaflet::addLegend(
        m,
        pal = pal,
        values = vals,
        title = legend_title,
        position = "bottomright",
        group = "Grid Cells"
      )
    }

    overlay_groups <- c(overlay_groups, "Grid Cells")
  }

  # Add occurrence points if available
  if (!is.null(pts) && nrow(pts) > 0L) {
    pts_ll <- sf::st_transform(pts, 4326)
    coords <- sf::st_coordinates(pts_ll)

    sp_names <- if ("species" %in% names(pts_ll)) pts_ll$species else
      if (!is.null(res$params$color_by) && res$params$color_by %in% names(pts_ll)) pts_ll[[res$params$color_by]] else
        "Occurrence"

    pt_tooltips <- vapply(seq_len(nrow(pts_ll)), function(i) {
      paste0(
        "<div style='font-family: sans-serif;'>",
        "<b>Species:</b> <i>", sp_names[i], "</i><br/>",
        "<b>Lat:</b> ", round(coords[i, 2], 4), "&deg; | <b>Lon:</b> ", round(coords[i, 1], 4), "&deg;",
        "</div>"
      )
    }, character(1))

    u_sps <- sort(unique(as.character(sp_names)))
    custom_pal <- if (!is.null(res$params$point_color)) {
      res$params$point_color
    } else if (!is.null(res$params$palette)) {
      res$params$palette
    } else {
      NULL
    }

    pal_to_use <- NULL
    if (!is.null(custom_pal)) {
      if (!is.null(names(custom_pal)) && any(u_sps %in% names(custom_pal))) {
        pal_to_use <- unname(vapply(u_sps, function(sp) {
          if (sp %in% names(custom_pal)) custom_pal[[sp]] else "#7f7f7f"
        }, character(1)))
      } else if (length(custom_pal) >= length(u_sps)) {
        pal_to_use <- unname(custom_pal[seq_along(u_sps)])
      }
    }
    if (is.null(pal_to_use)) {
      okabe_ito <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7", "#E69F00", "#56B4E9", "#F0E442", "#000000")
      pal_to_use <- if (length(u_sps) <= length(okabe_ito)) okabe_ito[seq_along(u_sps)] else "viridis"
    }

    pt_pal <- leaflet::colorFactor(
      palette = pal_to_use,
      domain = u_sps
    )

    m <- leaflet::addCircleMarkers(
      m,
      data = pts_ll,
      radius = 4,
      fillColor = ~pt_pal(sp_names),
      fillOpacity = 0.85,
      color = "black",
      weight = 0.6,
      label = lapply(pt_tooltips, htmltools::HTML),
      labelOptions = leaflet::labelOptions(
        style = list("font-weight" = "normal", padding = "3px 6px"),
        textsize = "12px",
        direction = "auto"
      ),
      group = "Occurrence Points"
    )

    if ((isTRUE(legend_inside) || identical(legend_position, "inside")) &&
        length(u_sps) > 1L &&
        !isFALSE(res$params$show_species_legend) &&
        !isFALSE(res$params$species_legend) &&
        length(u_sps) <= 50L) {
      m <- leaflet::addLegend(
        m,
        position = "bottomright",
        pal = pt_pal,
        values = u_sps,
        title = if (!is.null(res$params$color_by)) res$params$color_by else "Species",
        opacity = 0.9,
        group = "Occurrence Points"
      )
    }

    overlay_groups <- c(overlay_groups, "Occurrence Points")
  }

  if (length(overlay_groups) > 0L) {
    m <- leaflet::addLayersControl(
      m,
      overlayGroups = overlay_groups,
      options = leaflet::layersControlOptions(collapsed = FALSE)
    )
  }

  # Save HTML if requested
  if (!is.null(output_file)) {
    if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
      stop("Package 'htmlwidgets' is required to save interactive HTML maps.")
    }
    dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
    htmlwidgets::saveWidget(m, file = output_file, selfcontained = TRUE)
    return(invisible(m))
  }

  m
}

#' Interactive Map Modifier Gadget
#'
#' Opens a Shiny Gadget in the IDE / RStudio Viewer or modal dialog. Allows
#' exploring map values via mouse hover and provides interactive controls to
#' modify layers, grid cell size, species richness vs. record counts, point
#' visibility, and boundaries in real time.
#'
#' @param res Result list returned by [run_map()] or [construct_map()].
#' @param ... Additional arguments passed to [run_map()] when `res` is a file path or data frame.
#' @return Updated result list invisibly when the user clicks **Done**.
#' @export
#' @examples
#' \dontrun{
#' res <- run_map("localities.csv")
#' mod_res <- modify_map(res)
#' }
modify_map <- function(res, ...) {
  if (!requireNamespace("shiny", quietly = TRUE) || !requireNamespace("miniUI", quietly = TRUE)) {
    stop("Packages 'shiny' and 'miniUI' are required for modify_map(). Install them with install.packages(c('shiny', 'miniUI')).")
  }

  if (is.character(res) || is.data.frame(res)) {
    res <- run_map(res, ...)
  }

  p_init <- res$params
  dat <- res$dat
  if (is.null(dat) && !is.null(p_init$data_path)) {
    dat <- utils::read.csv(p_init$data_path)
  }

  ui <- miniUI::miniPage(
    miniUI::miniTitleBar(
      "OneClickCartographics Map Modifier",
      right = miniUI::miniTitleBarButton("done", "Done", primary = TRUE)
    ),
    miniUI::miniContentPanel(
      padding = 10,
      shiny::fluidRow(
        shiny::column(
          width = 4,
          shiny::wellPanel(
            style = "max-height: 640px; overflow-y: auto; padding: 12px;",
            shiny::h4("Modify Map Layers", style = "margin-top:0; color:#2C3E50;"),
            shiny::selectInput(
              "admin_level", "Administrative Boundaries:",
              choices = c("None (clean default)" = "none",
                          "Country borders (Level 0)" = "0",
                          "Countries + Provinces/States/Departments (Level 1)" = "1"),
              selected = if (is.null(p_init$admin_level)) "none" else as.character(min(as.integer(p_init$admin_level), 1L))
            ),
            shiny::checkboxInput("show_water", "Bodies of Water (Ocean, Lakes, Rivers)",
                                 value = isTRUE(p_init$show_water)),
            shiny::checkboxInput("show_terrain", "Terrain Hillshade Relief",
                                 value = isTRUE(p_init$show_terrain)),
            shiny::hr(style = "margin: 8px 0;"),
            shiny::h4("Grid & Points", style = "margin-top:0; color:#2C3E50;"),
            shiny::selectInput(
              "grid_metric", "Grid Mode:",
              choices = c("No Grid" = "none",
                          "Species Richness Grid" = "richness",
                          "Occurrence Records Grid" = "records",
                          "Both (Dual Maps)" = "both"),
              selected = if (is.null(p_init$grid_km)) "none" else
                if (!is.null(p_init$grid_metric)) p_init$grid_metric else "richness"
            ),
            shiny::conditionalPanel(
              condition = "input.grid_metric != 'none'",
              shiny::sliderInput(
                "grid_km", "Cell size (km):",
                min = 10, max = 200,
                value = if (!is.null(p_init$grid_km)) p_init$grid_km else 50,
                step = 5
              ),
              shiny::checkboxInput(
                "show_points", "Overlay points on grid",
                value = isTRUE(p_init$show_points)
              )
            ),
            shiny::hr(style = "margin: 8px 0;"),
            shiny::actionButton("refresh_btn", "Update Map",
                                class = "btn-primary btn-block",
                                icon = shiny::icon("refresh"))
          )
        ),
        shiny::column(
          width = 8,
          shiny::tabsetPanel(
            shiny::tabPanel(
              "Live Map & Hover",
              shiny::plotOutput("live_map_plot", height = "500px", hover = "plot_hover"),
              shiny::wellPanel(
                style = "margin-top: 8px; padding: 8px; background-color: #F8F9F9; font-size: 13px;",
                shiny::htmlOutput("hover_readout")
              )
            ),
            shiny::tabPanel(
              "Interactive Web Map",
              if (requireNamespace("leaflet", quietly = TRUE)) {
                leaflet::leafletOutput("interactive_web_plot", height = "580px")
              } else {
                shiny::p("Install leaflet to view interactive web map.")
              }
            )
          )
        )
      )
    )
  )

  server <- function(input, output, session) {
    curr_res <- shiny::reactiveVal(res)

    shiny::observeEvent(input$refresh_btn, {
      admin_val <- if (input$admin_level == "none") NULL else as.integer(input$admin_level)
      grid_k <- if (input$grid_metric == "none") NULL else input$grid_km
      pts_val <- if (input$grid_metric != "none") input$show_points else TRUE

      new_res <- run_map(
        data_path = dat,
        admin_level = admin_val,
        show_water = input$show_water,
        show_terrain = input$show_terrain,
        grid_km = grid_k,
        grid_metric = if (input$grid_metric == "none") "auto" else input$grid_metric,
        show_points = pts_val,
        formats = "png",
        verbose = FALSE
      )
      curr_res(new_res)
    })

    output$live_map_plot <- shiny::renderPlot({
      curr_res()$p
    })

    output$hover_readout <- shiny::renderUI({
      h <- input$plot_hover
      if (is.null(h)) {
        return(shiny::HTML("<i>Hover over the map to view coordinates, cell counts, and species info...</i>"))
      }

      shiny::HTML(paste0(
        "<b>Pointer Coordinates (Projected):</b> X: ", round(h$x, 1), ", Y: ", round(h$y, 1), "<br/>",
        "<b>Grid Cells:</b> ", if (!is.null(curr_res()$grid_sf)) paste0(nrow(curr_res()$grid_sf), " occupied cells") else "None", " | ",
        "<b>Total Records:</b> ", curr_res()$n_records
      ))
    })

    if (requireNamespace("leaflet", quietly = TRUE)) {
      output$interactive_web_plot <- leaflet::renderLeaflet({
        interactive_map(curr_res())
      })
    }

    shiny::observeEvent(input$done, {
      shiny::stopApp(invisible(curr_res()))
    })
  }

  shiny::runGadget(ui, server, viewer = shiny::dialogViewer("OneClickCartographics Map Modifier", width = 1050, height = 750))
}
