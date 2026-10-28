test_that("clean_coordinates filters problematic coordinates", {
  df_dirty <- data.frame(
    species = rep("Test species", 6),
    lat = c(40.5, 0.0, NA, 120.0, 35.0, 42.1),
    lon = c(-96.5, 0.0, -96.5, 40.0, 35.0, -95.2),
    stringsAsFactors = FALSE
  )

  cl <- clean_coordinates(df_dirty, verbose = FALSE)
  expect_equal(cl$n_original, 6L)
  # Rows kept: row 1 (40.5, -96.5), row 4 swapped (40.0, 120.0 -> lat=40, lon=120), row 6 (42.1, -95.2)
  # Row 2 (0,0) dropped, Row 3 (NA) dropped, Row 5 (35,35) dropped
  expect_equal(cl$n_cleaned, 3L)
  expect_equal(cl$n_removed, 3L)
  expect_true(any(grepl("Null Island", cl$flags)))
  expect_true(any(grepl("swapped", cl$flags)))
  expect_true(nzchar(cl$report))
})

test_that("calculate_iucn_metrics computes EOO, AOO and Criterion B", {
  pts_df <- data.frame(
    species = "Species X",
    lat = c(40.0, 40.5, 41.0, 40.8),
    lon = c(-97.0, -96.5, -97.2, -96.0)
  )
  pts_sf <- sf::st_as_sf(pts_df, coords = c("lon", "lat"), crs = 4326)

  iucn <- calculate_iucn_metrics(pts_sf)
  expect_true(iucn$eoo_km2 > 0)
  expect_true(iucn$aoo_km2 > 0)
  expect_equal(iucn$n_points, 4L)
  expect_true(iucn$preliminary_status %in% c("Critically Endangered (CR)", "Endangered (EN)", "Vulnerable (VU)", "Least Concern (LC)"))
  expect_s3_class(iucn$polygon, "sf")
  expect_true(grepl("Extent of Occurrence", iucn$report))
})

test_that("resolve_journal resolves dimensions and DPI correctly", {
  j_zoo1 <- resolve_journal("zootaxa", columns = 1)
  expect_equal(j_zoo1$width_mm, 84)
  expect_equal(j_zoo1$dpi, 600)

  j_zoo2 <- resolve_journal("zootaxa", columns = 2)
  expect_equal(j_zoo2$width_mm, 175)
  expect_equal(j_zoo2$dpi, 600)
  j_zoo_300 <- resolve_journal("zootaxa", columns = 2, dpi = 300)
  expect_equal(j_zoo_300$dpi, 300)

  j_pen <- resolve_journal("pensoft", columns = 2)
  expect_equal(j_pen$width_mm, 166)

  j_nat <- resolve_journal("nature", columns = 2)
  expect_equal(j_nat$width_mm, 183)

  expect_null(resolve_journal(NULL))
})

test_that("is_taxon_name identifies scientific names vs files", {
  expect_true(OneClickCartographics:::is_taxon_name("Gryllus veletis"))
  expect_true(OneClickCartographics:::is_taxon_name("Panthera onca"))
  expect_false(OneClickCartographics:::is_taxon_name("localities.csv"))
  expect_false(OneClickCartographics:::is_taxon_name("path/to/data.xlsx"))
  expect_false(OneClickCartographics:::is_taxon_name("one_word"))
})

test_that("run_map integrates journal preset, IUCN metrics, cleaning, and BibTeX", {
  skip_if_offline()

  f <- tempfile(fileext = ".csv")
  df <- data.frame(
    species = rep(c("SpA", "SpB"), each = 5),
    lat = c(40.5, 40.8, 41.2, 40.9, 0.0, 40.6, 40.7, 41.1, 41.3, 40.4),
    lon = c(-96.5, -96.2, -96.8, -96.4, 0.0, -96.3, -96.7, -96.6, -96.9, -96.1),
    typeStatus = c("holotype", rep(NA, 9)),
    stringsAsFactors = FALSE
  )
  write.csv(df, f, row.names = FALSE)

  out_dir <- tempfile("pub_map_")
  res <- run_map(
    f,
    output_dir = out_dir,
    journal = "zootaxa",
    journal_cols = 2,
    clean_coords = TRUE,
    iucn = TRUE,
    highlight_type = TRUE,
    facet = TRUE,
    formats = "png",
    verbose = FALSE
  )

  # Check journal dimension preset applied
  expect_equal(res$params$width, 6.89) # 175 mm / 25.4
  expect_equal(res$params$dpi, 600)

  # Check cleaning was performed (0,0 was dropped)
  expect_equal(res$n_records, 9L)
  expect_true(!is.null(res$cleaning))

  # Check IUCN results
  expect_true(!is.null(res$iucn))
  expect_true(res$iucn$eoo_km2 > 0)
  expect_true(res$iucn$aoo_km2 > 0)

  # Check methods file contains copy-paste paragraph and BibTeX block
  methods_content <- readLines(res$methods_file)
  expect_true(any(grepl("Materials and Methods", methods_content, ignore.case = TRUE)))
  expect_true(any(grepl("@manual\\{Anonymous2026", methods_content, ignore.case = TRUE)))
  expect_true(any(grepl("IUCN Red List", methods_content, ignore.case = TRUE)))
})

test_that("calculate_iucn_metrics constrains EOO polygon to habitat boundaries", {
  pts_df <- data.frame(
    species = "Species Land",
    lat = c(30.0, 35.0, 30.0),
    lon = c(-90.0, -85.0, -80.0)
  )
  pts_sf <- sf::st_as_sf(pts_df, coords = c("lon", "lat"), crs = 4326)

  # Unconstrained
  iucn_raw <- calculate_iucn_metrics(pts_sf)
  expect_false(iucn_raw$is_constrained)
  expect_equal(iucn_raw$eoo_km2, iucn_raw$eoo_raw_km2)

  # Create a constraining polygon covering only the northern half
  poly_bound <- sf::st_as_sfc(sf::st_bbox(c(xmin = -95, ymin = 32.5, xmax = -75, ymax = 40), crs = sf::st_crs(4326)))
  bound_sf <- sf::st_sf(geometry = poly_bound)

  iucn_const <- calculate_iucn_metrics(pts_sf, constrain_sf = bound_sf)
  expect_true(iucn_const$is_constrained)
  expect_true(iucn_const$eoo_km2 < iucn_raw$eoo_km2)
  expect_equal(iucn_const$eoo_raw_km2, iucn_raw$eoo_km2)
  expect_true(grepl("constrained to habitat boundary", iucn_const$report))
})

test_that("run_map restricts IUCN evaluation to user-specified iucn_taxa", {
  skip_if_offline()
  f <- tempfile(fileext = ".csv")
  df <- data.frame(
    species = rep(c("Species Alpha", "Species Beta"), each = 5),
    lat = c(20.1, 20.2, 20.3, 20.4, 20.5, 21.1, 21.2, 21.3, 21.4, 21.5),
    lon = c(-80.1, -80.2, -80.3, -80.4, -80.5, -81.1, -81.2, -81.3, -81.4, -81.5),
    stringsAsFactors = FALSE
  )
  write.csv(df, f, row.names = FALSE)

  res <- run_map(
    f,
    iucn = TRUE,
    iucn_taxa = "Species Alpha",
    output_dir = tempfile("occ_"),
    formats = "png",
    verbose = FALSE
  )

  expect_true(!is.null(res$iucn))
  expect_true("Species Alpha" %in% names(res$iucn$by_group))
  expect_false("Species Beta" %in% names(res$iucn$by_group))

  # Verify that only Species Alpha points are plotted by default
  pt_layers <- Filter(function(l) inherits(l$geom, "GeomSf") && any(sf::st_is(l$data, c("POINT", "MULTIPOINT"))), res$p$layers)
  expect_true(length(pt_layers) > 0)
  plotted_species <- unique(pt_layers[[1]]$data$species)
  expect_equal(as.character(plotted_species), "Species Alpha")

  # Verify user override with show_taxa
  res_override <- run_map(
    f,
    iucn = TRUE,
    iucn_taxa = "Species Alpha",
    show_taxa = c("Species Alpha", "Species Beta"),
    output_dir = tempfile("occ_"),
    formats = "png",
    verbose = FALSE
  )
  pt_layers_ov <- Filter(function(l) inherits(l$geom, "GeomSf") && any(sf::st_is(l$data, c("POINT", "MULTIPOINT"))), res_override$p$layers)
  expect_setequal(as.character(unique(pt_layers_ov[[1]]$data$species)), c("Species Alpha", "Species Beta"))
})

test_that("faceted plate respects user-defined stacking and italicized headers", {
  skip_if_offline()
  f <- tempfile(fileext = ".csv")
  df <- data.frame(
    species = rep(c("Species Alpha", "Species Beta"), each = 5),
    lat = c(20.1, 20.2, 20.3, 20.4, 20.5, 21.1, 21.2, 21.3, 21.4, 21.5),
    lon = c(-80.1, -80.2, -80.3, -80.4, -80.5, -81.1, -81.2, -81.3, -81.4, -81.5),
    stringsAsFactors = FALSE
  )
  write.csv(df, f, row.names = FALSE)

  # Test row layout
  res_row <- run_map(
    f,
    facet = TRUE,
    facet_layout = "row",
    facet_italic = TRUE,
    output_dir = tempfile("occ_"),
    formats = "png",
    verbose = FALSE
  )
  expect_equal(res_row$p$facet$params$nrow, 1L)
  expect_equal(res_row$p$theme$strip.text$face, "italic")

  # Test column layout
  res_col <- run_map(
    f,
    facet = TRUE,
    facet_layout = "col",
    output_dir = tempfile("occ_"),
    formats = "png",
    verbose = FALSE
  )
  expect_equal(res_col$p$facet$params$ncol, 1L)
})

test_that("custom administrative boundaries in width and color work properly", {
  skip_if_offline()
  f <- tempfile(fileext = ".csv")
  df <- data.frame(
    species = rep("Anolis sagrei", 5),
    lat = c(22.1, 22.2, 22.3, 22.4, 22.5),
    lon = c(-80.1, -80.2, -80.3, -80.4, -80.5),
    stringsAsFactors = FALSE
  )
  write.csv(df, f, row.names = FALSE)

  # Custom country borders with custom color and width
  res_custom <- run_map(
    f,
    admin_level = 0,
    country_color = "firebrick",
    country_width = 0.85,
    output_dir = tempfile("occ_"),
    formats = "png",
    verbose = FALSE
  )
  # Check land layer aesthetics
  land_layer <- res_custom$p$layers[[1]]
  expect_equal(land_layer$aes_params$colour, "firebrick")
  expect_equal(land_layer$aes_params$linewidth, 0.85)

  # Admin level 2 standardization: level 2 is safely mapped to level 1 (provinces/states)
  res_lvl2 <- run_map(
    f,
    admin_level = 2,
    state_color = "navy",
    state_width = 0.45,
    output_dir = tempfile("occ_"),
    formats = "png",
    verbose = FALSE
  )
  expect_equal(res_lvl2$info$admin_level, 1L)

  # Check construct_map forwarding of country_color and width
  m_built <- construct_map(
    f,
    admin = "country",
    country_color = "darkgreen",
    country_width = 0.7,
    formats = "png",
    output_dir = tempfile("occ_"),
    verbose = FALSE
  )
  expect_equal(m_built$p$layers[[1]]$aes_params$colour, "darkgreen")
  expect_equal(m_built$p$layers[[1]]$aes_params$linewidth, 0.7)

  # Check list_map_aes contains new administrative settings
  aes_df <- list_map_aes(print = FALSE)
  expect_true("country_color" %in% aes_df$setting)
  expect_true("country_width" %in% aes_df$setting)
  expect_true("state_color" %in% aes_df$setting)
  expect_true("state_width" %in% aes_df$setting)
})

test_that("safe_wdpa_fetch_country retrieves protected areas cleanly", {
  skip_if_offline()
  skip_if_not_installed("wdpar")
  wdpa_test_dir <- file.path(tempdir(), "test_wdpa_dir")
  dir.create(wdpa_test_dir, recursive = TRUE, showWarnings = FALSE)
  pa <- safe_wdpa_fetch_country("LIE", wdpa_test_dir)
  expect_true(!is.null(pa) && nrow(pa) > 0L)
  expect_s3_class(pa, "sf")
})


