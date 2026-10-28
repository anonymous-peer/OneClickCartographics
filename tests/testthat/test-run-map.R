test_that("run_map requires data_path and fails when missing or NULL", {
  expect_error(run_map(), "Argument 'data_path' is required")
  expect_error(run_map(NULL), "Argument 'data_path' is required")
})

test_that("run_map on local spreadsheet returns a complete result", {
  skip_if_offline()
  f <- tempfile(fileext = ".csv")
  write.csv(data.frame(species = rep("Taxon A", 10),
                       lat = seq(20, 21, length.out = 10),
                       lon = seq(-80, -79, length.out = 10)),
            f, row.names = FALSE)
  res <- run_map(f, output_dir = tempfile("occ_"), formats = "png", verbose = FALSE)
  expect_type(res, "list")
  expect_s3_class(res$p, "ggplot")
  expect_true(file.exists(res$files[1]))
  expect_true(file.exists(res$methods_file))
  expect_equal(res$n_records, 10L)
  expect_true(res$tier %in% c("local", "regional", "continental", "global"))
  m <- readLines(res$methods_file)
  expect_true(any(grepl("Scale Tier", m, ignore.case = TRUE)))
  expect_true(any(grepl("Projection", m, ignore.case = TRUE)))
})

test_that("run_map selects the local tier and UTM for a small sample", {
  skip_if_offline()
  f <- tempfile(fileext = ".csv")
  set.seed(1)
  write.csv(data.frame(species = rep(c("A", "B"), each = 25),
                       lat = 40.8 + rnorm(50, 0, 0.3),
                       lon = -96.7 + rnorm(50, 0, 0.3)),
            f, row.names = FALSE)
  res <- run_map(f, output_dir = tempfile("occ_"), formats = "png", verbose = FALSE)
  expect_equal(res$tier, "local")
  expect_equal(res$epsg, 32614)
})

test_that("run_map draws colour and shape scales for multiple species", {
  skip_if_offline()
  f <- tempfile(fileext = ".csv")
  write.csv(data.frame(species = c("A", "B", "C"),
                       lat = c(40.8, 40.9, 41.0),
                       lon = c(-96.7, -96.6, -96.5)),
            f, row.names = FALSE)
  res <- run_map(f, output_dir = tempfile("occ_"), formats = "png", verbose = FALSE)
  classes <- vapply(res$p$scales$scales, function(s) class(s)[1], "")
  expect_true(sum(classes == "ScaleDiscrete") >= 2)  # colour + shape
})

test_that("run_map density grid adds a records scale", {
  skip_if_offline()
  f <- tempfile(fileext = ".csv")
  set.seed(2)
  write.csv(data.frame(species = "A",
                       lat = 40.8 + rnorm(40, 0, 0.3),
                       lon = -96.7 + rnorm(40, 0, 0.3)),
            f, row.names = FALSE)
  res <- run_map(f, output_dir = tempfile("occ_"), formats = "png",
                 grid_km = 20, verbose = FALSE)
  m <- readLines(res$methods_file)
  expect_true(any(grepl("20 km", m)))
})

test_that("run_map stops when the specified colour column is missing", {
  f <- tempfile(fileext = ".csv")
  write.csv(data.frame(pop = c("A"), lat = 40, lon = -96), f, row.names = FALSE)
  expect_error(run_map(f, output_dir = tempfile("occ_"), formats = "png",
                       color_by = "species", verbose = FALSE),
               "Column 'species' not found")
})

test_that("run_map handles user-defined map_bounds correctly", {
  skip_if_offline()
  f <- tempfile(fileext = ".csv")
  write.csv(data.frame(species = "A", lat = 22.5, lon = -82.5), f, row.names = FALSE)
  # User passes 4 coordinates in WGS84: c(xmin, xmax, ymin, ymax)
  res <- run_map(f, output_dir = tempfile("occ_"), formats = "png",
                 map_bounds = c(-85, -80, 20, 25), verbose = FALSE)
  expect_s3_class(res$p, "ggplot")
  expect_equal(res$extent[1], -85, tolerance = 0.5)
  expect_equal(res$extent[3], -80, tolerance = 0.5)
  expect_equal(res$extent[2], 20, tolerance = 0.5)
  expect_equal(res$extent[4], 25, tolerance = 0.5)
})

test_that("run_map validates invalid map_bounds", {
  f <- tempfile(fileext = ".csv")
  write.csv(data.frame(species = "A", lat = 22.5, lon = -82.5), f, row.names = FALSE)
  expect_error(run_map(f, output_dir = tempfile("occ_"), formats = "png",
                       map_bounds = c(-80, -85, 20, 25), verbose = FALSE),
               "xmin < xmax")
})

test_that("run_map accepts elevation_scope land, sea, and both", {
  skip_if_offline()
  f <- tempfile(fileext = ".csv")
  write.csv(data.frame(species = "A", lat = 22.5, lon = -84.5), f, row.names = FALSE)
  res_land <- run_map(f, output_dir = tempfile("occ_"), formats = "png",
                      map_bounds = c(-85, -84, 22, 23),
                      show_elevation = "land", verbose = FALSE)
  expect_s3_class(res_land$p, "ggplot")

  res_both <- run_map(f, output_dir = tempfile("occ_"), formats = "png",
                      map_bounds = c(-85, -84, 22, 23),
                      show_elevation = "both", verbose = FALSE)
  expect_s3_class(res_both$p, "ggplot")
})

test_that("build_grid_bins partitions counts appropriately across ranges", {
  # Small range (1..4)
  b_small <- OneClickCartographics:::build_grid_bins(c(1, 1, 2, 3, 4))
  expect_equal(b_small$n_bins, 4L)
  expect_equal(b_small$labels, c("1", "2", "3", "4"))
  expect_equal(as.character(b_small$factor), c("1", "1", "2", "3", "4"))

  # Medium range (1..25)
  b_med <- OneClickCartographics:::build_grid_bins(c(1, 5, 12, 25))
  expect_true(b_med$n_bins >= 3L && b_med$n_bins <= 6L)
  expect_false(any(is.na(b_med$factor)))

  # Large range (1..350)
  b_large <- OneClickCartographics:::build_grid_bins(c(1, 50, 150, 350))
  expect_true(b_large$n_bins >= 3L)
  expect_false(any(is.na(b_large$factor)))

  # User-defined steps
  b_custom <- OneClickCartographics:::build_grid_bins(c(1, 3, 10, 45), user_steps = c(1, 5, 20))
  expect_false(any(is.na(b_custom$factor)))
})

test_that("run_map defaults to no country boundaries and no water bodies", {
  skip_if_offline()
  f <- tempfile(fileext = ".csv")
  write.csv(data.frame(species = c("A", "A"),
                       lat = c(40.8, 41.2),
                       lon = c(-96.7, -96.2)),
            f, row.names = FALSE)
  res <- run_map(f, output_dir = tempfile("occ_"), formats = "png", verbose = FALSE)
  m <- readLines(res$methods_file)
  expect_true(any(grepl("Administrative Boundaries", m, ignore.case = TRUE)))
  expect_true(any(grepl("Disabled", m, ignore.case = TRUE)))
})

test_that("run_map enables country boundaries with show_countries = TRUE", {
  skip_if_offline()
  f <- tempfile(fileext = ".csv")
  write.csv(data.frame(species = c("A", "A"),
                       lat = c(40.8, 41.2),
                       lon = c(-96.7, -96.2)),
            f, row.names = FALSE)
  res <- run_map(f, output_dir = tempfile("occ_"), formats = "png",
                 show_countries = TRUE, verbose = FALSE)
  m <- readLines(res$methods_file)
  expect_true(any(grepl("Country borders", m, ignore.case = TRUE)))
})

test_that("run_map density grid supports custom contours and multi-species richness", {
  skip_if_offline()
  f <- tempfile(fileext = ".csv")
  set.seed(42)
  write.csv(data.frame(species = rep(c("Sp_A", "Sp_B", "Sp_C"), each = 20),
                       lat = 40.8 + rnorm(60, 0, 0.2),
                       lon = -96.7 + rnorm(60, 0, 0.2)),
            f, row.names = FALSE)
  res <- run_map(f, output_dir = tempfile("occ_"), formats = "png",
                 grid_km = 25,
                 grid_color = "navy", grid_linewidth = 0.8, grid_linetype = "dashed",
                 verbose = FALSE)
  m <- readLines(res$methods_file)
  expect_true(any(grepl("25 km", m)))
  expect_true(any(grepl("navy", m)))
  expect_s3_class(res$p, "ggplot")
})

test_that("run_map hides points by default on grid maps and shows them when requested", {
  skip_if_offline()
  f <- tempfile(fileext = ".csv")
  write.csv(data.frame(species = rep(c("A", "B"), each = 10),
                       lat = 40.8 + rnorm(20, 0, 0.2),
                       lon = -96.7 + rnorm(20, 0, 0.2)),
            f, row.names = FALSE)

  # Default on grid map: show_points is FALSE
  res_grid_nopoints <- run_map(f, output_dir = tempfile("occ_"), formats = "png",
                               grid_km = 30, verbose = FALSE)
  expect_false(res_grid_nopoints$params$show_points)

  # Explicit show_points = TRUE on grid map
  res_grid_withpoints <- run_map(f, output_dir = tempfile("occ_"), formats = "png",
                                 grid_km = 30, show_points = TRUE, verbose = FALSE)
  expect_true(res_grid_withpoints$params$show_points)

  # Default on non-grid map: show_points is TRUE
  res_pointmap <- run_map(f, output_dir = tempfile("occ_"), formats = "png",
                          verbose = FALSE)
  expect_true(res_pointmap$params$show_points)
})

test_that("run_map grid_metric = 'both' generates both richness and records maps", {
  skip_if_offline()
  f <- tempfile(fileext = ".csv")
  write.csv(data.frame(species = rep(c("A", "B", "C"), each = 10),
                       lat = 40.8 + rnorm(30, 0, 0.2),
                       lon = -96.7 + rnorm(30, 0, 0.2)),
            f, row.names = FALSE)

  res_both <- run_map(f, output_dir = tempfile("occ_"), formats = "png",
                      grid_km = 30, grid_metric = "both", verbose = FALSE)

  expect_s3_class(res_both$p_richness, "ggplot")
  expect_s3_class(res_both$p_records, "ggplot")
  expect_true(any(grepl("map_richness\\.png$", res_both$files)))
  expect_true(any(grepl("map_records\\.png$", res_both$files)))
  expect_true("species_richness" %in% names(res_both$grid_counts_df))
  expect_true("total_records" %in% names(res_both$grid_counts_df))
  expect_true(!is.null(res_both$grid_richness_summary))
  expect_true(!is.null(res_both$grid_density_summary))
  expect_true(all(c("total_cells", "min_species", "max_species", "mean_species", "median_species") %in% names(res_both$grid_richness_summary)))
  expect_true(all(c("total_cells", "min_records", "max_records", "mean_records", "median_records") %in% names(res_both$grid_density_summary)))
  expect_equal(res_both$grid_richness_summary$total_cells[1], nrow(res_both$grid_counts_df))
  expect_equal(res_both$grid_density_summary$total_cells[1], nrow(res_both$grid_counts_df))
  expect_true(any(grepl("grid_richness_summary\\.csv$", res_both$files)))
  expect_true(any(grepl("grid_record_density_summary\\.csv$", res_both$files)))
})

test_that("construct_map cleanly builds maps and decouples layers", {
  skip_if_offline()
  f <- tempfile(fileext = ".csv")
  write.csv(data.frame(species = c("A", "B"),
                       lat = c(40.8, 41.2),
                       lon = c(-96.7, -96.2)),
            f, row.names = FALSE)

  # Default minimalist map: plain map with points, no grid, no water, no admin borders
  res_default <- construct_map(f, output_dir = tempfile("occ_"), formats = "png", verbose = FALSE)
  expect_true(res_default$params$show_points)
  expect_null(res_default$grid_sf)
  expect_null(res_default$params$admin_level)
  expect_false(res_default$params$show_water)
  expect_s3_class(res_default$p, "ggplot")

  # Modular construction with state boundaries and water
  res <- construct_map(f, output_dir = tempfile("occ_"), formats = "png",
                       admin = "state", water = TRUE, verbose = FALSE)
  expect_equal(res$params$admin_level, 1L)
  expect_true(res$params$show_water)
  expect_s3_class(res$p, "ggplot")
})

test_that("interactive_map returns a leaflet widget with tooltips", {
  skip_if_offline()
  f <- tempfile(fileext = ".csv")
  write.csv(data.frame(species = c("A", "B"),
                       lat = c(40.8, 41.2),
                       lon = c(-96.7, -96.2)),
            f, row.names = FALSE)

  res <- run_map(f, output_dir = tempfile("occ_"), grid_km = 50, formats = "png", verbose = FALSE)
  leaf_map <- interactive_map(res)
  expect_s3_class(leaf_map, "leaflet")
})