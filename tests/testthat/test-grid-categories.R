test_that("build_grid_bins preserves default adaptive binning", {
  counts <- c(1, 2, 3, 5, 8, 12, 18, 25, 40)
  res_def <- build_grid_bins(counts)
  expect_type(res_def, "list")
  expect_s3_class(res_def$factor, "factor")
  expect_equal(res_def$method, "adaptive")
  expect_true(length(res_def$labels) >= 2)
  expect_equal(sum(is.na(res_def$factor)), 0)
})

test_that("user can define exact number of categories wanted", {
  counts <- c(1, 2, 3, 5, 8, 12, 18, 25, 40)

  # Via n_categories parameter
  res_3 <- build_grid_bins(counts, n_categories = 3)
  expect_equal(res_3$n_bins, 3)
  expect_equal(length(res_3$labels), 3)
  expect_equal(sum(is.na(res_3$factor)), 0)

  # Via user_steps as single integer
  res_4 <- build_grid_bins(counts, user_steps = 4)
  expect_equal(res_4$n_bins, 4)
  expect_equal(length(res_4$labels), 4)
  expect_equal(sum(is.na(res_4$factor)), 0)
})

test_that("user can define observation width across all categories", {
  counts <- c(1, 2, 5, 8, 12, 18, 25, 33)
  # 10 observations per category across
  res_step <- build_grid_bins(counts, step_size = 10)
  expect_equal(res_step$method, "step_size")
  expect_equal(res_step$labels[1], "1–10")
  expect_equal(res_step$labels[2], "11–20")
  expect_equal(res_step$labels[3], "21–30")
  expect_equal(sum(is.na(res_step$factor)), 0)
})

test_that("user can define observations on each category independently", {
  counts <- c(1, 2, 5, 8, 12, 18, 25, 33)

  # Numeric vector breaks defining cutoffs independently
  res_indep <- build_grid_bins(counts, breaks = c(1, 5, 15, 30))
  expect_equal(res_indep$method, "custom_breaks")
  expect_equal(res_indep$labels[1], "1–4")
  expect_equal(res_indep$labels[2], "5–14")
  expect_equal(res_indep$labels[3], "15–29")
  expect_equal(sum(is.na(res_indep$factor)), 0)

  # List of range intervals defining independent categories
  res_list <- build_grid_bins(counts, breaks = list(
    "Rare" = c(1, 4),
    "Moderate" = c(5, 15),
    "High" = c(16, 100)
  ))
  expect_equal(res_list$method, "custom_ranges")
  expect_equal(res_list$labels, c("Rare", "Moderate", "High"))
  expect_equal(sum(is.na(res_list$factor)), 0)
})

test_that("quantile/equal-count binning works across categories", {
  counts <- c(1, 1, 2, 3, 5, 8, 13, 21, 34, 55)
  res_q <- build_grid_bins(counts, n_categories = 4, method = "quantile")
  expect_equal(res_q$method, "quantile")
  expect_equal(res_q$n_bins, 4)
  expect_equal(sum(is.na(res_q$factor)), 0)
})

test_that("custom category labels can be supplied", {
  counts <- c(1, 5, 10, 20)
  res_lbl <- build_grid_bins(counts, n_categories = 2, labels = c("Low Density", "High Density"))
  expect_equal(levels(res_lbl$factor), c("Low Density", "High Density"))
  expect_equal(res_lbl$labels, c("Low Density", "High Density"))
})

test_that("run_map Occurrence Density No Points supports custom categories, across width, and independent breaks", {
  df <- data.frame(
    species = rep("Anolis huilae", 15),
    lon = c(-75.2, -75.1, -75.0, -75.2, -75.1, -75.2, -75.2, -75.0, -75.1, -75.1, -75.1, -75.2, -75.0, -75.2, -75.1),
    lat = c(3.2, 3.3, 3.4, 3.2, 3.3, 3.2, 3.2, 3.4, 3.3, 3.3, 3.3, 3.2, 3.4, 3.2, 3.3)
  )

  # 1. Default adaptive binning
  res_def <- run_map(
    df,
    grid_km = 10,
    grid_metric = "records",
    show_points = FALSE,
    formats = "png",
    output_dir = tempfile("test_grid_def")
  )
  expect_s3_class(res_def$p, "ggplot")
  expect_false(is.null(res_def$grid_bins))
  expect_equal(res_def$grid_bins$method, "adaptive")

  # 2. User defines number of categories (3 categories)
  res_3 <- run_map(
    df,
    grid_km = 10,
    grid_metric = "records",
    grid_categories = 3,
    show_points = FALSE,
    formats = "png",
    output_dir = tempfile("test_grid_3")
  )
  expect_equal(res_3$grid_bins$n_bins, 3)

  # 3. User defines observations across categories (width = 2)
  res_across <- run_map(
    df,
    grid_km = 10,
    grid_metric = "records",
    grid_category_width = 2,
    show_points = FALSE,
    formats = "png",
    output_dir = tempfile("test_grid_across")
  )
  expect_equal(res_across$grid_bins$method, "step_size")

  # 4. User defines observations independently (breaks = c(1, 3, 6))
  res_indep <- run_map(
    df,
    grid_km = 10,
    grid_metric = "records",
    grid_breaks = c(1, 3, 6),
    show_points = FALSE,
    formats = "png",
    output_dir = tempfile("test_grid_indep")
  )
  expect_equal(res_indep$grid_bins$method, "custom_breaks")
})

test_that("construct_map forwards grid category arguments cleanly", {
  df <- data.frame(
    species = rep("Anolis huilae", 10),
    lon = c(-75.2, -75.1, -75.0, -75.2, -75.1, -75.2, -75.2, -75.0, -75.1, -75.1),
    lat = c(3.2, 3.3, 3.4, 3.2, 3.3, 3.2, 3.2, 3.4, 3.3, 3.3)
  )

  res_bld <- construct_map(
    df,
    grid = "records",
    grid_km = 10,
    grid_categories = 3,
    formats = "png",
    output_dir = tempfile("test_builder_grid")
  )
  expect_equal(res_bld$grid_bins$n_bins, 3)
})
