test_that("read_input parses CSV with standard column names", {
  f <- tempfile(fileext = ".csv")
  write.csv(data.frame(species = c("A", "B"),
                       decimalLatitude = c(40.5, 41.0),
                       decimalLongitude = c(-96.5, -96.0)),
            f, row.names = FALSE)
  d <- read_input(f)
  expect_equal(d$lat, c(40.5, 41.0))
  expect_equal(d$lon, c(-96.5, -96.0))
  expect_true("species" %in% names(d))
})

test_that("read_input parses TSV and Excel-style variants", {
  f <- tempfile(fileext = ".tsv")
  writeLines(c("species\tlatitude\tlongitude", "A\t10.5\t-70.25"),
             f)
  d <- read_input(f)
  expect_equal(d$lat, 10.5)
  expect_equal(d$lon, -70.25)
})

test_that("read_input drops missing and out-of-range coordinates", {
  f <- tempfile(fileext = ".csv")
  write.csv(data.frame(species = c("A", "B", "C", "D"),
                       lat = c(40, NA, 91, -96),
                       lon = c(-96, -96, -96, 0)),
            f, row.names = FALSE)
  expect_warning(d <- read_input(f), "Dropped 3 row")
  expect_equal(nrow(d), 1L)
})

test_that("read_input errors without coordinate columns", {
  f <- tempfile(fileext = ".csv")
  write.csv(data.frame(x = 1:3, y = 1:3), f, row.names = FALSE)
  expect_error(read_input(f), "Could not find latitude/longitude")
})

test_that("read_input errors on unsupported extension", {
  f <- tempfile(fileext = ".dat")
  writeLines("a,b", f)
  expect_error(read_input(f), "Unsupported input file type")
})

test_that("bundled datasets anoles and bees load properly", {
  data("anoles", envir = environment(), package = "OneClickCartographics")
  expect_true(is.data.frame(anoles))
  expect_equal(nrow(anoles), 1538L)

  data("bees", envir = environment(), package = "OneClickCartographics")
  expect_true(is.data.frame(bees))
  expect_equal(nrow(bees), 88919L)
})
