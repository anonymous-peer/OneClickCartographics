test_that("auto_utm_epsg picks the correct zone and hemisphere", {
  expect_equal(auto_utm_epsg(-96.7, 40.8), 32614)   # Lincoln, NE
  expect_equal(auto_utm_epsg(-100, 40), 32614)       # zone boundary edge
  expect_equal(auto_utm_epsg(10, 52), 32632)         # Berlin
  expect_equal(auto_utm_epsg(-70, -33), 32719)       # Santiago (south)
  expect_equal(auto_utm_epsg(180, 0), 32660)         # clamped max zone
  expect_equal(auto_utm_epsg(-180, 0), 32601)        # clamped min zone
})

test_that("crosses_dateline detects antimeridian crossing only", {
  sf_pts <- function(lons, lats) {
    sf::st_as_sf(data.frame(lon = lons, lat = lats),
                 coords = c("lon", "lat"), crs = 4326)
  }
  expect_true(crosses_dateline(sf_pts(c(170, -170), c(0, 0))))
  expect_false(crosses_dateline(sf_pts(c(-100, -90), c(0, 0))))
  expect_false(crosses_dateline(sf_pts(c(170, 179), c(0, 0))))
  expect_false(crosses_dateline(sf_pts(c(10, 20), c(0, 0))))
})

test_that("wrap_lons maps longitudes to 0-360", {
  pts <- sf::st_as_sf(data.frame(lon = c(175, -175), lat = c(0, 5)),
                      coords = c("lon", "lat"), crs = 4326)
  w <- wrap_lons(pts)
  x <- sf::st_coordinates(w)[, 1]
  expect_true(all(x > 170))
})

test_that("point_span_km approximates degrees of latitude", {
  pts <- sf::st_as_sf(data.frame(lon = c(-70, -70), lat = c(0, 10)),
                      coords = c("lon", "lat"), crs = 4326)
  expect_equal(point_span_km(pts), 1111.95, tolerance = 0.05)
})

test_that("scale_tier and tier_ne_scale use the expected boundaries", {
  expect_equal(scale_tier(100), "local")
  expect_equal(scale_tier(300), "local")
  expect_equal(scale_tier(301), "regional")
  expect_equal(scale_tier(2500), "regional")
  expect_equal(scale_tier(2501), "continental")
  expect_equal(scale_tier(8000), "continental")
  expect_equal(scale_tier(8001), "global")
  expect_equal(tier_ne_scale("local"), 10)
  expect_equal(tier_ne_scale("regional"), 10)
  expect_equal(tier_ne_scale("continental"), 50)
  expect_equal(tier_ne_scale("global"), 110)
})

test_that("auto_crs picks UTM, LAEA, equirectangular and polar CRS", {
  expect_equal(auto_crs(c(-96.7, 40.8), 200, FALSE)$epsg, 32614)
  expect_equal(auto_crs(c(-96.7, 40.8), 200, FALSE)$epsg, 32614)
  c1 <- auto_crs(c(-100, 40), 5000, FALSE)
  expect_null(c1$epsg)                                  # centroid-centred LAEA
  expect_match(c1$label, "Lambert Azimuthal")
  expect_equal(auto_crs(c(10, 30), 20000, FALSE)$epsg, 4087)
  expect_equal(auto_crs(c(170, 0), 500, TRUE)$epsg, 4087)  # dateline
  expect_equal(auto_crs(c(0, 85), 100, FALSE)$epsg, 3413)  # arctic
  expect_equal(auto_crs(c(0, -85), 100, FALSE)$epsg, 3031) # antarctic
})

test_that("expand_bbox adds a fraction of the largest dimension", {
  b <- c(xmin = 0, ymin = 0, xmax = 100, ymax = 50)
  e <- expand_bbox(b, frac = 0.1)
  expect_equal(e[["xmin"]], -10)
  expect_equal(e[["xmax"]], 110)
  expect_equal(e[["ymin"]], -10)
  expect_equal(e[["ymax"]], 60)
})