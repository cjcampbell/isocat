# Scaffold smoke test: confirms the harness runs and the shared fixtures build.
# Per-function behavioral tests are added alongside each function's migration.

test_that("bundled datasets build terra SpatRasters", {
  iso <- example_isoscape()
  iso_sd <- example_isoscape_sd()
  expect_s4_class(iso, "SpatRaster")
  expect_s4_class(iso_sd, "SpatRaster")
  expect_true(terra::compareGeom(iso, iso_sd))
})

test_that("example_assignment fixture returns a normalized multi-layer surface", {
  a <- example_assignment()
  expect_s4_class(a, "SpatRaster")
  expect_equal(terra::nlyr(a), 3L)
  # Each assignment layer is normalized to sum to 1.
  layer_sums <- as.numeric(terra::global(a, "sum", na.rm = TRUE)[, 1])
  expect_equal(layer_sums, rep(1, 3), tolerance = 1e-8)
})
