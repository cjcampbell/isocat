# Core assignment engine. These guard the published contract (normalized Gaussian
# density) and the legacy-Raster coercion at the function head.

test_that("assignment layers are normalized probability surfaces (sum to 1)", {
  a <- example_assignment()
  layer_sums <- as.numeric(terra::global(a, "sum", na.rm = TRUE)[, 1])
  expect_equal(layer_sums, rep(1, terra::nlyr(a)), tolerance = 1e-8)
})

test_that("legacy Raster* inputs are coerced to the same result as SpatRaster inputs", {
  skip_if_not_installed("raster")
  iso <- example_isoscape()
  iso_sd <- example_isoscape_sd()

  spat <- isotopeAssignmentModel(
    ID = "A", isotopeValue = -100, SD_indv = 5,
    precip_raster = iso, precip_SD_raster = iso_sd
  )
  # Pass RasterLayer inputs; before the "raster"->"Raster" fix these were never
  # coerced and the call errored at compareGeom().
  rr <- isotopeAssignmentModel(
    ID = "A", isotopeValue = -100, SD_indv = 5,
    precip_raster = raster::raster(iso), precip_SD_raster = raster::raster(iso_sd)
  )

  expect_s4_class(rr, "SpatRaster")
  expect_equal(terra::values(rr), terra::values(spat), tolerance = 1e-10)
})
