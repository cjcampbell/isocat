# Tests for the per-surface transforms migrated off raster:
# makecumsumSurface, makeOddsSurfaces, makeQuantileSurfaces, makeQuantileSimulationSurface.
# Anchored on a single normalized probability surface (sums to 1).

single_surface <- function() example_assignment(ids = "A", values = -100, sd_indv = 5)

test_that("makecumsumSurface returns a SpatRaster whose max equals the surface total (~1)", {
  cs <- makecumsumSurface(single_surface())
  expect_s4_class(cs, "SpatRaster")
  expect_equal(as.numeric(terra::global(cs, "max", na.rm = TRUE)[1, 1]), 1, tolerance = 1e-6)
})

test_that("makecumsumSurface rescale=TRUE maps to [0,1] (regression for divide-by-zero)", {
  cs <- makecumsumSurface(single_surface(), rescale = TRUE)
  rng <- as.numeric(terra::global(cs, c("min", "max"), na.rm = TRUE)[1, ])
  expect_equal(rng[1], 0, tolerance = 1e-8)
  expect_equal(rng[2], 1, tolerance = 1e-8)
  expect_false(any(is.infinite(terra::values(cs))))  # previously produced Inf/NaN
})

test_that("makeOddsSurfaces peaks at 1 at the surface maximum", {
  odds <- makeOddsSurfaces(single_surface())
  expect_s4_class(odds, "SpatRaster")
  expect_equal(as.numeric(terra::global(odds, "max", na.rm = TRUE)[1, 1]), 1, tolerance = 1e-8)
})

test_that("makeQuantileSurfaces lies in [0,1] and peaks at 1", {
  q <- makeQuantileSurfaces(single_surface())
  rng <- as.numeric(terra::global(q, c("min", "max"), na.rm = TRUE)[1, ])
  expect_gte(rng[1], 0)
  expect_equal(rng[2], 1, tolerance = 1e-8)
})

test_that("makeQuantileSimulationSurface validates input, stays in [0,1], and preserves NA cells", {
  r <- single_surface()
  set.seed(1)
  vq <- runif(500)
  qs <- makeQuantileSimulationSurface(r, ValidationQuantiles = vq, rescale = TRUE)
  expect_s4_class(qs, "SpatRaster")
  rng <- as.numeric(terra::global(qs, c("min", "max"), na.rm = TRUE)[1, ])
  expect_gte(rng[1], 0)
  expect_lte(rng[2], 1)
  # NA cells of the input remain NA after the mask step.
  expect_equal(sum(is.na(terra::values(qs))), sum(is.na(terra::values(r))))
  # Non-SpatRaster input is rejected.
  expect_error(makeQuantileSimulationSurface(1:10, ValidationQuantiles = vq))
})

test_that("surface transforms coerce legacy Raster* inputs to matching results", {
  skip_if_not_installed("raster")
  r <- single_surface()
  rr <- raster::raster(r)
  expect_equal(terra::values(makeOddsSurfaces(rr)),     terra::values(makeOddsSurfaces(r)),     tolerance = 1e-8)
  expect_equal(terra::values(makeQuantileSurfaces(rr)), terra::values(makeQuantileSurfaces(r)), tolerance = 1e-8)
  expect_equal(terra::values(makecumsumSurface(rr)),    terra::values(makecumsumSurface(r)),    tolerance = 1e-6)
})
