# Tests for the point-extraction helpers migrated from sp/raster to terra:
# oddsAtSamplingLocation, quantileAtSamplingLocation, cumsumAtSamplingLocation.
#
# Anchored on a single normalized probability surface (sums to 1). At the cell
# holding the surface maximum:
#   - odds ratio     = 1  (p == max)
#   - quantile (ecdf)= 1  (max value)
#   - cumulative sum = 1  (all cells <= max, and the surface sums to 1)

single_surface <- function() example_assignment(ids = "A", values = -100, sd_indv = 5)

max_cell_xy <- function(r) {
  cell <- which.max(terra::values(r)[, 1])
  terra::xyFromCell(r, cell)  # 1x2 matrix: (x = Lon, y = Lat)
}

test_that("oddsAtSamplingLocation returns 1 at the surface maximum", {
  r <- single_surface()
  xy <- max_cell_xy(r)
  expect_equal(oddsAtSamplingLocation(r, Lat = xy[2], Lon = xy[1]), 1, tolerance = 1e-8)
})

test_that("quantileAtSamplingLocation is 1 at the maximum and within [0, 1] elsewhere", {
  r <- single_surface()
  xy <- max_cell_xy(r)
  expect_equal(quantileAtSamplingLocation(r, Lat = xy[2], Lon = xy[1]), 1, tolerance = 1e-8)

  set.seed(1)
  midcell <- sample(which(!is.na(terra::values(r)[, 1])), 1)
  xy2 <- terra::xyFromCell(r, midcell)
  q <- quantileAtSamplingLocation(r, Lat = xy2[2], Lon = xy2[1])
  expect_gte(q, 0)
  expect_lte(q, 1)
})

test_that("cumsumAtSamplingLocation at the maximum equals the normalized total (~1)", {
  r <- single_surface()
  xy <- max_cell_xy(r)
  expect_equal(cumsumAtSamplingLocation(r, Lat = xy[2], Lon = xy[1]), 1, tolerance = 1e-6)
})

test_that("extractors reject non-numeric coordinates and pass through NA coordinates", {
  r <- single_surface()

  expect_error(oddsAtSamplingLocation(r, Lat = "a", Lon = 1))
  expect_error(quantileAtSamplingLocation(r, Lat = "a", Lon = 1))
  expect_error(cumsumAtSamplingLocation(r, Lat = "a", Lon = 1))

  expect_true(is.na(oddsAtSamplingLocation(r, Lat = NA_real_, Lon = NA_real_)))
  expect_true(is.na(quantileAtSamplingLocation(r, Lat = NA_real_, Lon = NA_real_)))
  expect_true(is.na(cumsumAtSamplingLocation(r, Lat = NA_real_, Lon = NA_real_)))
})

test_that("legacy Raster* input is coerced and yields the same result as SpatRaster", {
  skip_if_not_installed("raster")
  r <- single_surface()
  xy <- max_cell_xy(r)
  rr <- raster::raster(r)  # SpatRaster -> RasterLayer

  expect_equal(
    oddsAtSamplingLocation(rr, Lat = xy[2], Lon = xy[1]),
    oddsAtSamplingLocation(r,  Lat = xy[2], Lon = xy[1]),
    tolerance = 1e-8
  )
  expect_equal(
    quantileAtSamplingLocation(rr, Lat = xy[2], Lon = xy[1]),
    quantileAtSamplingLocation(r,  Lat = xy[2], Lon = xy[1]),
    tolerance = 1e-8
  )
  expect_equal(
    cumsumAtSamplingLocation(rr, Lat = xy[2], Lon = xy[1]),
    cumsumAtSamplingLocation(r,  Lat = xy[2], Lon = xy[1]),
    tolerance = 1e-6
  )
})
