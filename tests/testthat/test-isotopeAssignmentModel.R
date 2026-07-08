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

test_that("legacy Raster additionalModels are coerced and combined", {
  skip_if_not_installed("raster")
  iso <- example_isoscape()
  iso_sd <- example_isoscape_sd()
  extra <- raster::raster(iso)  # legacy RasterLayer additionalModels

  res <- isotopeAssignmentModel(
    ID = "A", isotopeValue = -100, SD_indv = 5,
    precip_raster = iso, precip_SD_raster = iso_sd,
    additionalModels = extra
  )
  expect_s4_class(res, "SpatRaster")
  # combined surface is renormalized to sum to 1
  expect_equal(as.numeric(terra::global(res, "sum", na.rm = TRUE)[1, 1]), 1, tolerance = 1e-8)
})

test_that("savePath writes a readable .grd (regression for the raster format= arg)", {
  iso <- example_isoscape()
  iso_sd <- example_isoscape_sd()
  d <- tempfile("isosave"); dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)

  res <- isotopeAssignmentModel(
    ID = c("A", "B"), isotopeValue = c(-100, -80), SD_indv = c(5, 5),
    precip_raster = iso, precip_SD_raster = iso_sd, savePath = d
  )
  f <- file.path(d, "IsotopeAssignments.grd")
  expect_true(file.exists(f))
  expect_equal(terra::nlyr(terra::rast(f)), 2L)
})

test_that("a supplied SD_indv vector is respected even when its first element is 0", {
  iso <- example_isoscape()
  iso_sd <- example_isoscape_sd()
  ids <- c("A", "B", "C"); vals <- c(-100, -80, -50)
  a_vec  <- isotopeAssignmentModel(ID = ids, isotopeValue = vals,
              SD_indv = c(0, 5, 3), precip_raster = iso, precip_SD_raster = iso_sd)
  a_zero <- isotopeAssignmentModel(ID = ids, isotopeValue = vals,
              SD_indv = c(0, 0, 0), precip_raster = iso, precip_SD_raster = iso_sd)
  # Layers B and C carry SD 5 and 3, so they must differ from all-zero error;
  # the old guard silently overwrote c(0, 5, 3) with zeros.
  expect_gt(max(abs(terra::values(a_vec[[2]]) - terra::values(a_zero[[2]]))), 0)
  expect_gt(max(abs(terra::values(a_vec[[3]]) - terra::values(a_zero[[3]]))), 0)
  # Layer A carries SD 0 in both, so it is unchanged.
  expect_equal(terra::values(a_vec[[1]]), terra::values(a_zero[[1]]))
})

test_that("omitted (NULL) SD_indv matches an explicit all-zero vector", {
  iso <- example_isoscape()
  iso_sd <- example_isoscape_sd()
  a_null <- isotopeAssignmentModel(ID = c("A", "B"), isotopeValue = c(-100, -60),
              precip_raster = iso, precip_SD_raster = iso_sd)          # SD_indv omitted -> NULL
  a_zero <- isotopeAssignmentModel(ID = c("A", "B"), isotopeValue = c(-100, -60),
              SD_indv = c(0, 0), precip_raster = iso, precip_SD_raster = iso_sd)
  expect_equal(terra::values(a_null), terra::values(a_zero), tolerance = 1e-12)
})
