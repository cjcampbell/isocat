# Tests for makeAssignmentSurface() and its internals (.predictSurface, .overlapProb).
# The point path needs no brms, so most tests run everywhere on a hand-built transfer
# (example_transfer()). The posterior path is gated on brms + a Stan backend.

test_that("point-overlap surface: one layer per ID, values in [0,1], and unnormalized", {
  tf     <- example_transfer()
  iso    <- example_isoscape()
  iso_sd <- example_isoscape_sd()
  s <- makeAssignmentSurface(c(A = -120, B = -80), iso, iso_sd, tf,
                             method = "point", metric = "overlap", tissue_SD = 3)
  expect_s4_class(s, "SpatRaster")
  expect_equal(terra::nlyr(s), 2L)
  expect_equal(names(s), c("A", "B"))
  mm <- terra::minmax(s)
  expect_true(all(mm[1, ] >= 0) && all(mm[2, ] <= 1))
  # Overlap is an absolute consistency probability -- it does NOT sum to 1.
  sums <- as.numeric(terra::global(s, "sum", na.rm = TRUE)[, 1])
  expect_false(isTRUE(all.equal(sums, c(1, 1))))
})

test_that("density metric normalizes to 1 (overlap does not)", {
  tf     <- example_transfer()
  d <- makeAssignmentSurface(c(A = -120, B = -80), example_isoscape(), example_isoscape_sd(),
                             tf, method = "point", metric = "density")
  expect_equal(as.numeric(terra::global(d, "sum", na.rm = TRUE)[, 1]), c(1, 1), tolerance = 1e-8)
})

test_that("point x density (published propagation) reproduces isotopeAssignmentModel", {
  tf     <- example_transfer(intercept = 20, slope = 1.1, sigma = 6)
  iso    <- example_isoscape()
  iso_sd <- example_isoscape_sd()
  new <- makeAssignmentSurface(-100, iso, iso_sd, tf, ID = "A", method = "point",
                               metric = "density", iso_se_scaling = "none", normalize = TRUE)
  legacy <- isotopeAssignmentModel(ID = "A", isotopeValue = -100, SD_indv = 6,
                                   precip_raster = 1.1 * iso + 20, precip_SD_raster = iso_sd)
  expect_equal(terra::values(new)[, 1], terra::values(legacy)[, 1], tolerance = 1e-8)
})

test_that("iso_se_scaling = 'slope' gives a wider sd than 'none' (point path)", {
  tf <- example_transfer(slope = 1.5, sigma = 2)  # slope far from 1 makes the factor visible
  sl <- .predictSurface(tf, example_isoscape(), example_isoscape_sd(),
                        method = "point", iso_se_scaling = "slope")
  no <- .predictSurface(tf, example_isoscape(), example_isoscape_sd(),
                        method = "point", iso_se_scaling = "none")
  sdSl <- terra::values(sl[["sd"]])[, 1]; sdNo <- terra::values(no[["sd"]])[, 1]
  expect_true(all(sdSl >= sdNo, na.rm = TRUE))
  expect_gt(mean(sdSl, na.rm = TRUE), mean(sdNo, na.rm = TRUE))
})

test_that("tissue_SD_in_density widens the density (lower peak)", {
  tf     <- example_transfer()
  iso    <- example_isoscape()
  iso_sd <- example_isoscape_sd()
  withSD <- makeAssignmentSurface(-100, iso, iso_sd, tf, ID = "A", tissue_SD = 8,
                                  method = "point", metric = "density",
                                  tissue_SD_in_density = TRUE, normalize = FALSE)
  noSD   <- makeAssignmentSurface(-100, iso, iso_sd, tf, ID = "A", tissue_SD = 8,
                                  method = "point", metric = "density",
                                  tissue_SD_in_density = FALSE, normalize = FALSE)
  expect_lt(max(terra::values(withSD)[, 1], na.rm = TRUE),
            max(terra::values(noSD)[, 1], na.rm = TRUE))
})

test_that("NA cells in the isoscape are preserved, not zero-filled", {
  tf     <- example_transfer()
  iso    <- example_isoscape();    iso[seq_len(200)]    <- NA
  iso_sd <- example_isoscape_sd(); iso_sd[seq_len(200)] <- NA
  s <- makeAssignmentSurface(c(A = -120, B = -80), iso, iso_sd, tf,
                             method = "point", metric = "overlap", tissue_SD = 3)
  expect_true(all(is.na(terra::values(s)[seq_len(200), 1])))
  expect_gt(sum(!is.na(terra::values(s)[, 1])), 0)
})

test_that(".overlapProb is 1 for identical distributions and shrinks with separation", {
  expect_equal(.overlapProb(0, 1, 0, 1), 1)
  expect_lt(.overlapProb(0, 1, 5, 1), .overlapProb(0, 1, 1, 1))
})

test_that("savePath writes one .tif per ID", {
  tf <- example_transfer()
  d  <- tempfile("surf"); on.exit(unlink(d, recursive = TRUE), add = TRUE)
  makeAssignmentSurface(c(A = -120, B = -80), example_isoscape(), example_isoscape_sd(),
                        tf, method = "point", metric = "overlap", savePath = d)
  expect_true(all(file.exists(file.path(d, c("A.tif", "B.tif")))))
})

test_that("a non-isocat_transfer is rejected", {
  expect_error(
    makeAssignmentSurface(-100, example_isoscape(), example_isoscape_sd(), transfer = list()),
    "isocat_transfer"
  )
})

test_that("posterior path runs end-to-end and preserves NA (needs brms + backend)", {
  skip_on_cran()
  skip_if_not_installed("brms")
  has_backend <- isTRUE(tryCatch(
    requireNamespace("cmdstanr", quietly = TRUE) &&
      !is.null(cmdstanr::cmdstan_version(error_on_NA = FALSE)),
    error = function(e) FALSE))
  skip_if_not(has_backend, "no cmdstan backend available")

  set.seed(1); n <- 40
  known <- data.frame(iso = runif(n, -120, -40), iso_se = 3, fur_me = 3)
  known$fur <- 20 + 1.1 * known$iso + rnorm(n, 0, 5)
  tf <- fitTransferFunction(known, "fur", "iso", "fur_me", "iso_se",
                            chains = 2, iter = 800, warmup = 400, seed = 1,
                            backend = "cmdstanr", refresh = 0, silent = 2)

  iso    <- terra::aggregate(example_isoscape(), 8)      # aggregation introduces NA
  iso_sd <- terra::aggregate(example_isoscape_sd(), 8)
  naCells <- sum(is.na(terra::values(iso)[, 1]))

  s <- makeAssignmentSurface(c(A = -100, B = -70), iso, iso_sd, tf, tissue_SD = 3,
                             method = "posterior", metric = "overlap", ndraws = 300)
  expect_s4_class(s, "SpatRaster")
  expect_equal(terra::nlyr(s), 2L)
  expect_equal(sum(is.na(terra::values(s)[, 1])), naCells)  # NA mask preserved
  mm <- terra::minmax(s)
  expect_true(all(mm[1, ] >= 0) && all(mm[2, ] <= 1))
})
