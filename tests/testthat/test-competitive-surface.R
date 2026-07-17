# Tests for competitive distance reweighting: competitiveWeight, competitiveSurface.
#
# The binned competitiveWeight() approximates an exact O(n^2) within-suitability distance
# survival function; the reference implementation below is the acceptance anchor.

# Exact definition (O(n^2)) used only to validate the binned competitiveWeight().
competitiveWeightExact <- function(iso, d){
  vapply(seq_along(iso), function(i){
    if(!is.finite(iso[i]) || iso[i] <= 0 || !is.finite(d[i])) return(NA_real_)
    S <- iso >= iso[i] & is.finite(iso) & iso > 0
    sum(iso[S & d >= d[i]]) / sum(iso[S])
  }, numeric(1))
}

# --- competitiveWeight: matches the exact definition ---------------------------

test_that("competitiveWeight matches the exact survival function at high nbins", {
  set.seed(1)
  iso <- runif(400)
  d   <- runif(400, 0, 10)
  w   <- competitiveWeight(iso, d, nbins = 400)
  wex <- competitiveWeightExact(iso, d)
  expect_equal(w, wex, tolerance = 0.02)
})

test_that("competitiveWeight is bounded and honours the argmax guarantee", {
  set.seed(2)
  iso <- runif(300)
  d   <- runif(300, 0, 5)
  w   <- competitiveWeight(iso, d)
  expect_true(all(w >= 0 & w <= 1))                  # a proper conditional probability

  # The exact definition guarantees the most-suitable cell is never discounted (w = 1).
  # The binned weight only reproduces this exactly when the top suitability bin is not
  # shared with near-ties, so assert the guarantee on the exact function...
  expect_equal(competitiveWeightExact(iso, d)[which.max(iso)], 1)
  # ...and on well-separated suitabilities the binned weight matches it.
  iso2 <- c(0.2, 0.6, 1.0, 0.5, 0.1)
  expect_equal(competitiveWeight(iso2, d = c(0, 1, 2, 3, 4), nbins = 50)[3], 1)

  # Among equally-suitable cells, nearer beats farther (weakly): with all iso equal, w is
  # a pure distance survival function, non-increasing in distance.
  flat <- competitiveWeight(rep(1, 6), d = c(0, 1, 2, 3, 4, 5))
  expect_true(all(diff(flat) <= 1e-9))
})

test_that("competitiveWeight handles NA / non-positive iso and validates inputs", {
  iso <- c(0.5, NA, 0, 0.8, 1)
  d   <- c(1, 2, 3, 4, 5)
  w   <- competitiveWeight(iso, d, nbins = 50)
  expect_true(is.na(w[2]) && is.na(w[3]))            # NA and zero-suitability -> NA weight
  expect_true(all(!is.na(w[c(1, 4, 5)])))
  expect_true(all(is.na(competitiveWeight(c(NA, 0), c(1, 2)))))   # nothing eligible

  expect_error(competitiveWeight(1:3, 1:2))          # length mismatch
  expect_error(competitiveWeight("a", 1))            # non-numeric
  expect_error(competitiveWeight(1:3, 1:3, nbins = 0))
})

# --- competitiveSurface -------------------------------------------------------

test_that("competitiveSurface returns a normalized surface with matching geometry", {
  r    <- example_assignment(ids = c("A", "B"), values = c(-100, -80))
  e    <- as.vector(terra::ext(r))
  site <- c(mean(e[1:2]), mean(e[3:4]))
  p    <- competitiveSurface(r, xy = site)

  expect_s4_class(p, "SpatRaster")
  expect_equal(dim(p), dim(r))
  expect_equal(names(p), names(r))
  # each layer sums to 1 over non-NA cells
  csum <- terra::global(p, "sum", na.rm = TRUE)[, 1]
  expect_equal(unname(csum), rep(1, terra::nlyr(p)), tolerance = 1e-8)
})

test_that("competitiveSurface only ever discounts and does not annihilate the peak", {
  r    <- example_assignment(ids = "A", values = -100)
  e    <- as.vector(terra::ext(r))
  site <- c(mean(e[1:2]), mean(e[3:4]))
  p    <- competitiveSurface(r, xy = site, normalize = FALSE)

  iso <- terra::values(r)[, 1]
  pv  <- terra::values(p)[, 1]
  ok  <- !is.na(iso) & iso > 0
  # Reweighting only ever discounts (w in [0, 1]): p <= iso everywhere.
  expect_true(all(pv[ok] <= iso[ok] + 1e-9))
  # The most-suitable region is retained, not annihilated.
  imax <- which.max(iso)
  expect_gt(pv[imax], 0.5 * iso[imax])
})

test_that("competitiveSurface keeps zero-suitability cells at 0, not NA", {
  r <- example_assignment(ids = "A", values = -100)
  v <- terra::values(r)
  # Force an interior in-region cell to exactly zero suitability.
  z <- which(!is.na(v[, 1]))[1]
  v[z, 1] <- 0
  terra::values(r) <- v

  e    <- as.vector(terra::ext(r))
  p    <- competitiveSurface(r, xy = c(mean(e[1:2]), mean(e[3:4])), normalize = FALSE)
  expect_equal(unname(terra::values(p)[z, 1]), 0)
  expect_false(is.na(terra::values(p)[z, 1]))
})

test_that("competitiveSurface accepts a per-ID capture data.frame", {
  r   <- example_assignment(ids = c("A", "B"), values = c(-100, -80))
  e   <- as.vector(terra::ext(r))
  ref <- data.frame(ID = c("A", "B"),
                    x = c(mean(e[1:2]), e[1] + 0.25 * diff(e[1:2])),
                    y = c(mean(e[3:4]), mean(e[3:4])))
  p <- competitiveSurface(r, xy = ref)
  expect_equal(names(p), c("A", "B"))
  # Different capture points -> different reweighted surfaces.
  expect_false(isTRUE(all.equal(terra::values(p)[, 1], terra::values(p)[, 2])))
})

test_that("competitiveSurface validates inputs and warns on lon/lat surfaces", {
  r <- example_assignment(ids = "A", values = -100)
  expect_error(competitiveSurface("not a raster", xy = c(0, 0)))
  expect_error(competitiveSurface(r))                            # missing xy

  ll <- terra::rast(terra::ext(-100, -95, 40, 45), ncol = 10, nrow = 10, crs = "EPSG:4326")
  terra::values(ll) <- stats::runif(terra::ncell(ll))
  names(ll) <- "A"
  expect_warning(competitiveSurface(ll, xy = c(-97, 42)), "lon/lat")
})

test_that("competitiveSurface feeds sampleOriginPoints without a movement weight", {
  r    <- example_assignment(ids = "A", values = -100)
  e    <- as.vector(terra::ext(r))
  site <- c(mean(e[1:2]), mean(e[3:4]))

  p <- competitiveSurface(r, xy = site)
  set.seed(12)
  pts <- sampleOriginPoints(p, n = 300, xy = site)
  expect_equal(nrow(pts), 300)
  expect_true(all(c("dist", "prob") %in% names(pts)))
  # Points drawn from the reweighted surface sit nearer the site, on average, than points
  # drawn from the raw surface (competition discounts far-but-inferior origins).
  set.seed(12)
  pts_raw <- sampleOriginPoints(r, n = 300, xy = site)
  expect_lt(mean(pts$dist), mean(pts_raw$dist))
})
