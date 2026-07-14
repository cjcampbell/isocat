# Tests for the origin-point summary family:
#   sampleOriginPoints, extractEnvPoints, movementKernel, .weightedQuantile,
#   summarizeDistribution.
#
# Anchored on the shared example assignment surfaces (helper-isocat.R). Points are
# drawn with probability proportional to the surface, so eligible cells are non-NA
# and > minValue, and the sampled `prob` column echoes the surface value.

# --- sampleOriginPoints -------------------------------------------------------

test_that("sampleOriginPoints returns n points per layer with valid cells", {
  r <- example_assignment(ids = c("A", "B"), values = c(-100, -80))
  set.seed(1)
  pts <- sampleOriginPoints(r, n = 300)

  expect_s3_class(pts, "data.frame")
  expect_setequal(unique(pts$ID), c("A", "B"))
  expect_equal(nrow(pts), 600)                       # n per layer
  expect_true(all(c("ID", "cell", "x", "y", "prob") %in% names(pts)))
  expect_true(all(pts$prob > 0) && !anyNA(pts$prob)) # only eligible cells sampled

  # `prob` equals the surface value at each sampled (ID, cell).
  vA <- terra::values(r)[, 1]
  a  <- pts[pts$ID == "A", ]
  expect_equal(a$prob, vA[a$cell])
})

test_that("sampleOriginPoints attaches distance to a reference location", {
  r <- example_assignment(ids = "A", values = -100)
  set.seed(2)
  pts <- sampleOriginPoints(r, n = 200, xy = c(-90, 45))

  expect_true(all(c("easting", "northing", "dist") %in% names(pts)))
  expect_equal(pts$dist, sqrt(pts$easting^2 + pts$northing^2))
  expect_equal(pts$easting, pts$x - (-90))
  expect_equal(pts$northing, pts$y - 45)
})

test_that("sampleOriginPoints accepts a per-ID reference data.frame", {
  r <- example_assignment(ids = c("A", "B"), values = c(-100, -80))
  ref <- data.frame(ID = c("A", "B"), x = c(-90, -85), y = c(45, 40))
  set.seed(3)
  pts <- sampleOriginPoints(r, n = 100, xy = ref)

  a <- pts[pts$ID == "A", ]
  expect_equal(a$easting, a$x - (-90))
  b <- pts[pts$ID == "B", ]
  expect_equal(b$northing, b$y - 40)
})

test_that("sampleOriginPoints validates inputs and reference locations", {
  r <- example_assignment(ids = "A", values = -100)
  expect_error(sampleOriginPoints(r, n = 0))
  expect_error(sampleOriginPoints("not a raster"))
  expect_error(sampleOriginPoints(r, n = 10, xy = data.frame(ID = "Z", x = 1, y = 1)))
  expect_error(sampleOriginPoints(r, n = 10, xy = c(1, 2, 3)))
})

# --- extractEnvPoints ---------------------------------------------------------

test_that("extractEnvPoints attaches env values at point coordinates", {
  r <- example_assignment(ids = "A", values = -100)
  set.seed(4)
  pts <- sampleOriginPoints(r, n = 200)

  env <- example_isoscape(); names(env) <- "d2H"
  out <- extractEnvPoints(pts, env)

  expect_true("d2H" %in% names(out))
  expect_equal(nrow(out), nrow(pts))
  # value at each point equals a direct terra::extract at that coordinate
  direct <- terra::extract(env, as.matrix(pts[, c("x", "y")]))[, 1]
  expect_equal(out$d2H, direct)
})

test_that("extractEnvPoints errors on name clash and non-raster env", {
  r <- example_assignment(ids = "A", values = -100)
  set.seed(5)
  pts <- sampleOriginPoints(r, n = 50)
  env <- example_isoscape(); names(env) <- "x"      # clashes with coordinate column
  expect_error(extractEnvPoints(pts, env))
  expect_error(extractEnvPoints(pts, "not a raster"))
})

# --- movementKernel -----------------------------------------------------------

test_that("movementKernel is half-normal and flat at L = Inf", {
  expect_equal(movementKernel(0, L = 150), 1)
  expect_equal(movementKernel(c(0, 100, 200), L = Inf), rep(1, 3))
  d <- c(0, 50, 100, 200)
  w <- movementKernel(d, L = 100)
  expect_true(all(diff(w) < 0))                       # strictly decreasing in distance
  expect_equal(w, exp(-0.5 * (d / 100)^2))
  expect_error(movementKernel(1:3, L = -1))
})

# --- .weightedQuantile --------------------------------------------------------

test_that(".weightedQuantile recovers unweighted quantiles under equal weights", {
  set.seed(6)
  x <- rnorm(2000)
  wq <- .weightedQuantile(x, w = rep(1, length(x)), probs = c(0.25, 0.5, 0.75))
  uq <- as.numeric(stats::quantile(x, c(0.25, 0.5, 0.75), type = 4))
  expect_equal(wq, uq, tolerance = 0.05)
  expect_true(all(is.na(.weightedQuantile(1:5, w = rep(0, 5), probs = 0.5))))
})

# --- summarizeDistribution ----------------------------------------------------

test_that("unweighted summary matches a plain mean over points", {
  r <- example_assignment(ids = c("A", "B"), values = c(-100, -80))
  set.seed(7)
  pts <- sampleOriginPoints(r, n = 400)
  env <- example_isoscape(); names(env) <- "d2H"
  pts <- extractEnvPoints(pts, env)

  s <- summarizeDistribution(pts, vars = "d2H")
  expect_true(all(c("ID", "variable", "n", "mean", "sd") %in% names(s)))
  expect_true(all(c("q2.5", "q50", "q97.5") %in% names(s)))

  a_mean <- mean(pts$d2H[pts$ID == "A"], na.rm = TRUE)
  expect_equal(s$mean[s$ID == "A"], a_mean)
  expect_equal(s$n[s$ID == "A"], sum(!is.na(pts$d2H[pts$ID == "A"])))
})

test_that("finite L applies movement weighting and requires a distance column", {
  r <- example_assignment(ids = "A", values = -100)
  set.seed(8)
  pts_nd <- sampleOriginPoints(r, n = 300)               # no reference -> no dist
  env <- example_isoscape(); names(env) <- "d2H"
  pts_nd <- extractEnvPoints(pts_nd, env)
  expect_error(summarizeDistribution(pts_nd, vars = "d2H", L = 5))

  pts <- sampleOriginPoints(r, n = 300, xy = c(-90, 45))
  pts <- extractEnvPoints(pts, env)
  unw <- summarizeDistribution(pts, vars = "d2H", L = Inf)$mean
  wtd <- summarizeDistribution(pts, vars = "d2H", L = 5)$mean
  expect_false(isTRUE(all.equal(unw, wtd)))             # weighting changes the mean
})

# --- legacy Raster* coercion --------------------------------------------------

test_that("legacy Raster* inputs are coerced", {
  skip_if_not_installed("raster")
  r <- example_assignment(ids = "A", values = -100)
  set.seed(9)
  pts <- sampleOriginPoints(raster::raster(r), n = 50)
  expect_equal(nrow(pts), 50)

  env <- example_isoscape(); names(env) <- "d2H"
  out <- extractEnvPoints(pts, raster::raster(env))
  expect_true("d2H" %in% names(out))
})
