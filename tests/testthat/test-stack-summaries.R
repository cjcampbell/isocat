# Tests for the stack-summary functions migrated off raster:
# meanAggregateClusterProbability, projectSummaryMaxSurface, getPrecisionPar.

test_that("meanAggregateClusterProbability returns one mean surface per cluster", {
  a <- example_assignment(ids = LETTERS[1:4],
                          values = c(-120, -115, -40, -35), sd_indv = 5)
  clusters <- c(1, 1, 2, 2)
  means <- meanAggregateClusterProbability(indivIDs = names(a),
                                           clusters = clusters, surfaces = a)
  expect_s4_class(means, "SpatRaster")
  expect_equal(terra::nlyr(means), 2L)
  # Cluster 1's mean equals the cell-wise mean of layers A and B.
  manual <- terra::mean(a[[c("A", "B")]], na.rm = TRUE)
  expect_equal(as.vector(terra::values(means[[1]])),
               as.vector(terra::values(manual)), tolerance = 1e-10)
})

test_that("meanAggregateClusterProbability handles non-default cluster labels (GitHub #5)", {
  a <- example_assignment(ids = LETTERS[1:4],
                          values = c(-120, -115, -40, -35), sd_indv = 5)
  # Letter labels, not 1..K: the old 1:length(unique(clusters)) loop broke here.
  clusters <- c("x", "x", "y", "y")
  means <- meanAggregateClusterProbability(names(a), clusters, a)
  expect_equal(terra::nlyr(means), 2L)
  expect_equal(names(means), c("cluster_x", "cluster_y"))
  # cluster "x" = mean of the first two individuals (A, B).
  manual <- terra::mean(a[[c("A", "B")]], na.rm = TRUE)
  expect_equal(as.vector(terra::values(means[["cluster_x"]])),
               as.vector(terra::values(manual)), tolerance = 1e-6)
})

test_that("meanAggregateClusterProbability errors clearly on indivIDs not matching surface names", {
  a <- example_assignment(ids = LETTERS[1:3], values = c(-100, -80, -50), sd_indv = 5)
  expect_error(
    meanAggregateClusterProbability(c("A", "B", "Z"), c(1, 1, 2), a),
    "must match layer names"
  )
})

test_that("projectSummaryMaxSurface returns a categorical index within the layer range", {
  a <- example_assignment(ids = LETTERS[1:4],
                          values = seq(-120, -25, length.out = 4), sd_indv = 5)
  summ <- projectSummaryMaxSurface(a)
  expect_s4_class(summ, "SpatRaster")
  expect_true(terra::is.factor(summ))
  vals <- terra::values(terra::as.int(summ))
  vals <- vals[!is.na(vals)]
  expect_true(all(vals >= 1 & vals <= terra::nlyr(a)))
  # Matches an independent which.max over the layer values (NA where all layers are NA).
  expected <- apply(terra::values(a), 1, function(r) {
    w <- which.max(r); if (length(w) == 0) NA_integer_ else w
  })
  expect_equal(as.integer(terra::values(terra::as.int(summ))), expected)
})

test_that("getPrecisionPar reports cell counts and proportions above thresholds", {
  a <- example_assignment(ids = LETTERS[1:3],
                          values = c(-100, -80, -50), sd_indv = 5)
  vals <- c(0.001, 0.005)
  df <- getPrecisionPar(a, checkVals = vals)

  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), terra::nlyr(a) * length(vals))
  expect_true(all(df$propAbove >= 0 & df$propAbove <= 1))
  # Higher threshold => no more cells above it than a lower threshold.
  for (id in unique(df$id)) {
    sub <- df[df$id == id, ]
    expect_true(sub$cellsAbove[sub$z == 0.005] <= sub$cellsAbove[sub$z == 0.001])
  }
  # Cross-check one layer's count against a direct computation.
  lv <- stats::na.omit(as.vector(terra::values(a[[1]])))
  expect_equal(df$cellsAbove[df$id == names(a)[1] & df$z == 0.001],
               sum(lv >= 0.001))
})

test_that("deprecated nClust/nCluster path messages and still returns serial result", {
  a <- example_assignment(ids = LETTERS[1:2], values = c(-100, -60), sd_indv = 5)
  expect_message(projectSummaryMaxSurface(a, nClust = 2), "depreciated")
  expect_message(
    meanAggregateClusterProbability(names(a), clusters = c(1, 2), surfaces = a, nClust = 2),
    "depreciated"
  )
  expect_message(getPrecisionPar(a, checkVals = 0.001, nCluster = 2), "depreciated")
})

test_that("stack summaries coerce legacy Raster* input", {
  skip_if_not_installed("raster")
  a <- example_assignment(ids = LETTERS[1:3],
                          values = c(-100, -80, -50), sd_indv = 5)
  rr <- raster::stack(a)  # SpatRaster -> RasterStack
  expect_s4_class(projectSummaryMaxSurface(rr), "SpatRaster")
  expect_equal(
    getPrecisionPar(rr, checkVals = 0.001)$cellsAbove,
    getPrecisionPar(a,  checkVals = 0.001)$cellsAbove
  )
})
