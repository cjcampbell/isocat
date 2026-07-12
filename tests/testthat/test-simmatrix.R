# Tests for the Schoener's D similarity-matrix builders:
# surfaceSimilarityMatrix (terra) and the legacy simmatrixMaker (raster::stack).

test_that("surfaceSimilarityMatrix places every pairwise D at the correct cell (n >= 4)", {
  a <- example_assignment(ids = LETTERS[1:5],
                          values = seq(-120, -40, length.out = 5), sd_indv = 5)
  m <- surfaceSimilarityMatrix(a)

  expect_equal(dim(m), c(5L, 5L))
  expect_equal(unname(diag(m)), rep(1, 5))
  expect_true(isSymmetric(unname(m)))

  # Regression for the upper.tri/combn fill-order bug: each off-diagonal entry
  # must equal the direct pairwise schoenersD for that pair. Loose tolerance
  # because the vectorized path (base-R sum) and schoenersD (terra global sum)
  # accumulate ~ncell terms in a different order — bit-identical on some
  # platforms, ~1e-9 apart on others. A mis-placed cell would differ by O(0.01+).
  for (i in 1:4) for (j in (i + 1):5) {
    expect_equal(m[i, j], as.numeric(unlist(schoenersD(a[[i]], a[[j]]))),
                 tolerance = 1e-6, info = paste("cell", i, j))
  }
})

test_that("surfaceSimilarityMatrix rejects non-SpatRaster input", {
  expect_error(surfaceSimilarityMatrix(matrix(1:4, 2)))
})

test_that("legacy simmatrixMaker matches surfaceSimilarityMatrix on the same surfaces", {
  skip_if_not_installed("raster")
  a <- example_assignment(ids = LETTERS[1:5],
                          values = seq(-120, -40, length.out = 5), sd_indv = 5)
  sm <- simmatrixMaker(raster::stack(a))
  sd <- surfaceSimilarityMatrix(a)
  expect_equal(unname(sm[upper.tri(sm)]), unname(sd[upper.tri(sd)]), tolerance = 1e-6)
})

test_that("simmatrixMaker rejects non-RasterStack input", {
  skip_if_not_installed("raster")
  a <- example_assignment(ids = LETTERS[1:3], values = c(-100, -80, -50), sd_indv = 5)
  expect_error(simmatrixMaker(a))  # a SpatRaster is not a RasterStack
})

test_that("surfaceSimilarityMatrix handles surfaces with NA cells (no NA off-diagonal)", {
  # Before the na.rm fix, colSums()/sum() over any NA cell made every off-diagonal
  # entry NA on masked surfaces.
  m <- surfaceSimilarityMatrix(example_assignment_na())
  expect_false(any(is.na(m)))
  expect_equal(unname(diag(m)), rep(1, 3))
  expect_true(all(m >= 0 & m <= 1))
})

test_that("schoenersD handles NA-cell surfaces and agrees with surfaceSimilarityMatrix", {
  # Before the na.rm fix, global(rast, "sum") was NA and `if (NA != 1)` errored.
  am <- example_assignment_na()
  d  <- as.numeric(unlist(schoenersD(am[[1]], am[[2]])))
  expect_true(is.finite(d) && d >= 0 && d <= 1)
  expect_equal(d, surfaceSimilarityMatrix(am)["A", "B"], tolerance = 1e-6)
})

test_that("schoenersD coerces legacy Raster* input (raster -> Raster fix)", {
  skip_if_not_installed("raster")
  a <- example_assignment(ids = LETTERS[1:2], values = c(-100, -60), sd_indv = 5)
  expect_equal(
    as.numeric(unlist(schoenersD(raster::raster(a[[1]]), raster::raster(a[[2]])))),
    as.numeric(unlist(schoenersD(a[[1]], a[[2]]))),
    tolerance = 1e-6
  )
})

test_that("schoenersD returns a numeric scalar, not a data.frame", {
  # terra::global() yields a 1x1 data.frame; before the as.numeric() fix schoenersD
  # returned that data.frame (column "sum"), contradicting its documented scalar value
  # and letting guards like all(res$D > x) pass vacuously.
  a <- example_assignment(ids = c("A", "B"), values = c(-100, -80), sd_indv = 5)
  d <- schoenersD(a[[1]], a[[2]])
  expect_true(is.numeric(d))
  expect_length(d, 1)
  expect_false(is.data.frame(d))
  expect_equal(d, surfaceSimilarityMatrix(a)["A", "B"], tolerance = 1e-6, ignore_attr = TRUE)
  # The natural idiom keeps the value under the caller's column name.
  df <- data.frame(D = schoenersD(a[[1]], a[[2]]))
  expect_identical(names(df), "D")
  expect_true(is.numeric(df$D))
})
