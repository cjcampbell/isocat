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
  # must equal the direct pairwise schoenersD for that pair.
  for (i in 1:4) for (j in (i + 1):5) {
    expect_equal(m[i, j], as.numeric(unlist(schoenersD(a[[i]], a[[j]]))),
                 tolerance = 1e-10, info = paste("cell", i, j))
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
