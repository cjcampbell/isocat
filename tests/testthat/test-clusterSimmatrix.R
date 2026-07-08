# Tests for clusterSimmatrix (a pvclust wrapper). No raster/sp in the body.

sim_matrix <- function() {
  a <- example_assignment(ids = LETTERS[1:5],
                          values = seq(-120, -40, length.out = 5), sd_indv = 5)
  surfaceSimilarityMatrix(a)
}

test_that("clusterSimmatrix returns a pvclust object", {
  skip_if_not_installed("pvclust")
  res <- suppressWarnings(clusterSimmatrix(sim_matrix(), nBoot = 50))
  expect_s3_class(res, "pvclust")
  expect_s3_class(res$hclust, "hclust")
})

test_that("clusterSimmatrix passes its r argument through to pvclust (regression)", {
  skip_if_not_installed("pvclust")
  # Before the fix, r was hardcoded to seq(.7, 1.4, by = .1) (8 values, min 0.7) and
  # the argument ignored. pvclust snaps r to achievable bootstrap fractions, so we
  # check structure rather than exact values: the custom r has 3 values and reaches
  # below 0.7, neither of which is true of the ignored default.
  res <- suppressWarnings(clusterSimmatrix(sim_matrix(), nBoot = 50, r = c(0.5, 1.0, 1.5)))
  expect_length(res$r, 3)
  expect_lt(min(as.numeric(res$r)), 0.7)
})

test_that("clusterSimmatrix rejects non-matrix input", {
  skip_if_not_installed("pvclust")
  expect_error(clusterSimmatrix(1:10))
})
