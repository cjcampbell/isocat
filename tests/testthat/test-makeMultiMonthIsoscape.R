# Tests for makeMultiMonthIsoscape, built on a controlled two-month example with
# hand-verifiable answers. Guards the two bugs the migration fixed:
#   - the weighted mean was off by a factor of n (calc mean vs sum)
#   - the NULL-precip branch crashed on eager `|` evaluation of nlayers(NULL)

mm_stacks <- function() {
  tmpl <- terra::rast(nrows = 4, ncols = 4, xmin = 0, xmax = 4, ymin = 0, ymax = 4)
  mk <- function(v) { r <- tmpl; terra::values(r) <- rep(v, terra::ncell(tmpl)); r }
  list(
    iso    = c(mk(10), mk(20)),   # two months of isoscape values
    iso_se = c(mk(2),  mk(4)),    # two months of SE
    precip = c(mk(1),  mk(3))     # precip amounts -> weights 1/4, 3/4
  )
}
cell1 <- function(r) as.numeric(terra::values(r)[1, 1])

test_that("precipitation-weighted mean isoscape is a weighted average (not a mean)", {
  s <- mm_stacks()
  out <- makeMultiMonthIsoscape(s$iso, s$iso_se, s$precip)
  expect_equal(cell1(out$mean_iso), 0.25 * 10 + 0.75 * 20)  # 17.5, not 8.75
})

test_that("combined error is the root-sum-of-square of the monthly SE layers", {
  s <- mm_stacks()
  out <- makeMultiMonthIsoscape(s$iso, s$iso_se, s$precip)
  expect_equal(cell1(out$iso_se), sqrt(2^2 + 4^2))  # sqrt(20)
})

test_that("NULL precip yields the equal-weight mean across months", {
  s <- mm_stacks()
  out <- makeMultiMonthIsoscape(s$iso, s$iso_se, NULL)  # previously crashed
  expect_equal(cell1(out$mean_iso), mean(c(10, 20)))    # 15
})

test_that("mismatched layer counts are rejected", {
  s <- mm_stacks()
  expect_error(
    makeMultiMonthIsoscape(s$iso, c(s$iso_se, s$iso_se[[1]]), s$precip)
  )
})

test_that("legacy Raster* stacks are coerced to the same result", {
  skip_if_not_installed("raster")
  s <- mm_stacks()
  out <- makeMultiMonthIsoscape(raster::stack(s$iso),
                                raster::stack(s$iso_se),
                                raster::stack(s$precip))
  expect_equal(cell1(out$mean_iso), 17.5)
})
