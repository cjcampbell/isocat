# Tests for fitTransferFunction(), transferPrior(), and the isocat_transfer S3 methods.
# Formula-builder and S3 tests need no model fit; transferPrior needs brms (no backend);
# the end-to-end fit is gated on brms + a Stan backend.

fstr <- function(...) paste(deparse(stats::formula(.transferFormula(...))), collapse = " ")

test_that(".transferFormula assembles the right formula for each argument combination", {
  skip_if_not_installed("brms")
  expect_equal(fstr("fur", "iso", "fur_me", "iso_se"),
               "fur | mi(fur_me) ~ me(iso, iso_se, gr = NULL)")
  expect_equal(fstr("fur", "iso", "fur_me", NULL),
               "fur | mi(fur_me) ~ iso")
  expect_equal(fstr("fur", "iso", NULL, "iso_se"),
               "fur ~ me(iso, iso_se, gr = NULL)")
  expect_equal(fstr("fur", "iso"),
               "fur ~ iso")
  expect_equal(fstr("fur", "iso", "fur_me", "iso_se", "myweights"),
               "fur | mi(fur_me) + weights(myweights) ~ me(iso, iso_se, gr = NULL)")
})

test_that("transferPrior builds a brmsprior with the requested classes", {
  skip_if_not_installed("brms")
  p <- transferPrior(slope = c(1.1, 0.05), intercept = c(-25, 10))
  expect_s3_class(p, "brmsprior")
  expect_equal(nrow(p), 2L)
  expect_true(all(c("b", "Intercept") %in% p$class))
  expect_error(transferPrior(), "at least one")
  expect_error(transferPrior(slope = 1.1), "length-2")
})

test_that("coef() and print() work on an isocat_transfer without a fit", {
  tf <- example_transfer()
  expect_equal(coef(tf), c(intercept = 20, slope = 1.1, sigma = 6))
  expect_output(print(tf), "isocat_transfer")
})

test_that("fitTransferFunction validates column names before fitting", {
  d <- data.frame(fur = 1:3, iso = 1:3)
  expect_error(fitTransferFunction(d, "fur", "NOPE"), "not found")
  expect_error(fitTransferFunction(d, "fur", c("iso", "iso")), "single column name")
})

test_that("fitTransferFunction fits and caches point estimates (needs brms + backend)", {
  skip_on_cran()
  skip_if_not_installed("brms")
  has_backend <- isTRUE(tryCatch(
    requireNamespace("cmdstanr", quietly = TRUE) &&
      !is.null(cmdstanr::cmdstan_version(error_on_NA = FALSE)),
    error = function(e) FALSE))
  skip_if_not(has_backend, "no cmdstan backend available")

  set.seed(1); n <- 40
  d <- data.frame(iso = runif(n, -120, -40), iso_se = 3, fur_me = 3)
  d$fur <- 20 + 1.1 * d$iso + rnorm(n, 0, 5)
  tf <- fitTransferFunction(d, "fur", "iso", "fur_me", "iso_se",
                            chains = 2, iter = 800, warmup = 400, seed = 1,
                            backend = "cmdstanr", refresh = 0, silent = 2)

  expect_s3_class(tf, "isocat_transfer")
  co <- coef(tf)
  expect_named(co, c("intercept", "slope", "sigma"))
  expect_true(all(is.finite(co)))
  expect_equal(tf$vars$iso, "iso")
  expect_lt(abs(co[["slope"]] - 1.1), 0.3)   # recovers the simulated slope
})
