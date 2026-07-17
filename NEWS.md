# isocat 2.0.0

## New: Bayesian transfer functions and posterior-predictive assignment

This release adds a Bayesian workflow for building probability-of-origin surfaces, as new
functions alongside the classic `isotopeAssignmentModel()` path (which is unchanged).

* New `fitTransferFunction()` fits the tissue-to-isoscape transfer function as a Bayesian
  errors-in-variables regression -- measurement error in both the tissue and the isoscape
  values, via `brms`'s `mi()` and `me()` terms -- returning an `isocat_transfer` object.
  You name the columns of your known-origin data and it assembles the model formula; a
  `formula` escape hatch and a `prior` argument support custom models and informative priors.
* New `transferPrior()` builds informative priors (on the slope, intercept, residual sigma,
  and/or the latent isoscape mean) from familiar `c(mean, sd)` pairs, so a fit can be
  anchored on published estimates without knowing `brms`'s prior-class syntax -- especially
  useful when known-origin samples are few.
* New `makeAssignmentSurface()` builds per-individual probability-of-origin surfaces from an
  *untransformed* isoscape plus a fitted `isocat_transfer`, applying the transfer function
  internally. It offers a posterior-predictive `method` (propagating full transfer-function
  uncertainty) and a point-estimate `method`, crossed with an `overlap` (unnormalized
  consistency) or `density` (normalized Gaussian) `metric`. The per-cell predictive
  distribution is computed once and reused across individuals rather than recomputed per
  individual.
* New S3 class `isocat_transfer` with `print`, `coef` (intercept/slope/sigma point
  estimates), and `pp_check` (posterior-predictive check, via `brms`) methods.
* Exported internal helpers `.predictSurface`, `.overlapProb`, `.transferFormula`, and
  `.transferPoint` support the above, following the package's dot-prefixed helper convention.
* `brms` is added to `Suggests` (guarded at runtime): it is required only for the new
  Bayesian functions and is never loaded otherwise.

## New: summarizing origin environments from probability-of-origin surfaces

A small function family for post-processing a probability-of-origin surface into a
probability-weighted cloud of candidate origin points and per-individual summaries of the
environment (or distance travelled) at those origins.

* New `sampleOriginPoints()` draws points from each layer of a probability-of-origin
  `SpatRaster` with probability proportional to the surface, optionally recording each
  point's easting, northing, and distance from a supplied sampling location.
* New `extractEnvPoints()` attaches one or more environmental covariates to the points by
  extracting an `env` `SpatRaster` at each point's coordinates, retaining the full
  point-by-point distribution.
* New `summarizeDistribution()` reduces the retained points to per-individual, per-variable
  summaries (mean, SD, and quantiles), optionally under a half-normal movement prior.
* New `movementKernel()` provides the half-normal movement-prior weight used by the summary.
* New `plotOriginDistribution()` visualizes the per-individual distribution of a variable as
  horizontal violins (requires `ggplot2`, in `Suggests`).
* New example dataset `example_dem` (elevation over the `isoscape` grid, from NOAA's
  public-domain ETOPO 2022 global relief model) demonstrates extracting a secondary
  environmental surface at sampled points.
* The bundled example datasets (`isoscape`, `isoscape_sd`, `iso_monthly`,
  `iso_monthly_se`, `example_dem`) are now on a filled square 150x150 equal-area (Albers)
  grid in metres (previously lon/lat degrees), with the CRS carried in a `"crs"`
  attribute: rebuild with `rast(x, type = "xyz", crs = attr(x, "crs"))`.
* New vignette `origin-summaries` walks through the workflow on the bundled example data.

# isocat 1.0.1

## Bug fixes

* `isotopeAssignmentModel()`, `schoenersD()`, and `surfaceSimilarityMatrix()` now
  normalize correctly over isoscapes with masked (`NA`) cells, passing
  `na.rm = TRUE` in their internal sums; previously a single `NA` cell turned the
  whole surface `NA` (and errored in `schoenersD()`).
* `schoenersD()` returns a length-1 numeric value rather than a 1x1 `data.frame`,
  and coerces legacy `Raster*` inputs with the correct class check.


# isocat 1.0.0

## Breaking changes

* `isotopeAssignmentModel()` now uses `terra` instead of `raster`, and its
  in-function parallel-processing capability is deprecated.
* `schoenersD()` now expects `SpatRaster` inputs and automatically converts
  legacy `Raster*` objects.

## New features

* New `surfaceSimilarityMatrix()` builds a Schoener's D similarity matrix across
  all layers of a `SpatRaster`. It is vectorized (one value extraction plus
  base-R pairwise arithmetic), roughly 100x faster than per-pair evaluation. The
  legacy `simmatrixMaker()`, which takes a `RasterStack`, is retained but
  deprecated.

## Bug fixes

* `isotopeAssignmentModel()` no longer silently discards a supplied `SD_indv`
  vector whose first element is 0. The default is now `SD_indv = NULL` (no
  individual-level error; isoscape error only), replacing the ambiguous `0` that
  caused the bug.
* `makeMultiMonthIsoscape()` now uses `terra` and corrects two errors in the
  monthly combination: the precipitation-weighted mean previously divided by the
  number of months (returning half-magnitude values), and the equal-weight
  (`precip_stack = NULL`) path errored. The combined-error surface is unchanged
  (root-sum-of-square).
* `meanAggregateClusterProbability()` accepts any cluster labels (integers,
  letters, factors), not just `1..K`; the previous version indexed clusters by
  position and silently failed for other codings (#5). It also errors clearly
  when `indivIDs` do not match the surface layer names, and names each output
  layer by its cluster.

## Minor improvements

* Documentation and test coverage are expanded across all functions.

# isocat 0.3.0
## New function
* New function `makeMultiMonthIsoscape` to combine multiple monthly isoscapes and error maps into one of each, for making month-specific isoscape assignments.

## Tweaks
* `schoenersD` function now automatically normalizes surfaces to sum to 1, rather than just advising it in the documentation.
* corrected documentation parameter names in `.assignmentMaker`


# isocat 0.2.6

## Publication update
* Publication introducing some of isocat's functionalities is out. 
* Package citation is created, update. See `citation("isocat")`.
* Updated package description with links, typo fixes.


## Vignette improvements
* Added some additional text on transfer functions


# isocat 0.2.5

## Rewrote function `isotopeAssignmentModel`
* Function now checks for identical projections and resamples secondary models automatically.
* Multiple secondary models can be incorporated as a rasterStack.
* Replaced parallelization package with parallel::mcmapply.
* Removes bug where secondary models not created when running in parallel.

## Minor tweaks
* Remove now-unnecessary import of package `dplyr`.
* Resolved bug in vignette example by adding 'stringsAsFactors = FALSE' to example dataframe.
* Removed defunct class test for individual IDs from `meanAggregateClusterProbability` function.


## isocat 0.2.4

## Optimize 'cumsumbelow' function
* Function deals with NA values more efficiently.
* When imported into `makeCumSumSurface`, function iterates faster over RasterLayers.
* Removed dependency on `purrr::map` and pipe operators within function.
* The `purrr` package removed from suggested packages.

## Bug fix for R 4.0.0
* Modified conditional class check within `cumsumAtSamplingLocation` and `clusterSimmatrix` functions, in anticipation of change to matrix objects inheriting from class "array" (which broke old class check code).

## Vignette updated
* Vignette updated, examples improved, enable stage-skipping with working examples.

## Minor tweaks
* Tweaked package title.
* Added 'additionalModel_name' argument to `isotopeAssignmentModel`.
* Added 'seealso' and 'alias' components to function documentation.
* Fixed typo in examples for `makeQuantileSimulationSurface` function.
* Clarified function description for `schoenersD` function.
* Tweaked wording in example for `cumsumAtSamplingLocation` function.




# isocat 0.2.3

## Bug fixes
* Updated data file to realistic isoscape standard deviations.
* Fixed dependency / import issues throughout.
* Adjusted file names for cross-platform compatibility.
* Replaced references to superseded `snow` package with calls to `parallel`.

## Vignette updated
* Vignette updated, TODO's resolved.

## Minor Tweaks
* Expanded DESCRIPTION metadata.
* Resolved spelling errors and improved style throughout package.
* Fixed broken/suspect email and IsoMAP links.
* Corrected donttest wrappers on long analyses.
* Added examples to several functions.
* Switched from 'installed.packages' to '!requireNamespace' to check for package loading.


# isocat 0.2.2

## Vignette Included
* Added vignette with descriptions and example applications of all functions.

## New Functions
* New suite of functions for working with cumulative sums: `cumsumbelow`, `makeCumSumSurface`, `cumsumAtSamplingLocation`.
* Added a `meanAggregateClusterProbability` function to create mean probability-of-origin map based on cluster assignments (for individuals of similar origin).
* Added `projectSummarySurface` function to show which RasterLayer in a stack has highest value at any given location.

## Bug fixes
* Fixed minor issue with running `makeQuantileSimulationSurface` on object of class RasterLayer.
* Updated `clusterSimmatrix` function to match `pvclust` version 2.0-1 with debug of `r` parameter.
* Replaced buggy `projectCluster` function with alternative function (`projectSummarySurface`; see `isocat` vignette).


# isocat 0.2.1

## New Functions
* Added a `schoenersD` function that compares the spatial similarity of two surfaces.
* Added a `makeQuantileSimulationSurface` function that converts a surface to a quantile-simulation surface.

## Minor Tweaks
* Improved code readability within several functions.


# isocat 0.2.0

* Added a `NEWS.md` file to track changes to the package.
* Renamed package from `IsoModAT` (Isotope Models of Animal Origin Analytical Toolset) to `isocat` (Isotope Clustering and Assignment Tools). 
* Specified CC0 license.

## New Functions
* Added a `makeQuantileSurface` function to converts normalized probability surfaces to quantile surface. Matches `makeOddsSurface`.
* Added a `oddsAtSamplingLocation` function that finds odds ratios of origin probability at a set of coordinates within a normalized probability surface. Matches `quantileAtSamplingLocation`.
