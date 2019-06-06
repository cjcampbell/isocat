
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
* Fixed broken/suspect email and IsoMAP links
* Corrected donttest wrappers on long analyses
* Added examples to several functions
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
