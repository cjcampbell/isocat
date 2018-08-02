# isocat 0.2.0.9000

* Added a `NEWS.md` file to track changes to the package.
* Renamed package from 'IsoModAT' (Isotope Models of Animal Origin Analytical Toolset) to 'isocat' (Isotope Clustering and Assignment Tools). 
* Specified CC0 license.

## New Functions
* Added a `makeQuantileSurface` function to converts normalized probability surfaces to quantile surface. Matches `makeOddsSurface`.
* Added a `oddsAtSamplingLocation` function that finds odds ratios of origin probability at a set of coordinates within a normalized probability surface. Matches `quantileAtSamplingLocation`.
