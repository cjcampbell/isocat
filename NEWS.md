# isocat 0.2.2
## Vignette Included
* Basic vignette with example applications of all functions written.




# isocat 0.2.1

## New Functions
* Added a 'schoenersD' function that compares the spatial similarity of two surfaces.
* Added a 'makeQuantileSimulationSurface' function that converts a surface to a quantile-simulation surface.


## Minor Tweaks
* Improved code readibility within several functions.




# isocat 0.2.0

* Added a `NEWS.md` file to track changes to the package.
* Renamed package from 'IsoModAT' (Isotope Models of Animal Origin Analytical Toolset) to 'isocat' (Isotope Clustering and Assignment Tools). 
* Specified CC0 license.

## New Functions
* Added a `makeQuantileSurface` function to converts normalized probability surfaces to quantile surface. Matches `makeOddsSurface`.
* Added a `oddsAtSamplingLocation` function that finds odds ratios of origin probability at a set of coordinates within a normalized probability surface. Matches `quantileAtSamplingLocation`.
