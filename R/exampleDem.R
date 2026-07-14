#' Example digital elevation model (DEM)
#'
#' Elevation (in metres) over the same extent and grid as the example
#' \code{\link{isoscape}}. Provided to demonstrate extracting a secondary environmental
#' surface at sampled origin points with \code{\link{extractEnvPoints}}.
#'
#' @format A data.frame with 22500 rows and 3 columns (\code{x}, \code{y}, and
#'   \code{elevation} in metres), aligned cell-for-cell to the \code{isoscape} grid (a
#'   square 150 x 150 equal-area Albers grid; edge cells \code{NA}). Coordinates are in
#'   metres; the CRS is carried in a \code{"crs"} attribute. Build as a raster with
#'   \code{rast(example_dem, type = "xyz", crs = attr(example_dem, "crs"))}.
#'
#' @source ETOPO 2022 15 Arc-Second Global Relief Model (NOAA National Centers for
#'   Environmental Information), a public-domain (CC0-1.0) dataset, aggregated and
#'   resampled onto the isoscape grid. Reproduced by
#'   \code{setup_data/setup_dem_data.R}.
#'
#' @references NOAA National Centers for Environmental Information (2022) ETOPO 2022 15
#'   Arc-Second Global Relief Model. NOAA National Centers for Environmental Information.
#'   \doi{10.25921/fd45-gt74}
#'
#' @examples
#' dem <- rast(example_dem, type = "xyz", crs = attr(example_dem, "crs"))
#'
"example_dem"
