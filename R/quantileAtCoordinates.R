#' Quantile at coordinates function
#'
#' Function estimates percentile of each non-NA value within a RasterLayer using the empirical cumulative distribution function, and extracts value at location specified.
#' For more information, see help(ecdf).
#' @param indivraster A SpatRaster representing normalized probability of origin surface
#' @param Lat Integer latitude
#' @param Lon Integer longitude
#'
#' @aliases quantile_at_point
#'
#' @seealso \code{\link{makeQuantileSurfaces}}
#'
#' @importFrom methods is
#'
#' @examples
#' # Generate example probability surface.
#' myiso <- rast(isoscape, type="xyz")
#' myiso_sd <- rast(isoscape_sd, type="xyz")
#' exampleSurface <- isotopeAssignmentModel(
#'          ID = "A",
#'          isotopeValue = -100,
#'          SD_indv = 5,
#'          precip_raster = myiso,
#'          precip_SD_raster = myiso_sd,
#'          nClusters = FALSE
#'          )
#' # Calculate quantile probability value at specific point.
#' set.seed(1)
#' x <- sample( which( !is.na(exampleSurface[]) ), size = 1)
#' pt <- terra::xyFromCell(exampleSurface, x)
#' quantileAtSamplingLocation(exampleSurface, Lat = pt[2], Lon = pt[1])
#'
#'
#' @export
quantileAtSamplingLocation <- function(indivraster, Lat, Lon){

  if(is(indivraster, "Raster")) indivraster <- terra::rast(indivraster)

  if(!is.numeric(Lat) | !is.numeric(Lon))
    stop("'Lat' and 'Lon' must both be numeric values.")

  if(is.na(Lat) | is.na(Lon)) {return(NA)} else {

    f <- stats::ecdf(stats::na.omit(indivraster[]))
    l <- terra::extract(indivraster, cbind(Lon, Lat))[1, 1]
    quantileAtPoints <- f(l)
    return(quantileAtPoints)
  }
}
