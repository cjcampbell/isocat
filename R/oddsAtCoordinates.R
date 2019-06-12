#' Odds ratio at coordinates function
#'
#' Function estimates percentile of each non-NA value within a RasterLayer using the empirical cumulative distribution function, and extracts value at location specified. For more information, see help(ecdf).
#' @param indivraster RasterLayer representing normalized probability of origin surface
#' @param Lat Integer latitude
#' @param Lon Integer longitude
#'
#' @aliases odds_at_point
#'
#' @seealso \code{\link{makeOddsSurfaces}}
#'
#' @importFrom raster maxValue
#'
#' @examples
#' # Generate example probability surface.
#' myiso <- raster::rasterFromXYZ(isoscape)
#' myiso_sd <- rasterFromXYZ(isoscape_sd)
#' exampleSurface <- isotopeAssignmentModel(
#'          ID = "A",
#'          isotopeValue = -100,
#'          SD_indv = 5,
#'          precip_raster = myiso,
#'          precip_SD_raster = myiso_sd,
#'          nClusters = FALSE
#'          )
#' # Calculate odds ratio at specific point.
#' set.seed(1)
#' x <- sample( which( !is.na(exampleSurface[]) ), size = 1)
#' pt <- raster::xyFromCell(exampleSurface, x)
#' oddsAtSamplingLocation(exampleSurface, Lat = pt[2], Lon = pt[1])
#'
#' @export
oddsAtSamplingLocation <- function(indivraster, Lat, Lon){

  if(!is.numeric(Lat) | !is.numeric(Lon))
    stop("'Lat' and 'Lon' must both be numeric values.")

  if(is.na(Lat) | is.na(Lon)) {return(NA)} else {

    indivcoords <- sp::SpatialPoints(cbind(Lon,Lat))
    p_atPoint <- raster::extract(indivraster, indivcoords)

    odds_r_atPoint <- (p_atPoint/(1-p_atPoint))/(maxValue(indivraster)/(1-maxValue(indivraster)))

    return(odds_r_atPoint)
  }
}

