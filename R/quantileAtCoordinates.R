#' Quantile at coordinates function
#'
#' Function estimates percentile of each non-NA value within a RasterLayer using the empirical cumulative distribution function, and extracts value at location specified.
#' For more information, see help(ecdf).
#' @param indivraster A RasterLayer representing normalized probability of origin surface
#' @param Lat Integer latitude
#' @param Lon Integer longitude
#'
#' @export quantileAtSamplingLocation
#'

quantileAtSamplingLocation <- function(indivraster, Lat, Lon){

  if(!is.numeric(Lat) | !is.numeric(Lon))
    stop("'Lat' and 'Lon' must both be numeric values.")

  if(is.na(Lat) | is.na(Lon)) {return(NA)} else {

    indivcoords <- sp::SpatialPoints(cbind(Lon,Lat))
    f <- stats::ecdf(stats::na.omit(indivraster[]))
    l <- raster::extract(indivraster, indivcoords)
    quantileAtPoints <- f(l)
    return(quantileAtPoints)
  }
}
