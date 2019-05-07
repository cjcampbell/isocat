#' Odds ratio at coordinates function
#'
#' Function estimates percentile of each non-NA value within a rasterLayer using the empirical cumulative distribution function, and extracts value at location specified. For more information, see help(ecdf).
#' @param indivraster rasterlayer representing normalized probability of origin surface
#' @param Lat Integer latitude
#' @param Lon Integer longitude
#'
#' @importFrom raster maxValue
#'
#' @export oddsAtSamplingLocation
#'

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

