#' Create quantile-simulation surfaces
#'
#' Converts normalized probability surfaces (e.g. one layer output of
#' isotopeAssignmentModel function) to quantile surfaces.
#'
#' @param probabilitySurface Normalized probability surface RasterLayer.
#' @param ValidationQuantiles Vector of quantile values from known-origin individuals, against which to compare each value within the probability surface. Each value must be between 0 and 1.
#' @param rename Character value to append to raster name (e.g. "_quantileSimulation"). Defaults to FALSE.
#' @param rescale If rescale = TRUE, returns surface showing proportion of times each surface cell value fell within the validation quantiles distribution. If rescale = FALSE, returns discrete number of times the cell fell within the distribution.
#'
#' @return Returns RasterLayer rescaled to quantile values.
#'
#'
#' @examples
#' # Generate example probability surfaces.
#' library(isocat)
#' myiso <- raster::rasterFromXYZ(isoscape)
#' myiso_sd <- raster::rasterFromXYZ(isoscape_sd)
#' df <- data.frame(
#'          ID = c(-100, -80, -50),
#'          isotopeValue = c(-100, -80, -50),
#'          SD_indv = rep(5, 3)
#'          )
#' assignmentModels <- isotopeAssignmentModel(
#'          ID = df$ID,
#'          isotopeValue = df$isotopeValue,
#'          SD_indv = df$SD_indv,
#'          precip_raster = myiso,
#'          precip_SD_raster = myiso_sd
#'          )
#'
#' # Example known-origin quantile data.
#' q <- rweibull(20000, 6, .98)
#' q <- sample( q[ q >=0 & q <= 1 ], 10000, replace = TRUE)
#' hist(q)
#'
#' # Convert to quantile surfaces.
#' quantileSimulation_surface <-  raster::stack(
#'                   lapply(
#'                             unstack(assignmentModels),
#'                             makeQuantileSimulationSurface,
#'                             ValidationQuantiles = q)
#'                         )
#' plot(quantileSimulation_surface)
#'
#' @export
makeQuantileSimulationSurface <- function(probabilitySurface, ValidationQuantiles, rename = FALSE, rescale = TRUE){
  if(!is.logical(rescale))
    stop("'rescale' must be a logical value.")
  if(class(probabilitySurface) != "RasterLayer")
    stop("'probabilitySurface' must be of class RasterLayer")

  p <- probabilitySurface
  f <- stats::ecdf(stats::na.omit(probabilitySurface[]))
  quantile_surface <- p # create baseline surface.
  quantile_surface[] <- f(p[]) # redefine values.

  check_above <- function(x){ sum( x >= ValidationQuantiles, na.rm = TRUE) }

  quantileSimulation_surface <- quantile_surface
  quantileSimulation_surface[] <- unlist(lapply(quantile_surface[], check_above) )

  names(quantileSimulation_surface) <- names(probabilitySurface) # make layer names match up.
  quantileSimulation_surface <- raster::mask(quantileSimulation_surface, probabilitySurface) # Make extents line up (mask vals that should be NA).


  if(rename == FALSE){
    names(quantileSimulation_surface) <- names(p)
  } else {
    if(class(rename) != "character")
      stop("argument 'rename' should be of character class.")
    names(quantileSimulation_surface) <- paste0(names(p), rename)
  }

  if(rescale == TRUE){
    quantileSimulation_surface <- quantileSimulation_surface / length(ValidationQuantiles)
  }

  return(quantileSimulation_surface)
}
