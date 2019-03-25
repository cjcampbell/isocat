#'
#'
#' Converts normalized probability surface (e.g. one layer output of
#' isotopeAssignmentModel function) to quantile surfaces.
#'
#' @param probabilitySurface Normalized probability surface RasterLayer.
#' @param ValidationQuantiles Vector of quantile values from known-origin individuals, against which to compare each value within the probability surface. Each value must be between 0 and 1.
#' @param rename Character value to append to raster name (e.g. "_quantileSimulation"). Defaults to FALSE.
#' @param rescale If rescale = TRUE, returns surface showing proportion of times each surface cell value fell within the validation quantiles distribution. If rescale = FALSE, returns discrete number of times the cell fell within the distribution.
#'
#'
#' @return Returns rasterLayer rescaled to quantile values.
#'
#'
#' @examples
#' # Generate example probability surfaces.
#' data(isoscape)
#' library
#' myiso <- raster::rasterFromXYZ(isoscape)
#' myiso_sd <- raster::rasterFromXYZ(isoscape_sd)
#' df <- data.frame(ID = c(-100, -80, -50), dD = c(-100, -80, -50), SD_indv = rep(5, 3))
#' assignmentModels <- isotopeAssignmentModel(ID = df$ID, dD = df$dD, SD_indv = df$SD_indv, precip_raster = myiso, precip_SD_raster = myiso_sd, nClusters = FALSE)
#'
#' # Example known-origin quantile data.
#' set.seed(42)
#' x <- rgamma(10000, 1, 1)
#' myValidationQuantiles <- 1 - (x-min(x))/(max(x)-min(x)) # quantile values must be between 0 and 1.
#' hist(myValidationQuantiles)
#'
#' # Convert to quantile surfaces.
#' quantileSimulation_surface <-  stack( lapply( unstack(assignmentModels), makeQuantileSimulationSurface) )
#' plot(quantileSimulation_surface)
#'
#' @export makeQuantileSimulationSurface
#'

makeQuantileSimulationSurface <- function(probabilitySurface, ValidationQuantiles, rename = FALSE, rescale = TRUE){
  if(!is.logical(rescale)) stop("'rescale' must be a logical value.")
  if(class(probabilitySurface) != RasterLayer) stop("'probabilitySurface' must be of class RasterLayer")

  p <- probabilitySurface
  f <- ecdf(na.omit(probabilitySurface[]))
  quantile_surface <- p # create baseline surface.
  quantile_surface[] <- f(p[]) # redefine values.



  if(rename == FALSE){
    names(quantileSimulation_surface) <- names(p)
  } else {
    if(class(rename) != "character")
      stop("argument 'rename' should be of character class.")
    names(quantileSimulation_surface) <- paste0(names(p), rename)
  }

  if(rescale == TRUE)
    quantileSimulation_surface <- quantileSimulation_surface / length(ValidationQuantiles)



  return(quantileSimulation_surface)
}