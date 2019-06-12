#' Convert probability surface to probability-quantile surface
#'
#' Converts normalized probability surface (e.g. one layer output of
#' isotopeAssignmentModel function) to quantile surfaces.
#'
#' @param probabilitySurface Normalized probability surface RasterLayer
#' @param rename Character value to append to raster name (e.g. "_quantile"). Defaults to FALSE.
#'
#' @return Returns RasterLayer rescaled to quantile values.
#'
#' @aliases quantile_surface
#'
#' @seealso \code{\link{quantileAtSamplingLocation}}
#'
#'
#'
#' @examples
#' # Generate example probability surfaces.
#' myiso <- rasterFromXYZ(isoscape)
#' myiso_sd <- rasterFromXYZ(isoscape_sd)
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
#'          precip_SD_raster = myiso_sd,
#'          nClusters = FALSE
#'          )
#'
#' # Convert to quantile surfaces.
#' quantile_surface <-  raster::stack( lapply( unstack(assignmentModels), makeQuantileSurfaces) )
#' plot(quantile_surface)
#'
#' @export
makeQuantileSurfaces <- function(probabilitySurface, rename = FALSE){
  p <- probabilitySurface

  f <- stats::ecdf(stats::na.omit(probabilitySurface[]))

  quantile_surface <- p # create baseline surface.
  quantile_surface[] <- f(p[]) # redefine values.

  if(rename == FALSE){
    names(quantile_surface) <- names(p)
  } else {
    if(class(rename) != "character")
      stop("argument 'rename' should be of character class.")
    names(quantile_surface) <- paste0(names(p), rename)
  }

  return(quantile_surface)
}
