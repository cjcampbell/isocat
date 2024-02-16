#' Convert probability surface to odds-ratio surface
#'
#' Converts normalized probability surface (e.g. one layer output of isotopeAssignmentModel function) to Odds Ratio surfaces.
#'
#' @param probabilitySurface Normalized probability surface RasterLayer
#' @param rename Character value to append to raster name (e.g. "_odds"). Defaults to FALSE.
#'
#' @return Returns RasterLayer rescaled to Odds Ratio values.
#'
#' @importFrom magrittr %>%
#' @importFrom methods is
#'
#' @aliases odds_surface odds_surface
#'
#' @seealso \code{\link{oddsAtSamplingLocation}}
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
#' # Convert to odds ratio surfaces.
#' odds_ratio_surface <- stack(
#'    lapply( unstack(assignmentModels), makeOddsSurfaces )
#'    )
#' plot(odds_ratio_surface)
#'
#' @export
makeOddsSurfaces <- function(probabilitySurface, rename = FALSE){
  p <- probabilitySurface
  odds_r <- (p/(1-p))/(raster::maxValue(p)/(1-raster::maxValue(p)))

  if(rename == FALSE){
    names(odds_r) <- names(p)
  } else {
    if(!is(rename, "character") )
      stop("argument 'rename' should be of character class.")
    names(odds_r) <- paste0(names(p), rename)
    }

  return(odds_r)
}
