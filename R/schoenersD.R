#' Calculates Schoener's D-value between two RasterLayers.
#'
#' RasterLayers must have identical resolutions and extents.
#'
#' Applies pairwise comparisons of Schoener's D-metric between each RasterLayer in a RasterStack to populate a similarity matrix.
#' @param rast1 Input RasterLayer
#' @param rast2 Input RasterLayer 2
#'
#' @examples
#' # Generate example probability surfaces.
#' myiso <- raster::rasterFromXYZ(isoscape)
#' myiso_sd <- raster::rasterFromXYZ(isoscape_sd)
#' df <- data.frame(
#'         ID = c(-100, -80, -50),
#'         isotopeValue = c(-100, -80, -50),
#'         SD_indv = rep(5, 3)
#'         )
#' assignmentModels <- isotopeAssignmentModel(
#'         ID = df$ID,
#'         isotopeValue = df$isotopeValue,
#'         SD_indv = df$SD_indv,
#'         precip_raster = myiso,
#'         precip_SD_raster = myiso_sd,
#'         nClusters = FALSE
#'         )
#'
#' # Calculate Schoener's D-metric of spatial similarity between two of the
#' # example probability surfaces.
#' schoenersD(assignmentModels[[1]], assignmentModels[[2]])
#' ## 0.969156
#'
#' @export schoenersD



# Run pairwise comparisons.
schoenersD <- function(rast1, rast2){

  if(class(rast1) != "RasterLayer" | class(rast2) != "RasterLayer")
    stop("arguments are not of class 'RasterLayer'")

  1 - (0.5 * raster::cellStats(abs(rast1 - rast2), stat='sum'))

  }
