#' Calculates Schoener's D-value between two RasterLayers.
#'
#' RasterLayers must have identical resolutions and extents. The function will
#' automatically normalize surfaces to sum to 1.
#'
#' Calculates similarity value of two RasterLayers using Schoener's D-metric.
#' @param rast1 Input RasterLayer
#' @param rast2 Input RasterLayer 2
#'
#' @importFrom raster cellStats
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

  stopifnot(
    "Argument rast1 is not of class 'RasterLayer'" = class(rast1) == "RasterLayer" ,
    "Argument rast2 is not of class 'RasterLayer'" = class(rast2) == "RasterLayer"
  )

  if( cellStats(rast1, sum) != 1 ) { rast1 <- .findProductThenNormalize(rast1) }
  if( cellStats(rast2, sum) != 1 ) { rast2 <- .findProductThenNormalize(rast2) }

  1 - (0.5 * cellStats(abs(rast1 - rast2), stat='sum'))

  }
